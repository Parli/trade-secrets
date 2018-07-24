module Parli.Normalizer.Internal
( get
, post
) where

import RIO
import RIO.Text as T

import Control.Concurrent.QSem
import Data.Aeson
import Magicbane.HTTPClient hiding (query)
import Network.HTTP.Types

import Data.Time.Clock.POSIX
import Parli.Normalizer.Types.Environment

get :: (MonadNormalizer env m, FromJSON res)
  => Text -> [QueryItem] -> m (Maybe res)
get route query
  = fromJsonRes =<< get' runAuthenticated (makeRouteReq route) query

post :: (MonadNormalizer env m, ToJSON body, FromJSON res)
  => Text -> body -> m (Maybe res)
post route body
  = fromJsonRes =<< post' runAuthenticated (makeRouteReq route) body

get' :: MonadNormalizer env m => (Req m -> m Res) -> Req m -> [QueryItem] -> m Res
get' go req query = go $ setQueryString query <$> req

post' :: (MonadNormalizer env m, ToJSON body)
  => (Req m -> m Res) -> Req m -> body -> m Res
post' go req body = go $ postJson body =<< req

type Req m = ExceptT Text m Request
type Res = Response LByteString

makeRouteReq :: MonadNormalizer env m => Text -> Req m
makeRouteReq route = do
  server <- view $ normalizerEnvL . normalizerConnection . normalizerServer
  let req = reqS $ server <> route
  req <&> applyHeaders
    [ (hUserAgent, "mundane-clear-canister (Haskell, Magicbane/http-client)") ]

runAuthenticated :: MonadNormalizer env m => Req m -> m Res
runAuthenticated req = go False
  where
    go isRetry = do
      when isRetry resetGlobalToken
      bearer <- getBearerToken
      res <- runReq $ req
        <&> applyHeaders [ (hAuthorization, fromString $ T.unpack bearer) ]
      if not isRetry && statusCode (responseStatus res) == 401
        then go True
        else pure res

getBearerToken :: MonadNormalizer env m => m Text
getBearerToken = do
  globalToken <- view $ normalizerEnvL . normalizerAuthToken
  NormalizerAuthToken knownToken timestamp lifespan <- readIORef globalToken
  now <- truncate <$> liftIO getPOSIXTime
  token <- if now > timestamp + lifespan
    then fetchBearerToken now
    else pure knownToken
  pure $ "Bearer " <> token

fetchBearerToken :: MonadNormalizer env m => Int -> m Text
fetchBearerToken now = do
  env <- view normalizerEnvL
  res <- post' runReq (makeRouteReq "/login") $ object
    [ "email"    .= (env ^. normalizerConnection . normalizerUsername)
    , "password" .= (env ^. normalizerConnection . normalizerPassword)
    ]
  (TokenRes token expiry) <- fromJsonRes res
  let
    localToken = NormalizerAuthToken token expiry now
    globalToken = env ^. normalizerAuthToken
  atomicModifyIORef' globalToken $ const (localToken, ())
  pure token

data TokenRes = TokenRes Text Int
instance FromJSON TokenRes where
  parseJSON = withObject "tokenRes" $ \v -> TokenRes
    <$> v .:? "token"      .!= ""
    <*> v .:? "expires_in" .!= 0

resetGlobalToken :: MonadNormalizer env m => m ()
resetGlobalToken = do
  globalToken <- view $ normalizerEnvL . normalizerAuthToken
  atomicModifyIORef' globalToken $ const (NormalizerAuthToken "" 0 0, ())


fromJsonRes :: (MonadNormalizer env m, FromJSON a) => Res -> m a
fromJsonRes res = either error' pure . eitherDecode $ responseBody res
  where
    error' e = do
      logDebug . fromString . show $ res
      error e

runReq :: MonadNormalizer env m => Req m -> m Res
runReq req = throttled
  $ either (error . T.unpack) id
  <$> runHTTP (performWithBytes =<< req)

throttled :: MonadNormalizer env m => m a -> m a
throttled action = do
  q <- view $ normalizerEnvL . normalizerOutgoingQ
  bracket_ (liftIO $ waitQSem q) (liftIO $ signalQSem q) action
