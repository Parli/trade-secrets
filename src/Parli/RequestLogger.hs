module Parli.RequestLogger where

import RIO

import Data.Default
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import System.IO.Unsafe (unsafePerformIO)

runReqLog :: RequestLoggerSettings -> Middleware
runReqLog = unsafePerformIO . mkRequestLogger

runReqLogIf :: Bool -> RequestLoggerSettings -> Middleware
runReqLogIf p l = if p then runReqLog l else id

apacheReqLog :: RequestLoggerSettings
apacheReqLog = def { outputFormat = Apache FromFallback }

detailedReqLog :: Bool -> RequestLoggerSettings
detailedReqLog c = def { outputFormat = Detailed c }
