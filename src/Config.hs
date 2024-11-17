module Config(infiniteFuture, DatabaseType) where

import Data.Time (UTCTime(..), Day(..))
import Data.Map (Map)

infiniteFuture :: UTCTime
infiniteFuture = UTCTime (ModifiedJulianDay 1000000000) 0

type DatabaseType = Map String [(String, String)]
