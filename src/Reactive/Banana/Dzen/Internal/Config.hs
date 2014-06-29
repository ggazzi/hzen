{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental
-}

module Reactive.Banana.Dzen.Internal.Config where

import Data.Colour
import Control.Concurrent.Suspend (Delay, msDelay)


-- | Configuration for the dzen process, including its executable and the
-- frequency with which it should be updated.
--
-- Some of the command-line arguments passed to dzen are explicitly represented
-- in 'DzenConf'. Those are appended to the end of 'dzenArgs' when creating the
-- process.
--
-- NOTE: providing options both in explicit fields and in 'dzenArgs' leads to
-- /undefined behavior/, i.e. please don't do it.
data DzenConf = DzenConf
  { dzenPath :: FilePath
  , dzenArgs :: [String]
  , fgColor  :: Maybe (Colour Double)
  , bgColor  :: Maybe (Colour Double)
  , updateFreq :: Delay }

-- | A default or example configuration.
defaultConf :: DzenConf
defaultConf = DzenConf { dzenPath = "dzen2"
                       , dzenArgs = []
                       , updateFreq = msDelay 500
                       , fgColor = Nothing
                       , bgColor = Nothing }
