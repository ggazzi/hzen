{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental

A 'Monitor' for the current date and time.

This module is meant to be imported qualified, e.g.:

>  import qualified Reactive.Banana.Monitors.Time as Time

-}
{-# LANGUAGE TypeFamilies #-}
module Reactive.Banana.Monitors.Time
  ( TimeMonitor, getTime
  , newMonitor
  ) where

import Data.Time.LocalTime

import Reactive.Banana
import Reactive.Banana.Sources
import Reactive.Banana.Monitors.Class

-- | A 'Monitor' providing a single 'Behavior' with the current local time.
newtype TimeMonitor t = TM { getTime :: Behavior t ZonedTime }

newMonitor :: IO (SourceOf TimeMonitor)
newMonitor = fmap TS . newBehavior =<< getZonedTime

instance Monitor TimeMonitor where
    newtype SourceOf TimeMonitor = TS { unTS :: BehaviorSource ZonedTime }
    fromMonitorSource = fmap TM . fromBehaviorSource . unTS
    initMonitor (TS src) = return $ update src =<< getZonedTime
