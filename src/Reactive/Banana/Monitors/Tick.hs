{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental

A 'Monitor' used for synchronization/timing.

This module is meant to be imported qualified, e.g.:

>  import qualified Reactive.Banana.Monitors.Tick as T

-}
{-# LANGUAGE TypeFamilies #-}

module Reactive.Banana.Monitors.Tick
       ( Tick
       , tick
       , newTick
       ) where

import Reactive.Banana
import Reactive.Banana.Sources
import Reactive.Banana.Monitors.Class

-- | A 'Monitor' providing a single 'Event' with no content, meat to be used for
-- synchronization/timing.
newtype Tick t = Tick { tick :: Event t () }

newTick :: IO (SourceOf Tick)
newTick = TS `fmap` newEvent

instance Monitor Tick where
  newtype SourceOf Tick = TS { unTS :: EventSource () }
  fromMonitorSource = fmap Tick . fromEventSource . unTS
  initMonitor (TS src) = return $ fire src ()
