{-# LANGUAGE TypeFamilies #-}
module Reactive.Banana.Monitors.Tick
       ( Tick
       , tick
       , newTick
       ) where

import Reactive.Banana
import Reactive.Banana.Sources
import Reactive.Banana.Monitors.Class

-- | Monitor with no content.
newtype Tick t = Tick { tick :: Event t () }

newTick :: IO (SourceOf Tick)
newTick = TS `fmap` newEvent

instance Monitor Tick where
  newtype SourceOf Tick = TS { unTS :: EventSource () }
  fromMonitorSource = fmap Tick . fromEventSource . unTS
  initMonitor (TS src) = return $ fire src ()
