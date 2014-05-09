{-# LANGUAGE TypeFamilies #-}
module Reactive.Banana.Monitors.Class
       ( Monitor(..)
       , MonitorUpdater
       , updateMonitors
       ) where

import Reactive.Banana
import Reactive.Banana.Frameworks hiding (newEvent)


-- | A monitor of the system, providing one or more 'Signal's/'Behavior's.
--
-- Each instance of this class should group information about some part of
-- the system, which are always collected together.
class Monitor m where
  -- | Contains the necessary information in order for reactive-banana to
  -- create the necessary 'Signal's and 'Behavior's, and also the actions
  -- used for firing such 'Signal's and updating the 'Behavior's.
  data SourceOf m :: *

  -- | Given the source of a monitor, create it.
  fromMonitorSource :: (Frameworks t) => SourceOf m -> Moment t (m t)

  -- | Initialize any persistent state that the monitor requires, then
  -- return an action for updating the monitor.
  --
  -- If the monitor samples a value that is always available, the updater
  -- should obtain a sample.
  --
  -- If the monitor has to listen for changes to the value, a listener
  -- thread should be created upon initialization, and it should write
  -- to an 'IORef'. The updater should then sample this IORef.
  --
  -- (If reactive-banana supports concurrency, the listener thread can
  -- directly update the events/behaviours)
  initMonitor :: SourceOf m -> IO MonitorUpdater

-- | Action used for updating a monitor.
type MonitorUpdater = IO ()

-- | Updates several monitors.
updateMonitors :: [MonitorUpdater] -> IO ()
updateMonitors = sequence_
