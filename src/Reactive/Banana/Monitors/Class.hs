{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental

Defines the type class for 'Monitors'. For further documentation, see the module
'Reactive.Banana.Monitors'.

-}
{-# LANGUAGE TypeFamilies, Rank2Types #-}

module Reactive.Banana.Monitors.Class
       ( Monitor(..)
       , MonitorUpdater
       , updateMonitors
       ) where

import Reactive.Banana
import Reactive.Banana.Frameworks hiding (newEvent)

import Data.Maybe (catMaybes)

-- | A monitor of the system, providing one or more 'Event's and 'Behavior's.
--
-- Each instance of this class should group information about some part of the
-- system, which is always collected together.
class Monitor m where
  -- | Contains the necessary information in order for reactive-banana to create
  -- the necessary 'Event's and 'Behavior's, and also the actions used for
  -- firing such 'Event's and updating the 'Behavior's.
  data SourceOf m :: *

  -- | Given the source of a monitor, create it.
  fromMonitorSource :: (Frameworks t) => SourceOf m -> Moment t (m t)

  -- | Initialize any persistent state that the monitor requires, then  return
  -- an action for updating the monitor.
  --
  -- If the monitor samples a value that is always easily available, the updater
  -- should obtain such a sample. If sampling is expensive or blocking, a
  -- separate polling thread should be created by the initializer instead, and
  -- the updater should do nothing.
  --
  -- If changes to the value are asynchronously obtained (e.g. written to a
  -- pipe), a listener thread should be created upon initialization and the
  -- updater should do nothing.
  initMonitor :: SourceOf m -> IO MonitorUpdater

-- | Action used for updating a monitor.
type MonitorUpdater = IO ()

-- | Updates several monitors.
updateMonitors :: [MonitorUpdater] -> IO ()
updateMonitors = sequence_
{-# INLINE updateMonitors #-}
