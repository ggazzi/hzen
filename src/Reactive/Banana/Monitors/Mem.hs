{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental

A 'Monitor' for the memory usage.

This module is meant to be imported qualified, e.g.:

>  import qualified Reactive.Banana.Monitors.Mem as Mem

-}
{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Reactive.Banana.Monitors.Mem where

import Control.Monad

import Data.Map (Map)
import Data.Text (Text)

import qualified Data.Map as M

import Reactive.Banana
import Reactive.Banana.Sources
import Reactive.Banana.Monitors.Class
import qualified Reactive.Banana.Monitors.Internal.Procfs as Procfs


-- | A monitor for the memory usage.
--
-- Provides behaviors for the total available memory and
-- for the fraction of it that is currently free.
data MemMonitor t = MemMonitor { total   :: Behavior t Int
                               , free    :: Behavior t Int
                               , used    :: Behavior t Int
                               , freeRatio :: Behavior t Double
                               , usedRatio :: Behavior t Double
                               }

newMonitor :: IO (SourceOf MemMonitor)
newMonitor = do
    [total, free, used]    <- replicateM 3 $ newBehavior 0
    [freeRatio, usedRatio] <- replicateM 2 $ newBehavior 0
    return $ MemSource total free used freeRatio usedRatio

instance Monitor MemMonitor where
  data SourceOf MemMonitor = MemSource
          { stotal :: BehaviorSource Int
          , sfree  :: BehaviorSource Int
          , sused  :: BehaviorSource Int
          , sfreeRatio :: BehaviorSource Double
          , susedRatio :: BehaviorSource Double }

  -- Just obtain all behaviors from behavior sources
  fromMonitorSource src = MemMonitor <$>
                          fromBehaviorSource (stotal src) <*>
                          fromBehaviorSource (sfree  src) <*>
                          fromBehaviorSource (sused  src) <*>
                          fromBehaviorSource (sfreeRatio src) <*>
                          fromBehaviorSource (susedRatio src)

  initMonitor src = return $ checkMem >>= updateMemMonitor src

updateMemMonitor :: SourceOf MemMonitor -> Map Text Int -> IO ()
updateMemMonitor mon fields = do
  let [total, free, buffer, cache] = map (fields M.!) ["MemTotal", "MemFree", "Buffers", "Cached"]
      rest = free + buffer + cache
      used = total - M.findWithDefault rest "MemAvailable" fields

  update (stotal mon) total
  update (sfree  mon) free
  update (sused  mon) used
  update (sfreeRatio mon) $ fromIntegral free / fromIntegral total
  update (susedRatio mon) $ fromIntegral used / fromIntegral total

checkMem :: IO (Map Text Int)
checkMem = Procfs.readMeminfo ["MemTotal", "MemFree", "Buffers", "Cached", "MemAvailable"]
