{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental

A 'Monitor' for the swap usage.

This module is meant to be imported qualified, e.g.:

>  import qualified Reactive.Banana.Monitors.Swap as Swap

-}
{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Reactive.Banana.Monitors.Swap where

import Control.Monad

import Data.Map (Map)
import Data.Text (Text)

import qualified Data.Map as M

import Reactive.Banana
import Reactive.Banana.Sources
import Reactive.Banana.Monitors.Class
import qualified Reactive.Banana.Monitors.Internal.Procfs as Procfs


-- | A monitor for the swap usage.
--
-- Provides behaviors for the total available swap space and
-- for the fraction of it that is currently free.
data SwapMonitor t = SwapMonitor { total   :: Behavior t Int
                               , free    :: Behavior t Int
                               , used    :: Behavior t Int
                               , freeRatio :: Behavior t Double
                               , usedRatio :: Behavior t Double
                               }

newMonitor :: IO (SourceOf SwapMonitor)
newMonitor = do
    [total, free, used]    <- replicateM 3 $ newBehavior 0
    [freeRatio, usedRatio] <- replicateM 2 $ newBehavior 0
    return $ SwapSource total free used freeRatio usedRatio

instance Monitor SwapMonitor where
  data SourceOf SwapMonitor = SwapSource
          { stotal :: BehaviorSource Int
          , sfree  :: BehaviorSource Int
          , sused  :: BehaviorSource Int
          , sfreeRatio :: BehaviorSource Double
          , susedRatio :: BehaviorSource Double }

  -- Just obtain all behaviors from behavior sources
  fromMonitorSource src = SwapMonitor <$>
                          fromBehaviorSource (stotal src) <*>
                          fromBehaviorSource (sfree  src) <*>
                          fromBehaviorSource (sused  src) <*>
                          fromBehaviorSource (sfreeRatio src) <*>
                          fromBehaviorSource (susedRatio src)

  initMonitor src = return $ checkSwap >>= updateSwapMonitor src

updateSwapMonitor :: SourceOf SwapMonitor -> Map Text Int -> IO ()
updateSwapMonitor mon fields = do
  let [total, free] = map (fields M.!) ["SwapTotal", "SwapFree"]
      used = total - free

  update (stotal mon) total
  update (sfree  mon) free
  update (sused  mon) used
  update (sfreeRatio mon) $ fromIntegral free / fromIntegral total
  update (susedRatio mon) $ fromIntegral used / fromIntegral total

checkSwap :: IO (Map Text Int)
checkSwap = Procfs.readMeminfo ["SwapTotal", "SwapFree"]
