{-# LANGUAGE TypeFamilies, ScopedTypeVariables, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Main(main) where

import Reactive.Banana.Monitors

import Reactive.Banana.Monitors.Cpu (CpuMonitor)
import qualified Reactive.Banana.Monitors.Cpu as Cpu

import Reactive.Banana.Monitors.Tick (Tick)
import qualified Reactive.Banana.Monitors.Tick as T


import Control.Concurrent.Suspend (suspend, sDelay, hDelay)
import Control.Concurrent.Timer

import Data.Monoid

import Reactive.Banana
import Reactive.Banana.Frameworks


main :: IO ()
main = do
  tickSrc <- T.newTick
  cpuSrc  <- Cpu.newMonitor

  network <- compile $ setupNetwork tickSrc cpuSrc

  actuate network
  eventLoop tickSrc cpuSrc


eventLoop :: SourceOf Tick -> SourceOf CpuMonitor -> IO ()
eventLoop tick cpu = do
    monitors <- sequence [initMonitor cpu, initMonitor tick]
    repeatedTimer (updateMonitors monitors) (sDelay 1)
    loop
  where loop = suspend (hDelay 24) >> loop


setupNetwork :: Frameworks t
              => SourceOf Tick -> SourceOf CpuMonitor -> Moment t ()
setupNetwork tickSrc cpuSrc = do
  cpu  <- fromMonitorSource cpuSrc
  tick <- fromMonitorSource tickSrc

  reactimateBar tick (cpuWidget cpu)

reactimateBar :: Frameworks t => Tick t -> Widget t -> Moment t ()
reactimateBar tick widget =
  reactimate (putStrLn <$> unWidget widget <@ T.tick tick)

newtype Widget t = Widget { unWidget :: Behavior t String }

instance Monoid (Widget t) where
  mempty = Widget $ pure ""
  Widget b1 `mappend` Widget b2 = Widget $ (++) <$> b1 <*> b2

label :: String -> Widget t
label = Widget . pure

string :: Behavior t String -> Widget t
string = Widget

wshow :: Show a => Behavior t a -> Widget t
wshow = Widget . fmap show

cpuWidget :: CpuMonitor t -> Widget t
cpuWidget cpu = let percent = round . (*100) <$> Cpu.busy cpu
                in label "CPU: " <> wshow percent <> label "%"
