{-# LANGUAGE TypeFamilies #-}
module Main(main) where

import Reactive.Banana.Monitors

import Reactive.Banana.Monitors.Cpu (CpuMonitor)
import qualified Reactive.Banana.Monitors.Cpu as Cpu


import Control.Concurrent.Suspend (suspend, sDelay, hDelay)
import Control.Concurrent.Timer

import Reactive.Banana
import Reactive.Banana.Frameworks


main :: IO ()
main = do
  tickSrc <- newTick
  cpuSrc  <- Cpu.newMonitor
  
  network <- compile $ setupNetwork tickSrc cpuSrc
  
  actuate network
  eventLoop tickSrc cpuSrc



eventLoop :: SourceOf Tick -> SourceOf CpuMonitor -> IO ()
eventLoop tick cpu = do
    monitors <- sequence [initMonitor cpu, initMonitor tick]
    repeatedTimer (updateMonitors monitors) (sDelay 1)
    loop
  where loop = do suspend (hDelay 24) >> loop



setupNetwork :: Frameworks t => SourceOf Tick -> SourceOf CpuMonitor -> Moment t ()
setupNetwork tickSrc cpuSrc = do
  cpu  <- fromMonitorSource cpuSrc
  etick <- tick <$> fromMonitorSource tickSrc

  let boutput = cpuFormat <$> Cpu.busy cpu
      cpuFormat v = concat ["CPU: ", show . round $ v*100, "%"]
  reactimate (putStrLn <$> boutput <@ etick)
  
