{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental
-}
{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Reactive.Banana.Monitors

import Reactive.Banana.Monitors.Cpu (CpuMonitor)
import qualified Reactive.Banana.Monitors.Cpu as Cpu
import Reactive.Banana.Monitors.Mem (MemMonitor)
import qualified Reactive.Banana.Monitors.Mem as Mem
import Reactive.Banana.Monitors.Swap (SwapMonitor)
import qualified Reactive.Banana.Monitors.Swap as Swap
import Reactive.Banana.Monitors.Time (TimeMonitor)
import qualified Reactive.Banana.Monitors.Time as Time

import qualified Data.Colour.Names as C
import Data.Monoid
import System.Locale (defaultTimeLocale)

import Reactive.Banana
import Reactive.Banana.Dzen
import Reactive.Banana.Dzen.Color
import Reactive.Banana.Dzen.Graphics
import Reactive.Banana.Dzen.Bars (vbar)
import Reactive.Banana.Dzen.Time
import qualified Reactive.Banana.Dzen.Bars as B

main :: IO ()
main = do
  cpuSrc  <- Cpu.newMonitor
  memSrc  <- Mem.newMonitor
  swapSrc <- Swap.newMonitor
  timeSrc <- Time.newMonitor
  monitors <- sequence [initMonitor cpuSrc, initMonitor memSrc, initMonitor swapSrc, initMonitor timeSrc]

  debugDzen putStrLn conf monitors $ do
    cpu  <- fromMonitorSource cpuSrc
    mem  <- fromMonitorSource memSrc
    swap <- fromMonitorSource swapSrc
    time <- fromMonitorSource timeSrc
    return $ "CPU " <> costBar (Cpu.busy cpu) <> sep
          <> "Mem " <> costBar (Mem.usedRatio mem) <> spacer 3 <> costBar (Swap.usedRatio swap) <> sep
          <> ypos (-5) (icon "examples/bitmaps/battery.xbm")
          <> spacer 100
          <> formatTime defaultTimeLocale "%a %d. %b  %l:%M" time

conf :: DzenConf
conf = defaultConf { dzenArgs = ["-xs", "1", -- Display only on one monitor
                                 "-e", "button3=unhide"] } -- Workaround to avoid dzen being closed by right-clicks

costBar :: Behavior t Double -> Widget t
costBar cost = let color = gradients [(0.1, C.limegreen),
                                      (0.5, C.yellow),
                                      (0.9, C.red)]
                                     cost
               in vbar [B.Size (10,16), B.ColourB color] cost

sep :: Widget t
sep = separator (20,15)
