{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental
-}

module Main(main) where

import Reactive.Banana.Monitors

import Reactive.Banana.Monitors.Cpu (CpuMonitor)
import qualified Reactive.Banana.Monitors.Cpu as Cpu

import qualified Data.Colour.Names as C
import Data.Monoid

import Reactive.Banana
import Reactive.Banana.Dzen
import Reactive.Banana.Dzen.Color
import Reactive.Banana.Dzen.Graphics

main :: IO ()
main = do
  cpuSrc  <- Cpu.newMonitor
  monitors <- sequence [initMonitor cpuSrc]

  debugDzen putStrLn conf monitors $ do
    cpu <- fromMonitorSource cpuSrc
    return $ cpuWidget cpu <> sep <> ypos (-5) (icon "examples/bitmaps/battery.xbm")

conf :: DzenConf
conf = defaultConf { dzenArgs = ["-xs", "1", -- Display only on one monitor
                                 "-e", "button3=unhide"] } -- Workaround to avoid dzen being closed by left-clicks

cpuWidget :: CpuMonitor t -> Widget t
cpuWidget cpu = let percent = round . (*100) <$> Cpu.busy cpu
                    color = gradients [(0.1, C.limegreen),
                                       (0.5, C.yellow),
                                       (0.9, C.red)]
                                      (Cpu.busy cpu)
                in label "CPU: " <> fgB color (wshow percent) <> label "%"

sep :: Widget t
sep = separator (20,15)
