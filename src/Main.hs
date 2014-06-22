module Main(main) where

import Reactive.Banana.Monitors

import Reactive.Banana.Monitors.Cpu (CpuMonitor)
import qualified Reactive.Banana.Monitors.Cpu as Cpu

import Data.Monoid

import Reactive.Banana
import Reactive.Banana.Dzen

main :: IO ()
main = do
  cpuSrc  <- Cpu.newMonitor
  monitors <- sequence [initMonitor cpuSrc]

  runDzen conf monitors $ do
    cpu <- fromMonitorSource cpuSrc
    return $ cpuWidget cpu

conf :: DzenConf
conf = defaultConf { dzenArgs = ["-xs", "2"] }

cpuWidget :: CpuMonitor t -> Widget t
cpuWidget cpu = let percent = round . (*100) <$> Cpu.busy cpu
                in label "CPU: " <> wshow percent <> label "%"
