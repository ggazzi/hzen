{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fwarn-hi-shadowing #-}
module Reactive.Banana.Monitors.Cpu
       ( CpuMonitor
       , busy
       , user
       , nice
       , system
       , idle
       , iowait
       , irq
       , softirq

       , newMonitor
       ) where

import Reactive.Banana.Monitors.Class
import Reactive.Banana.Sources

import Control.Monad (zipWithM_)

import qualified Data.ByteString.Lazy.Char8 as B
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Reactive.Banana


-- | A monitor for the CPU activity.
--
-- Provides behaviors for the fraction of time that the
-- CPU is spending in several states.
data CpuMonitor t = CpuMonitor { busy    :: Behavior t Double
                               , user    :: Behavior t Double
                               , nice    :: Behavior t Double
                               , system  :: Behavior t Double
                               , idle    :: Behavior t Double
                               , iowait  :: Behavior t Double
                               , irq     :: Behavior t Double
                               , softirq :: Behavior t Double
                               }

newMonitor :: IO (SourceOf CpuMonitor)
newMonitor = do
  sources <- sequence . take 8 $ repeat (newBehavior 0)
  let [busy, user, nice, system, idle, iowait, irq, softirq] = sources
  return CpuSource
          { sbusy    = busy
          , suser    = user
          , snice    = nice
          , ssystem  = system
          , sidle    = idle
          , siowait  = iowait
          , sirq     = irq
          , ssoftirq = softirq
          , updateCpuMonitor = updateMany sources
          }

instance Monitor CpuMonitor where
  data SourceOf CpuMonitor = CpuSource
            { sbusy    :: BehaviorSource Double
            , suser    :: BehaviorSource Double
            , snice    :: BehaviorSource Double
            , ssystem  :: BehaviorSource Double
            , sidle    :: BehaviorSource Double
            , siowait  :: BehaviorSource Double
            , sirq     :: BehaviorSource Double
            , ssoftirq :: BehaviorSource Double
            , updateCpuMonitor :: [Double] -> IO ()
            }

  -- Just obtain all behaviors from behavior sources
  fromMonitorSource src = CpuMonitor <$>
                          fromBehaviorSource (sbusy    src) <*>
                          fromBehaviorSource (suser    src) <*>
                          fromBehaviorSource (snice    src) <*>
                          fromBehaviorSource (ssystem  src) <*>
                          fromBehaviorSource (sidle    src) <*>
                          fromBehaviorSource (siowait  src) <*>
                          fromBehaviorSource (sirq     src) <*>
                          fromBehaviorSource (ssoftirq src)

  initMonitor src = do
    -- Obtain an IORef with the current CPU time
    initial <- readCpuData
    cpuTimeRef <- newIORef initial
    -- Use the IORef to update (comparing the previous times
    -- with the current, for how much time was spent in each
    -- state)
    return $ checkCpu cpuTimeRef >>= updateCpuMonitor src

-- | Reads the current CPU state, updates it on the given
-- 'IORef' and returns the fraction of time spent on
-- each of the following states: busy (user + nice + system),
-- user, nice, system, idle, iowait, irq, softirq.
checkCpu :: IORef [Double] -> IO [Double]
checkCpu cref = do
        -- Obtain the current and previous times
        prev <- readIORef cref
        curr <- readCpuData
        -- Update the IORef
        writeIORef cref curr
            -- Calculate the time spent since last check
        let dif = zipWith (-) curr prev
            tot = sum dif
            -- Calculate the fractions for each state
            frac = map (/ tot) dif
            t = sum $ take 3 frac
        return (t:frac)

-- | Update the given behaviors with the corresponding values.
--
-- Given @m@ 'BehaviorSource's and @n@ values, update the first
-- @min m n@ behaviors with the corresponding values.
updateMany :: [BehaviorSource a] -> [a] -> IO ()
updateMany = zipWithM_ update

-- | Obtain the total CPU time spent on each state, since the system startup
readCpuData :: IO [Double]
readCpuData = cpuParser <$> B.readFile "/proc/stat"

-- | Given the contents of /proc/stat, obtain the values about
-- all CPUs as a list of Doubles. Those values correspond to the
-- CPU time spent since the system startup in the following
-- states, respectively: user, nice, system, idle, iowait, irq,
-- softirq.
cpuParser :: B.ByteString -> [Double]
cpuParser = map (read . B.unpack) . tail . B.words . head . B.lines
