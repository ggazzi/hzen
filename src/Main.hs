{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Main(main) where

import Control.Applicative
import Control.Concurrent.Suspend (Delay, msDelay, sDelay, hDelay, suspend)
import Control.Concurrent.Timer

import qualified Data.ByteString.Lazy.Char8 as B
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Reactive.Banana
import Reactive.Banana.Frameworks

main :: IO ()
main = do
  sources <- makeSources
  network <- compile $ setupNetwork sources
  actuate network
  eventLoop sources



type Sources = (EventSource (), CpuSource)


makeSources = (,) <$> newEventSource <*> newCpuSource

eventLoop :: Sources -> IO ()
eventLoop (esclock, scpu) = do
    autofireFunctions <- sequence [initAutofire esclock, initAutofire scpu]
    let autofire = sequence_ autofireFunctions
    repeatedTimer autofire (sDelay 1)
    loop
  where loop = do suspend (hDelay 24) >> loop



setupNetwork :: Frameworks t => Sources -> Moment t ()
setupNetwork (esclock, scpu) = do
  bcpu <- fromCpuSource scpu
  eclock <- fromEventSource esclock

  let boutput = pure "CPU: " <++> (show . round . (*100) <$> total bcpu) <++> pure "%"
      (<++>) = liftA2 (++)
  reactimate (putStrLn <$> boutput <@ eclock)
  


data CpuSource = CpuSource { stotal   :: AddHandler Float
                           , suser    :: AddHandler Float
                           , snice    :: AddHandler Float
                           , ssystem  :: AddHandler Float
                           , sidle    :: AddHandler Float
                           , sirq     :: AddHandler Float
                           , ssoftirq :: AddHandler Float
                           , fireCpuMon :: [Float] -> IO ()
                           }

data CpuMonitor t = CpuMonitor { total   :: Behavior t Float
                               , user    :: Behavior t Float
                               , nice    :: Behavior t Float
                               , system  :: Behavior t Float
                               , idle    :: Behavior t Float
                               , irq     :: Behavior t Float
                               , softirq :: Behavior t Float
                               }

instance Autofirable CpuSource where
  initAutofire scpu = do
    initial <- readCpuData
    cref <- newIORef initial
    return $ checkCpu cref >>= fireCpuMon scpu

newCpuSource :: IO CpuSource
newCpuSource = do
  total   <- newAddHandler
  user    <- newAddHandler
  nice    <- newAddHandler
  system  <- newAddHandler
  idle    <- newAddHandler
  irq     <- newAddHandler
  softirq <- newAddHandler
  return $ CpuSource { stotal   = addHandler total
                     , suser    = addHandler user
                     , snice    = addHandler nice
                     , ssystem  = addHandler system
                     , sidle    = addHandler idle
                     , sirq     = addHandler irq
                     , ssoftirq = addHandler softirq
                     , fireCpuMon = fireCpu
                                    [total, user, nice, system, idle, irq, softirq]
                     }

fromCpuSource :: Frameworks t => CpuSource -> Moment t (CpuMonitor t)
fromCpuSource src = CpuMonitor <$>
                      initBehavior (stotal   src) <*>
                      initBehavior (suser    src) <*>
                      initBehavior (snice    src) <*>
                      initBehavior (sidle    src) <*>
                      initBehavior (ssystem  src) <*>
                      initBehavior (sirq     src) <*>
                      initBehavior (ssoftirq src)
  where initBehavior = fmap (stepper 0) . fromAddHandler

fireCpu :: [EventSource Float] -> [Float] -> IO ()
fireCpu sources vals = sequence_ $ zipWith fire sources vals

checkCpu :: IORef [Float] -> IO [Float]
checkCpu cref = do
        prev <- readIORef cref
        curr <- readCpuData
        writeIORef cref curr
        let dif = zipWith (-) curr prev
            tot = foldr (+) 0 dif
            percent = map (/ tot) dif
            t = sum $ take 3 percent
        return (t:percent)

readCpuData :: IO [Float]
readCpuData = cpuParser <$> B.readFile "/proc/stat"

cpuParser :: B.ByteString -> [Float]
cpuParser = map (read . B.unpack) . tail . B.words . head . B.lines
  
  
{-----------------------------------------------------------------------------
    Event sources
------------------------------------------------------------------------------}
    
-- |Event Sources - allows you to register event handlers
type EventSource a = (AddHandler a, a -> IO ())

newEventSource :: IO (EventSource a)
newEventSource = newAddHandler

fromEventSource :: (Frameworks t) => EventSource a -> Moment t (Event t a)
fromEventSource = fromAddHandler . addHandler

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

class Autofirable es where
  initAutofire :: es -> IO (IO ())

instance Autofirable ((AddHandler (), () -> IO ())) where
  initAutofire es = return (fire es ())


class Source s where
  type GeneratedFrom s :: *
  fromSource :: s -> GeneratedFrom s
