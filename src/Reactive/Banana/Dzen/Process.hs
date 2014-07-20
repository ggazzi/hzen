{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental
-}
{-# LANGUAGE Rank2Types #-}

module Reactive.Banana.Dzen.Process
   ( DzenConf(..), defaultConf
   , runDzen, debugDzen
   ) where


import Control.Concurrent.Timer (repeatedTimer, stopTimer)

import Data.Colour.SRGB

import System.Exit (exitWith)
import System.IO
import System.Process

import Reactive.Banana
import Reactive.Banana.Monitors
import Reactive.Banana.Frameworks

import qualified Reactive.Banana.Monitors.Tick as T

import Reactive.Banana.Dzen.Internal.Config
import Reactive.Banana.Dzen.Internal.Widget (Widget, evalWidget)


-- | Run the main loop for the dzen bar.
--
-- Spawn the dzen process according to the given configuration, then
-- periodically update the appropriate 'Monitor's, then update the given
-- 'Widget' and display it on the bar.
runDzen :: DzenConf                 -- ^ Configuration for the dzen process.
        -> [MonitorUpdater]         -- ^ 'Monitor's that should be updated.
        -> (forall t. Frameworks t
            => Moment t (Widget t)) -- ^ Description of what the bar should display.
        -> IO ()
runDzen = execDzen hPutStrLn

-- | Run the main loop for the dzen bar.
--
-- Spawn the dzen process according to the given configuration, then
-- periodically update the appropriate 'Monitor's, then update the given
-- 'Widget' and display it on the bar.
--
-- To help debugging the configuration, an extra action is taken with the
-- dzen string, whenever it is produced.
debugDzen :: (String -> IO ())      -- ^ Extra action to be taken with the dzen string.
        -> DzenConf                 -- ^ Configuration for the dzen process.
        -> [MonitorUpdater]         -- ^ 'Monitor's that should be updated.
        -> (forall t. Frameworks t
            => Moment t (Widget t)) -- ^ Description of what the bar should display.
        -> IO ()
debugDzen dbg = execDzen (\dzenIn txt -> hPutStrLn dzenIn txt >> dbg txt)

execDzen :: (Handle -> String -> IO ()) -- ^ Action for updating dzen, given its text.
         -> DzenConf                    -- ^ Configuration for the dzen process.
         -> [MonitorUpdater]            -- ^ 'Monitor's that should be updated.
         -> (forall t. Frameworks t
             => Moment t (Widget t))    -- ^ Description of what the bar should display.
         -> IO ()
{-# INLINE execDzen #-}
execDzen updateDzen = \conf monitors mWidget -> do
    -- Create a tick for updating the dzen bar
    tickSrc <- T.newTick
    updateTick <- initMonitor tickSrc

    -- Create the dzen process
    (dzenIn, dzenProc) <- createDzen conf

    -- Create a network that updates dzen with the widget whenever there's a tick
    network <- compile $ do
      widget <- mWidget
      tick <- fromMonitorSource tickSrc
      reactimate $ updateDzen dzenIn
                <$> evalWidget widget conf <@ T.tick tick

    -- Run the network, and spawn a monitor updater
    actuate network
    timer <- flip repeatedTimer (updateFreq conf) $ do
      updateMonitors monitors
      updateTick

    -- Wait for the dzen process to end, then terminate
    exitCode <- waitForProcess dzenProc
    stopTimer timer
    exitWith exitCode

-- | Creates a process for dzen and return its standard input 'Handle'.
createDzen :: DzenConf -> IO (Handle, ProcessHandle)
createDzen conf = do
    let args = makeArgs conf
        dzen = proc (dzenPath conf) args
    putStrLn . unwords . (dzenPath conf:) $ args
    (Just inHandle, _, _, pHandle) <- createProcess $ dzen { std_in = CreatePipe }
    hSetBuffering inHandle LineBuffering
    return (inHandle, pHandle)

makeArgs :: DzenConf -> [String]
makeArgs conf = dzenArgs conf ++
                makeArg (\c -> ["-fg", sRGB24show c]) (fgColor conf) ++
                makeArg (\c -> ["-bg", sRGB24show c]) (bgColor conf)

makeArg :: (a -> [String]) -> Maybe a -> [String]
makeArg = maybe []
