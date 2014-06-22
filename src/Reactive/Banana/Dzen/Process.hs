{-# LANGUAGE Rank2Types #-}
module Reactive.Banana.Dzen.Process
   ( DzenConf(..), defaultConf
   , runDzen
   ) where


import Control.Concurrent.Suspend (Delay, suspend, msDelay)

import System.IO
import System.Process


import Reactive.Banana
import Reactive.Banana.Monitors
import Reactive.Banana.Frameworks

import qualified Reactive.Banana.Monitors.Tick as T


import Reactive.Banana.Dzen.Widget

-- | Configuration for the dzen process, including its executable
--   and the frequency with which it should be updated.
data DzenConf = DzenConf
  { dzenPath :: FilePath
  , dzenArgs :: [String]
  , updateFreq :: Delay }

-- | A default configuration
defaultConf :: DzenConf
defaultConf = DzenConf { dzenPath = "dzen2"
                       , dzenArgs = []
                       , updateFreq = msDelay 500 }

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
runDzen conf monitors mWidget = do
    -- Create a tick for updating the dzen bar
    tickSrc <- T.newTick
    updateTick <- initMonitor tickSrc

    -- Create the dzen process
    dzen <- createDzen conf

    -- Create a network that updates dzen with the widget whenever there's a tick
    network <- compile $ do
      widget <- mWidget
      tick <- fromMonitorSource tickSrc
      reactimate $ (\x -> hPutStrLn dzen x >> putStrLn x) <$> unWidget widget <@ T.tick tick

    -- Run the network, keep updating the monitors and wait forever
    actuate network
    let loop = do updateMonitors monitors
                  updateTick
                  suspend (updateFreq conf)
                  loop
    loop

-- | Creates a process for dzen and return its standard input 'Handle'.
createDzen :: DzenConf -> IO Handle
createDzen conf = do
    let dzen = proc (dzenPath conf) (dzenArgs conf)
    (Just handle, _, _, _) <- createProcess $ dzen { std_in = CreatePipe }
    hSetBuffering handle LineBuffering
    return handle
