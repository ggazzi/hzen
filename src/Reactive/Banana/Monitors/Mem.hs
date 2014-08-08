{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental

A 'Monitor' for the memory usage.

This module is meant to be imported qualified, e.g.:

>  import qualified Reactive.Banana.Monitors.Mem as Mem

-}
{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Reactive.Banana.Monitors.Mem where

import Control.Monad

import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Attoparsec.Text as P

import Reactive.Banana
import Reactive.Banana.Sources
import Reactive.Banana.Monitors.Class


-- | A monitor for the memory usage.
--
-- Provides behaviors for the total available memory and
-- for the fraction of it that is currently free.
data MemMonitor t = MemMonitor { total   :: Behavior t Int
                               , free    :: Behavior t Int
                               , used    :: Behavior t Int
                               , freeRatio :: Behavior t Double
                               , usedRatio :: Behavior t Double
                               }

newMonitor :: IO (SourceOf MemMonitor)
newMonitor = do
    [total, free, used]    <- replicateM 3 $ newBehavior 0
    [freeRatio, usedRatio] <- replicateM 2 $ newBehavior 0
    return $ MemSource total free used freeRatio usedRatio

instance Monitor MemMonitor where
  data SourceOf MemMonitor = MemSource
          { stotal :: BehaviorSource Int
          , sfree  :: BehaviorSource Int
          , sused  :: BehaviorSource Int
          , sfreeRatio :: BehaviorSource Double
          , susedRatio :: BehaviorSource Double }

  -- Just obtain all behaviors from behavior sources
  fromMonitorSource src = MemMonitor <$>
                          fromBehaviorSource (stotal src) <*>
                          fromBehaviorSource (sfree  src) <*>
                          fromBehaviorSource (sused  src) <*>
                          fromBehaviorSource (sfreeRatio src) <*>
                          fromBehaviorSource (susedRatio src)

  initMonitor src = return $ checkMem >>= updateMemMonitor src

updateMemMonitor :: SourceOf MemMonitor -> Map Text Int -> IO ()
updateMemMonitor mon fields = do
  let [total, free, buffer, cache] = map (fields M.!) ["MemTotal", "MemFree", "Buffers", "Cached"]
      rest = free + buffer + cache
      used = total - M.findWithDefault rest "MemAvailable" fields

  update (stotal mon) total
  update (sfree  mon) free
  update (sused  mon) used
  update (sfreeRatio mon) $ fromIntegral free / fromIntegral total
  update (susedRatio mon) $ fromIntegral used / fromIntegral total

checkMem :: IO (Map Text Int)
checkMem = M.map (`div` 1024) . parseFile relevantFields <$> T.readFile "/proc/meminfo"
  where relevantFields = S.fromList ["MemTotal", "MemFree", "Buffers", "Cached", "MemAvailable"]

-- | Parses the text contained in the @/proc/meminfo@ file, keeping only the
-- information about the given set of expected fields.
parseFile :: Set Text -> Text -> Map Text Int
parseFile relevant input = case parseOnly (file relevant) input of
  Left  msg -> error $ "Failed parsing /proc/meminfo: " ++ msg
  Right res -> res

-- | Parser for the whole @/proc/meminfo@ file.
file :: Set Text -> Parser (Map Text Int)
file relevant = scan M.empty
  where scan acc =  (endOfInput *> pure acc)
                <|> (line relevant acc >>= scan)

-- | Parser for a line of the @/proc/meminfo@ file.
--
-- The line should have the format
-- @FieldName: value [kB]@, where value is a number.
line :: Set Text -> Map Text Int -> Parser (Map Text Int)
line relevant acc = do label <- P.takeWhile notColon
                       char ':'
                       result <- if label `S.member` relevant
                         then M.insert label <$> (skipSpace *> decimal) <*> pure acc
                         else pure acc
                       skipWhile (not . isEndOfLine) *> skipWhile isEndOfLine
                       return result
  where notColon c = c /= ':' && not (isEndOfLine c)
