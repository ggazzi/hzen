{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental

Provides utilities for reading files from Linux's procfs.

-}
module Reactive.Banana.Monitors.Internal.Procfs
  ( readMeminfo
  , readStatCpu
  ) where

import Prelude hiding (readFile)

import Control.Applicative

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Attoparsec.Text as P

-- | Obtain the total CPU time spent on each state, since the system startup, by
-- reading it from @/proc/stat@. Those values correspond to the CPU time spent
-- since the system startup in the following states, respectively: user, nice,
-- system, idle, iowait, irq, softirq.
readStatCpu :: IO [Double]
readStatCpu = cpuParser <$> T.readFile "/proc/stat"

-- | Given the contents of @/proc/stat@, obtain the aggregated values about all
-- CPUs as a list of 'Double's.
cpuParser :: Text -> [Double]
cpuParser = map (read . T.unpack) . tail . T.words . head . T.lines

-- | Obtains information about the given fields from the @/proc/meminfo@ file.
-- The resulting values on the map are measured in
readMeminfo :: [Text] -> IO (Map Text Int)
readMeminfo relevant =  M.map (`div` 1024)
                     .  parseMeminfo (S.fromList relevant)
                    <$> T.readFile "/proc/meminfo"

-- | Parses the text contained in the @/proc/meminfo@ file, keeping only the
-- information about the given set of expected fields.
parseMeminfo :: Set Text -> Text -> Map Text Int
parseMeminfo relevant input = case parseOnly (meminfoFile relevant) input of
  Left  msg -> error $ "Failed parsing /proc/meminfo: " ++ msg
  Right res -> res

-- | Parser for the whole @/proc/meminfo@ file.
meminfoFile :: Set Text -> Parser (Map Text Int)
meminfoFile relevant = scanFold (meminfoLine relevant) endOfInput M.empty

-- | Generic parser combinator for reading sequences that accumulate input.
scanFold :: (Alternative f, Monad f) => (a -> f a) -> f b -> a -> f a
scanFold toRead terminator = go
  where go acc = (terminator *> pure acc) <|> (toRead acc >>= go)

-- | Parser for a line of the @/proc/meminfo@ file.
--
-- The line should have the format
-- @FieldName: value [kB]@, where value is a number.
meminfoLine :: Set Text -> Map Text Int -> Parser (Map Text Int)
meminfoLine relevant acc = do
    label <- P.takeWhile notColon <* char ':'
    (if label `S.member` relevant
      then M.insert label <$> (skipSpace *> decimal)
      else pure id) <*> pure acc
     <* skipWhile (not . isEndOfLine)
     <* skipWhile isEndOfLine
  where notColon c = c /= ':' && not (isEndOfLine c)
