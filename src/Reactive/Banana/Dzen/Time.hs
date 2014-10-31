{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental

Provides widgets for displaying dates and times.
-}
module Reactive.Banana.Dzen.Time
  ( formatTime
  , FormatTime(..)
  , TimeLocale
  ) where

import           Data.Time.Format                     (FormatTime)
import qualified Data.Time.Format                     as T
import           System.Locale                        (TimeLocale)

import           Reactive.Banana.Dzen.Widget
import Reactive.Banana.Monitors.Time

formatTime :: TimeLocale -> String -> TimeMonitor t -> Widget t
formatTime loc fmt = string . fmap (T.formatTime loc fmt) . getTime
