{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental

Provides widgets for bars to express percentages. Such
bars have a rectangular outline, of the foreground colour,
and a partly filled interior, of a possibly user-defined
colour. The size of the filled interior may vary depends
on a time-varying number and on the expected range for
such number.

TODO find a better, "eindeutiger" name than "bar".

-}
{-# LANGUAGE RecordWildCards #-}

module Reactive.Banana.Dzen.Bars
  ( hbar, vbar
  , BarOption(..)
  ) where

import Data.Colour
import qualified Data.List as L
import Data.Monoid
import Reactive.Banana


import Reactive.Banana.Dzen.Widget
import Reactive.Banana.Dzen.Color
import Reactive.Banana.Dzen.Graphics
import Reactive.Banana.Dzen.Unsafe.Position

-- | Options for setting the appearance and behavior of a bar.
data BarOption t =
    Size (Int, Int)
  -- ^ Width and height of the bar, in pixels. If this options is not defined,
  -- the behavior of the bar is undefined.
  | Colour (Colour Double)
  -- ^ Colour of the filled part of the bar. By default, the foreground colour.
  | ColourB (Behavior t (Colour Double))
  -- ^ Time-varying colour for the filled partof the bar. By default, the
  -- foreground colour.
  | Bounds (Double, Double)
  -- ^ Expected bounds for the time-varying value that is graphically
  -- represented by the bar. If the value is at most the lower bound, the bar is
  -- totally empty; if the value is at least the upper bound, the bar is
  -- completely filled; if the value is in the range, the size of the filled
  -- region is linearly interpolated. By default, @(0, 1)@.

-- | Draws a bar which is filled vertically according to a time-varying value,
-- i.e. each line is either entirely filled or entirely unfilled.
--
-- The appearance and behavior of the bar are defined by the given 'BarOption's,
-- and are undefined if no 'Size' option is provided.
vbar :: [BarOption t] -> Behavior t Double -> Widget t
vbar opts bx =
  let cfg@BarConfig{size=(w,h), ..} = mkConfig opts
      insideSize x = (w-2, round $ x * fromIntegral (h-2))
  in bar cfg (insideSize <$> normalized bx)

-- | Draws a bar which is filled horizontally according to a time-varying value,
-- i.e. each column is either entirely filled or entirely unfilled.
--
-- The appearance and behavior of the bar are defined by the given 'BarOption's,
-- and are undefined if no 'Size' option is provided.
hbar :: [BarOption t] -> Behavior t Double -> Widget t
hbar opts bx =
  let cfg@BarConfig{size=(w,h), ..} = mkConfig opts
      insideSize x = (round $ x * fromIntegral (w-2), h-2)
  in bar cfg (insideSize <$> normalized bx)


data BarConfig t = BarConfig {
    size :: (Int, Int)
  , colourize :: Widget t -> Widget t
  , normalized :: Behavior t Double -> Behavior t Double
}

mkConfig :: [BarOption t] -> BarConfig t
mkConfig = L.foldl' go defaultConf
  where defaultConf = BarConfig { size = (0,0)
                                , colourize = id
                                , normalized = fmap (min 1 . max 0) }
        go conf (Size s)     = conf{size = s}
        go conf (Colour  c)  = conf{colourize = fg  c}
        go conf (ColourB bc) = conf{colourize = fgB bc}
        go conf (Bounds bs)  = conf{normalized = fmap (normalize bs)}
        normalize (low, high) = min 1 . max 0 . (/(high-low)) . (\x->x-low)


bar :: BarConfig t -> Behavior t (Int, Int) -> Widget t
bar BarConfig{size=size@(_,h), ..} insideSize =
       xpos 1                                <> colourize inside
    <> xposB (negate . (+1) <$> insideWidth) <> ignoreBg outline
  where insideWidth  = fst <$> insideSize
        insideHeight = snd <$> insideSize
        outline = rectO size
        inside = yposB ( (h+1-) <$> insideHeight) (rectB insideSize)
