{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental

Provides combinators for changing the colors of widgets.

-}

module Reactive.Banana.Dzen.Color
  ( fg, bg, fgB, bgB
  , gradient, gradients
  ) where

import Control.Applicative
import Control.Monad.State

import Data.Colour
import Data.Colour.SRGB
import qualified Data.List as L

import Reactive.Banana
import Reactive.Banana.Dzen.Internal.Widget

-- | Sets the foreground color of the given widget.
fg :: Colour Double -> Widget t -> Widget t
fg = withColor "fg" . Just

-- | Sets the background color of the given widget.
bg :: Colour Double -> Widget t -> Widget t
bg = withColor "bg" . Just

-- | Sets the foreground color of the given widget, varying through time.
fgB :: Behavior t (Colour Double) -> Widget t -> Widget t
fgB = withColorB "fg" . fmap Just

-- | Sets the background color of the given widget, varying through time.
bgB :: Behavior t (Colour Double) -> Widget t -> Widget t
bgB = withColorB "bg" . fmap Just

-- Helper functions
withColor :: String -> Maybe (Colour Double) -> Widget t -> Widget t
withColor typ c = Widget . fmap (changeColorM typ (const c)) . unWidget

withColorB :: String -> Behavior t (Maybe (Colour Double)) -> Widget t -> Widget t
withColorB typ bc = Widget . (changeColorM typ <$> (const <$> bc) <*>) . unWidget

changeColorM :: String -> (Maybe (Colour Double) -> Maybe (Colour Double)) -> WidgetM () -> WidgetM ()
changeColorM typ f = \subWidget -> do
    prevColor <- gets fgColor
    let newColor = f prevColor
    modify (\s -> s {fgColor = newColor})
    setColor typ newColor
    subWidget
    setColor typ prevColor
    modify (\s -> s {fgColor = prevColor})

setColor :: (RealFrac a, Floating a) => String -> Maybe (Colour a) -> WidgetM ()
setColor s c = command s [maybe "" sRGB24show c]


-- | Produces a time-varying colour by blending the given colors.
--
-- A call of @gradient (x, y)@ assumes that @x < y@. If this isn't satisfied,
-- the behavior of this is undefined.
--
-- The result of @gradient (x, y) (c1, c2) someBehavior@ is such that,
-- when the value of @someBehavior@ is @x@ or less, the resulting color is @c1@,
-- and when the value is @y@, the result is @c2@
gradient :: (Ord a, Fractional a)
         => (a, a) -> (Colour a, Colour a)
         -> Behavior t a -> Behavior t (Colour a)
gradient (x0, x1) (c0, c1) bx
  | x0 < x1 = blend' (x0, x1) (c0, c1) <$> bx
  | x0 > x1 = blend' (x1, x0) (c1, c0) <$> bx
  | otherwise = pure c0

-- | Produces a time-varying colour, according to a time-varying @x@,
-- by blending two colours according to the value of @x@.
--
-- The first argument to this function is a list of "control points"
-- partitioning the possible values of @x@ into intervals. This list /must/ have
-- at least two items and the first component of each item should monotonically
-- increase (i.e. in @[(x0, c0), (x1, c1), ...]@, for every @i@ and @j=i+1@ we
-- should have @xi < xj@). If these assumptions are not satisfied, the behavior
-- of this is undefined.
--
-- The resulting color is then computed as follows:
--
--  * If the current @x@ is before the first control point @(x0, c0)@ (i.e.
--    @x < x0@), the resulting color is @c0@.
--
--  * If the current @x@ is after the last control point @(xn, cn)@ (i.e.
--    @x > xn@), the resulting color is @cn@.
--
--  * If the current @x@ is between the control points @(xi, ci)@ and
--    @(xj, cj)@, the resulting color is a blend of @ci@ and @cj@ obtained
--    by linear interpolation.
--
gradients :: (Ord a, Fractional a) => [(a, Colour a)] -> Behavior t a -> Behavior t (Colour a)
gradients points = fmap (gradients' points)

-- | Computes the color as described in 'gradients'
gradients' :: (Ord a, Fractional a) => [(a, Colour a)] -> a -> Colour a
gradients' [] = error "Cannot make a color blend without control points."
gradients' [(x0, c0), (x1, c1)] = blend' (x0, x1) (c0, c1)
gradients' points =
      -- Compute/cache the intervals and their blending functions
  let ((minX, minC), blenders, (maxX, maxC))
         = makeBlenders $ L.sortBy (\a b -> fst b `compare` fst a) points
      -- Actual function
      go x | x < minX = minC
           | x > maxX = maxC
           | otherwise = case head $ filter (inInterval x) blenders of
                          (_, _, blendWith) -> blendWith x
        where inInterval y (xi, xf, _) = xi <= y && y <= xf
      in go

-- | Given a list of control points, compute the intervals they define and the
-- blending functions for such intervals.
makeBlenders :: (Ord a, Fractional a) => [(a, Colour a)] -> ((a, Colour a), [(a, a, a -> Colour a)], (a, Colour a))
makeBlenders = start . L.sortBy (\a b -> fst b `compare` fst a)
  where start [] = error "Cannot compute intervals without at least one control point."
        start ((xf, cf):pairs) =
            let go acc (x1, c1) [] = ((x1, c1), acc, (xf, cf))
                go acc (x1, c1) ((x0, c0):rest) = go ((x0, x1, blend' (x0,x1) (c0,c1)) : acc) (x0, c0) rest
            in go [] (xf, cf) pairs

-- | Blend the given colours by assigning them to the extremities of the given
-- interval, then linearly interpolating. If the interpolated point lies outside
-- the interval, the result is undefined.
blend' :: Fractional a => (a, a) -> (Colour a, Colour a) -> a -> Colour a
blend' (xi, xf) (ci, cf) x = blend ((x-xi)/(xf-xi)) cf ci
