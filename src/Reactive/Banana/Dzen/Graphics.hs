{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental

Provides widgets for basic graphical elements.

-}

module Reactive.Banana.Dzen.Graphics
  (
  -- * Icons
   icon, iconB
  -- * Shapes
  , rect, rectB, rectO, rectOB
  , square, squareB, squareO, squareOB
  , circle, circleB, circleO, circleOB
  ) where

import Control.Applicative

import Reactive.Banana
import Reactive.Banana.Dzen.Internal.Widget


-- | Draw an icon that can be found on the given path
icon :: FilePath -> Widget t
icon = Widget . pure . command "i" . (:[])

iconB :: Behavior t FilePath -> Widget t
iconB = Widget . fmap (command "i" . (:[]))

-- | Draw a rectangle with the given size (width, height).
rect :: (Int, Int) -> Widget t
rect = Widget . pure . shape2 "r"

-- | Draw a rectangle with the given time-varying size (width, height).
rectB :: Behavior t (Int, Int) -> Widget t
rectB = Widget . fmap (shape2 "r")

-- | Draw the outline of a rectangle with the given size (width, height).
rectO :: (Int, Int) -> Widget t
rectO = Widget . pure . shape2 "ro"

rectOB :: Behavior t (Int, Int) -> Widget t
rectOB = Widget . fmap (shape2 "ro")

-- | Draw a square with the given size.
square :: Int -> Widget t
square x = rect (x,x)

-- | Draw a square with the given time-varying size.
squareB :: Behavior t Int -> Widget t
squareB = rectB . fmap (\x -> (x,x))

-- | Draw the outline of a square with the given size.
squareO :: Int -> Widget t
squareO x = rectO (x,x)

-- | Draw the outline of a square with the given time-varying size.
squareOB :: Behavior t Int -> Widget t
squareOB = rectOB . fmap (\x -> (x,x))

-- | Draw a circle with the given radius.
circle :: Int -> Widget t
circle = Widget . pure . shape1 "c"

-- | Draw a circle with the given time-varying radius.
circleB :: Behavior t Int -> Widget t
circleB = Widget . fmap (shape1 "c")

-- | Draw a circle with the given radius.
circleO :: Int -> Widget t
circleO = Widget . pure . shape1 "co"

-- | Draw a circle with the given time-varying radius.
circleOB :: Behavior t Int -> Widget t
circleOB = Widget . fmap (shape1 "co")

shape1 :: String -> Int -> WidgetM ()
shape1 s x = tellStrings ["^", s, "(", show x, ")"]

shape2 :: String -> (Int, Int) -> WidgetM ()
shape2 s (x,y) = tellStrings ["^", s, "(", show x, "x", show y, ")"]
