{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental

Provides combinators for changing the drawing position.

Dzen's drawing behavior for the title bar is as follows. At any time, there is a
current (x,y) position. Whenever something is drawn, the x position is advanced
by the width of the object; the y position remains constant.

While the combinators for the vertical position are compositional, those for the
horizontal position are not. Their used must therefore always be encapsulated in
consistent ways:

 * The widget should not draw over previously drawn widgets.

 * The next widget to be drawn should not draw over the widget.

-}

module Reactive.Banana.Dzen.Unsafe.Position
  ( xpos, xposB
  , ypos, yposB
  , ycenter
  ) where

import Control.Applicative
import Control.Monad.State

import Reactive.Banana
import Reactive.Banana.Dzen.Internal.Widget

-- | Change the current horizontal position by the given amount of pixels.
--
-- Positive values move to the right; negative values, to the left.
xpos :: Int -> Widget t
xpos = Widget . pure . changeX

-- | Change the current horizontal position by the given time-varying amount of
-- pixels.
--
-- Positive values move to the right; negative values, to the left.
xposB :: Behavior t Int -> Widget t
xposB = Widget . fmap changeX

changeX :: Int -> WidgetM ()
changeX x = command "p" [show x]

-- | Change the widget's vertical position by the given amount of pixels.
--
-- Positive values move down; negative values, up.
ypos :: Int -> Widget t -> Widget t
ypos y = Widget . fmap (changeY $ Just y) . unWidget

-- | Change the widget's vertical position by the given time-varying amount of
-- pixels.
--
-- Positive values move down; negative values, up.
yposB :: Behavior t Int -> Widget t -> Widget t
yposB by = Widget . (changeY <$> (Just <$> by) <*>) . unWidget

-- | Change the widget's vertical position to the center of the line.
ycenter :: Widget t -> Widget t
ycenter = Widget . fmap (changeY Nothing) . unWidget


changeY :: Maybe Int -> WidgetM () -> WidgetM ()
changeY newY = \subWidget -> do
    prevY <- gets yPos
    setY newY
    subWidget
    setY prevY
  where setY y = do modify (\s -> s {yPos=y})
                    command "p" $ maybe [] (\v -> [";", show v]) $ y
