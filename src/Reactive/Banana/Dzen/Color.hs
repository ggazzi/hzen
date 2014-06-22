module Reactive.Banana.Dzen.Color
  ( withFg, withBg
  , withDefaultFg, withDefaultBg
  , changeFg, changeBg
  ) where

import Control.Applicative
import Control.Monad.State

import Data.Colour
import Data.Colour.SRGB


import Reactive.Banana.Dzen.Widget


withFg, withBg :: Colour Double -> Widget t -> Widget t
withFg = withColor "fg" . Just
withBg = withColor "bg" . Just

withDefaultFg, withDefaultBg :: Widget t -> Widget t
withDefaultFg = withColor "fg" Nothing
withDefaultBg = withColor "bg" Nothing

withColor :: String -> Maybe (Colour Double) -> Widget t -> Widget t
withColor typ c = changeColor typ (const c)

changeFg, changeBg :: (Colour Double -> Colour Double) -> Widget t -> Widget t
changeFg f = changeColor "fg" (fmap f)
changeBg f = changeColor "bg" (fmap f)

changeColor :: String -> (Maybe (Colour Double) -> Maybe (Colour Double)) -> Widget t -> Widget t
changeColor typ f (Widget subWidget)= Widget $ wrap <$> subWidget
  where wrap subWidget = do
          prevColor <- gets fgColor
          let newColor = f prevColor
          modify (\s -> s {fgColor = newColor})
          setColor typ newColor
          subWidget
          setColor typ prevColor
          modify (\s -> s {fgColor = prevColor})

setColor :: (RealFrac a, Floating a) => String -> Maybe (Colour a) -> WidgetM ()
setColor s c = tellString $ "^" ++ s ++ "(" ++ maybe "" sRGB24show c ++ ")"
