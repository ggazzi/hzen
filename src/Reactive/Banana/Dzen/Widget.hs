{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reactive.Banana.Dzen.Widget
   ( Widget(..), runWidget
   , WidgetM(..), runWidgetM, tellString
   , DzenState(..)

   , label
   , string
   , wshow
   ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.Writer
import Control.Monad.Trans.State

import Data.Colour

import Reactive.Banana

-- | A time-varying description of what dzen should display.
newtype Widget t = Widget { unWidget :: Behavior t (WidgetM ()) }

-- | A description of what dzen should display.
newtype WidgetM a = WidgetM {
  unWidgetM :: StateT DzenState (Writer (String -> String)) a
} deriving ( Functor
           , Applicative
           , Monad
           , MonadState DzenState
           , MonadWriter (String -> String))

tellString :: String -> WidgetM ()
tellString = tell . (++)

runWidgetM :: WidgetM a -> DzenState -> String
runWidgetM (WidgetM msw) st = execWriter (evalStateT msw st) ""

runWidget :: Widget t -> Behavior t String
runWidget = fmap (flip runWidgetM initialState) . unWidget

data DzenState = DzSt
  { fgColor :: Maybe (Colour Double)
  , bgColor :: Maybe (Colour Double)
  }

initialState :: DzenState
initialState = DzSt
  { fgColor = Nothing
  , bgColor = Nothing
  }

-- | The monoidal operation on widgets is sequential composition:
-- the widgets are displayed one next to the other, on the given order.
instance Monoid (Widget t) where
  mempty = Widget $ pure mempty
  Widget b1 `mappend` Widget b2 = Widget $ mappend <$> b1 <*> b2

instance Monoid w => Monoid (WidgetM w) where
  mempty = return mempty
  mappend = liftM2 mappend



-- | Displays an invariant string.
--
-- Some characters may be escaped, so that this never
-- generates dzen commands.
label :: String -> Widget t
label = Widget . pure . tellString . escape

-- | Displays a time-varying string.
--
-- Some characters may be escaped, so that this never
-- generates dzen commands.
string :: Behavior t String -> Widget t
string = Widget . fmap (tellString . escape)

-- | Displays a time-varying value according to its 'Show' instance.
--
-- Some characters may be escaped, so that this never
-- generates dzen commands.
wshow :: Show a => Behavior t a -> Widget t
wshow = Widget . fmap (tellString . escape . show)

escape :: String -> String
escape "" = ""
escape ('^':cs) = "^^" ++ escape cs
escape (c:cs) = c : escape cs
