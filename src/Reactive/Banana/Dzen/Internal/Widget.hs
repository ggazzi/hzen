{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Reactive.Banana.Dzen.Internal.Widget(
  -- * The Behavior
    Widget(..)
  , evalWidget

  -- * The Monad
  , WidgetM
  , DzenState(..)
  , evalWidgetM
  , tellString
) where


import Reactive.Banana.Dzen.Internal.Config (DzenConf)
import qualified Reactive.Banana.Dzen.Internal.Config as Conf

import Control.Applicative
import Control.Monad.State.Class
import Control.Monad.Writer
import Control.Monad.Trans.State

import Data.Colour

import Reactive.Banana


-- | A time-varying description of what dzen should display.
newtype Widget t = Widget { unWidget :: Behavior t (WidgetM ()) }

-- | The monoidal operation on widgets is sequential composition:
-- the widgets are displayed one next to the other, on the given order.
instance Monoid (Widget t) where
  mempty = Widget $ pure mempty
  Widget b1 `mappend` Widget b2 = Widget $ mappend <$> b1 <*> b2

-- | A description of what dzen should display.
--
-- In order to make widgets composable, their sequential description should
-- always restore the state (e.g. fg/bg colors) to what it was before they
-- changed. Therefore, they must know what the state was before their start,
-- which is why the description is a state monad.
--
-- For convenience, it is also a writer monad, so we need not explicitly
-- concatenate the strings. For efficiency, the writer monad represents the
-- output as a difference list (i.e. functions @'String' -> 'String'@ which
-- prepend the represented string to their argument).
newtype WidgetM a = WidgetM {
  unWidgetM :: StateT DzenState (Writer (String -> String)) a
} deriving ( Functor
           , Applicative
           , Monad
           , MonadState DzenState
           , MonadWriter (String -> String))

-- | Widget descriptions combine monoidally by combining the 'Writer' output.
instance Monoid w => Monoid (WidgetM w) where
  mempty = return mempty
  mappend = (>>)

data DzenState = DzSt
  { fgColor :: Maybe (Colour Double)
  , bgColor :: Maybe (Colour Double)
  }

-- | Append the given string to the output of the widget.
--
-- Helper function to automatically convert 'String's to difference lists.
tellString :: String -> WidgetM ()
tellString = tell . (++)

-- | Runs the widget monad, given the initial state, and obtains the sequential
-- textual description of what dzen should display.
evalWidgetM :: WidgetM a -> DzenState -> String
evalWidgetM (WidgetM m) st = execWriter (evalStateT m st) ""
{-# INLINE evalWidgetM #-}

-- | Given a widget, obtain a time-varying textual description of what dzen
-- should display under the given configuration.
evalWidget :: Widget t -> DzenConf -> Behavior t String
evalWidget (Widget bhv) conf = flip evalWidgetM (initialState conf) <$> bhv
{-# INLINE evalWidget #-}

-- | Obtain the initial state of the dzen bar, given its configuration.
initialState :: DzenConf -> DzenState
initialState conf = DzSt
  { fgColor = Conf.fgColor conf
  , bgColor = Conf.bgColor conf
  }
