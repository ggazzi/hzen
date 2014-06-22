module Reactive.Banana.Dzen.Widget
   ( Widget(..)

   , label
   , string
   , wshow
   ) where

import Reactive.Banana

-- | A time-varying description of what dzen should display.
newtype Widget t = Widget { unWidget :: Behavior t String }

-- | The monoidal operation on widgets is sequential composition:
-- the widgets are displayed one next to the other, on the given order.
instance Monoid (Widget t) where
  mempty = Widget $ pure ""
  Widget b1 `mappend` Widget b2 = Widget $ (++) <$> b1 <*> b2


-- | Displays an invariant string.
--
-- TODO Escape any dzen commands on the displayed string.
label :: String -> Widget t
label = Widget . pure

-- | Displays a time-varying string.
--
-- TODO Escape any dzen commands on the displayed string.
string :: Behavior t String -> Widget t
string = Widget

-- | Displays a time-varying value according to its 'Show' instance.
--
-- TODO Escape any dzen commands on the displayed string.
wshow :: Show a => Behavior t a -> Widget t
wshow = Widget . fmap show
