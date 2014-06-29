{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental

Provides the core definitions of the public API for Widgets. The internal
structure of widgets, defined in the hidden module
'Reactive.Banana.Dzen.Widget', is /NOT/ exported here.

Therefore, other packages are free to build widgets by combining the basic ones
defined by this package, without the risk of breaking our invariants.

-}

module Reactive.Banana.Dzen.Widget
   ( Widget
   , label
   , string
   , wshow
   ) where


import Control.Applicative

import Reactive.Banana
import Reactive.Banana.Dzen.Internal.Widget


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
