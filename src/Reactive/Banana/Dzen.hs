{-|
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental
-}

module Reactive.Banana.Dzen
   ( DzenConf(..), defaultConf
   , runDzen

   , Widget
   , label
   , string
   , wshow
   ) where

import Reactive.Banana.Dzen.Process
import Reactive.Banana.Dzen.Widget
