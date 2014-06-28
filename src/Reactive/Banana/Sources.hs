{-|
Description: Simple abstractions to ease working with reactive-banana.
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental

A thin layer on top of reactive-banana to help creating 'Event's and
'Behavior's. Provides two new types: 'EventSource' (mostly a renaming
of 'AddHandler') and 'BehaviorSource' (simply an 'EventSource' whose
event gets wrapped in a 'stepper'). Also provides convenient functions
for creating the sources, obtaining their 'Event's or 'Behavior's and
firing/updating them.

-}
{-# LANGUAGE TupleSections #-}

module Reactive.Banana.Sources (
       -- * Event Sources
         EventSource
       , newEvent
       , fromEventSource
       , fire

       -- * Behavior Sources
       , BehaviorSource
       , newBehavior
       , fromBehaviorSource
       , update
       ) where

import Reactive.Banana
import Reactive.Banana.Frameworks hiding (newEvent)

-- | A source for a reactive-banana 'Event'. Contains the information
-- required by reactive-banana to create the corresponding 'Event',
-- and also an action used to fire it.
newtype EventSource a = ES { unES :: (AddHandler a, a -> IO ()) }

-- | Create the source for a new event.
newEvent :: IO (EventSource a)
newEvent = ES `fmap` newAddHandler
{-# INLINE newEvent #-}

-- | Obtain the event that corresponds to the given source.
fromEventSource :: (Frameworks t) => EventSource a -> Moment t (Event t a)
fromEventSource = fromAddHandler . fst . unES
{-# INLINE fromEventSource #-}

-- | Fire the event that corresponds to the given source, with the given value.
fire :: EventSource a -> a -> IO ()
fire = snd . unES
{-# INLINE fire #-}


-- | A source for a reactive-banana 'Behavior'. Contains the information
-- required by reactive-banana to create the corresponding 'Behavior',
-- and also an action used to update it.
newtype BehaviorSource a = BS { unBS :: (EventSource a, a) }

-- | Create the source for a new behavior, given its initial value
newBehavior :: a -> IO (BehaviorSource a)
newBehavior x = (BS . (,x)) `fmap` newEvent
{-# INLINE newBehavior #-}

-- | Obtain the behavior that corresponds to the given source.
fromBehaviorSource :: (Frameworks t) => BehaviorSource a -> Moment t (Behavior t a)
fromBehaviorSource (BS (es, x)) = stepper x `fmap` fromEventSource es
{-# INLINE fromBehaviorSource #-}

-- | Update the value of the given behavior.
update :: BehaviorSource a -> a -> IO ()
update = fire . fst . unBS
{-# INLINE update #-}
