{-|
Description: Groups of Events and Behaviors for monitoring the system.
Copyright: (c) Guilherme Azzi, 2014
License: MIT
Maintainer: ggazzi@inf.ufrgs.br
Stability: experimental

People want a variety of information to be displayed in the system's status bar.
Some of the are easy to obtain, some require quite a bit of code. Some of them
are obtained quickly by polling the system (e.g. CPU load), some of them may
require a long time to poll (e.g. current weather) and some of them may be
better provided asynchronously (e.g. current workspace in XMonad).

The method used to obtain a piece of information should not, however, affect the
code describing how it should be displayed. In particular, blocking operations
should not delay updates to the bar, since other pieces of information may have
already been obtained.

In order to achieve this, the description of how information is displayed is
done in terms of 'Behavior's: time-varying values. Therefore the bar can be
redrawn at any point in time, simply using the current values of each
'Behavior'. These values must be updated somewhere, which is encapsulated using
'Monitor's and their sources.

A 'Monitor' is therefore a group of 'Event's and 'Behavior's, with a particular
kind of source. The source of a monitor provides the actions that will update
'Behavior's and fire 'Event's. Therefore, using multiple 'Monitor's in the main
loop of a reactive-banana application is just a matter of executing the actions
provided by their sources.

Much information about the system is obtained by simple and quick polling of
small files (e.g. CPU load, battery status). Therefore, the main loop should be
provided with an action that should be run regularly, polling the information
and causing the corresponding 'Behavior' to be updated. This polling may,
however, need information from previous iterations, i.e. some state. Therefore,
the 'SourceOf' a 'Monitor' provides an initialization action, which may then
provide the polling action.

Some information may also be asynchronously obtained. XMonad may, for instance,
write the current focused window on a certain pipe, whenever it has changed. In
this case, the initialization action should create a new listener thread, which
handles the external events and updates the 'Behavior's. In this case, no
polling action is provided.

Other kinds of information may be also obtained by polling, but with much more
expensive (or even blocking) actions, e.g. obtaining the current weather from a
website. In this case, the polling action /MUST NOT/ execute such operations.
Instead, similarly to asynchronous information, the initializer should setup
a different thread which, in turn, executes the polling.

-}

module Reactive.Banana.Monitors
       ( module Reactive.Banana.Monitors.Class
       ) where

import Reactive.Banana.Monitors.Class
