hzen
====

This package provides utilities for implementing status bars in a functional reactive style.
The actual rendering is done by the [dzen](github.com/robm/dzen) program, which receives a
textual description of what it should render, therefore users should ensure that this program
is installed.

This package just provides a structured way to feed dzen with data. A sample configuration can
be found in `examples/simple.hs`, and it may be executed by calling `cabal run` in the root
directory of this project.

While this README is admittedly tiny, the haddock-based documentation is much more extensive.
In order to read it, either browse the source files themselves or run `cabal haddock`, then
open the created html files.

The major components of this are Monitors and Widgets. There is a heavy dependency on the
reactive-banana package, and a lot of inspiration was drawn from the xmobar and dzen-utils
packages.

In order for a user to pick several aspects of the system that will be displayed in the
status bar, the notion of Monitor was introduced. A Monitor is essentially the source of
Behaviors and/or Events, providing also an initialization action and a polling action.
The obtaining of system information is thus encapsulated with little loss of composability.

In order to define the dzen output in a convenient and type-safe way, Widgets
are provided. They may be though of as Behaviors of String, that is, time-varying
dzen command strings. A little more machinery is defined, though, to enhance
the composability.
