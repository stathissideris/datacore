# Datacore

An emacs-like for data

Datacore:

 * Is currently vaporware, the rest of this list should be read as
   aspirations rather than a description of Datacore.
 * Is an EDN viewer, with support for Clojure spec.
 * Is a small ETL engine.
 * Uses cell-based programming to keep the UI and the data being
   processed consistent and constatly up to date throughout the
   application.
 * Is a Clojure snippets editor, with structural editing.
 * Supports simple graphs.
 * Mimics Emacs in terms of keyboard focus, configurability and
   extensibility via Lisp.
 * Is text driven where is makes sense, but can also be extended to
   include simple UIs by wrapping JavaFX.

## Design

Current thinking is that all state in the application (both for data
processing and UI state) will be propagated via cells. This feels
right because Datacore wants to be an spreadsheet for experts and
spreadsheets have cells.

Prior work on cells:

 * Kenny Tilton's
   [Common Lisp cells](https://github.com/kennytilton/cells). More
   explanation
   [on his blog](https://smuglispweeny.blogspot.gr/2009/02/cells-secret-transcript.html)
 * Kenny Tilton's deprecated cells port to Clojure,
   [It's Alive!](https://github.com/kennytilton/its-alive).
 * Kenny Tilton's Clojure/Clojurescript port of cells, [Rube](https://github.com/kennytilton/rube).
 * Clojurescript's [Javelin](https://github.com/hoplon/javelin). There's a [long thread](https://github.com/hoplon/javelin/issues/25) about Clojure support.
 * Python's [Trellis](http://peak.telecommunity.com/DevCenter/Trellis).

Still being considered:

 * A [React](https://facebook.github.io/react/docs/reconciliation.html)-like approach.

## Cells implementation

Differences to Javelin:

 * Both immutable and mutable APIs for the cells mechanism.
 * Cycle detection - TODO: make error messages more informative.
 * Cells can have labels and metadata.
 * Tabular debug information on all the cells, their state, their
   properties, possible errors etc.
 * Mute/unmute formula cells to make them pass values through -
   without processing.
 * Ability to have partially disconnected cells.
 * Ability to swap the function of a formula cell with a different
   one.
 * Ability to "move-up" and "move-down" cells along a linear chain of
   cells.
 * Ability to insert new cells in the middle of a linear chain of
   cells.
 * Ability to destroy a cell.
 * No support for `cell=` macro, only `formula` so creating formula
   cells is less convenient.

## Reading list

 * [Reactive programming](https://en.wikipedia.org/wiki/Reactive_programming)
 * [Functional reactive programming](https://en.wikipedia.org/wiki/Functional_reactive_programming)
 * [FRP - Three principles for GUI elements with bidirectional data flow](http://apfelmus.nfshost.com/blog/2012/03/29-frp-three-principles-bidirectional-gui.html) - very interesting comments too!
 * [HotDrink JavaScript library](https://github.com/HotDrink/hotdrink)

## License

Copyright © 2017 Stathis Sideris

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
