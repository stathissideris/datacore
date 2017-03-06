# datacore

An emacs-like for data

## Design

Current thinking is that all state in the application (both for data
processing and UI state) will be propagated via cells. This feels
right because datacore wants to be an spreadsheet for experts and
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

## Cells

Differences to Javelin:

 * No support for `cell=` macro, only `formula` so creating formula
   cells is less convenient.
 * Cycle detection TODO: make error messages more informative.
 * Cells can have labels.
 * Tabular debug information on all the cells, their state, their
   properties, possible errors etc.
 * Mute/unmute formula cells to make them pass values through -
   without processing.
 * Ability to destroy a cell and all its "orphaned" sinks (if
   any). TODO: fix potential stack overflow.

## License

Copyright © 2017 Stathis Sideris

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
