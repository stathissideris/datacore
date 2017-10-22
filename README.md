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

## Usage

This is an extremely young project, and the the UX still leaves a lot
to be desired, but if you'd like to get a glimpse of where it's going,
here are some instructions on how to run it and thy a few things:

* Checkout master

```
git clone git@github.com:stathissideris/datacore.git
```

* Jack-in the project (or start a REPL in some other way) and then do:

```clojure
> (dev/refresh)
> (datacore.main/init)
```

* Look for the window that was opened (in OSX at least it doesn't come
  to the front of other windows by default).
* Press `C-x 3` and `C-x 2` to try window splitting. `C-x 1`, `C-x 0`
  also work. `<F1> <right>` makes the focus jump to the window on the
  right (all directions work). Focus is indicated by a blue
  border. You can also change focus with the mouse. `<F2> <right>`
  swaps the current window with the window on the right.
* Press `C-x C-f` to get a prompt for loading a CSV file (for now it
  defaults to test-resources for ease of testing).
* File select prompt keys:
  * just type for autocomplete
  * `<up>`/`<down>` to select a different option from list
  * `<esc>` or `C-g` to cancel
  * `<tab>` to go into selected directory
  * `<enter>` to select a file
* Select watchlist.csv
* Press `M-x table/row-edn-view <enter>` to get a view that shows the
  currently selected row as EDN.
* Press `C-h f table/scroll-down <enter>` to see keys for table
  navigation (all related functions at the bottom).
* Press `C-x C-b` to see all the current cells (excluding system cells)
  and use arrow keys and <enter> to bring back the watchlist table
  view (indicated by eye icon and `:table-view` name).
* Press `M-x cells/chart-frequencies <enter>` to get a prompt on the
  Clojure expression on which to base the histogram. Just type `:year`
  and press `<enter>`.
* Make sure that the table has focus and press `<M-x>
  cells/add-transform-cell <enter>` and in the prompt enter this
  expression:

  ```clojure
  (filter #(= "Documentary" (:title-type %)) data)
  ```

* Notice that both the table view and the histogram get updated.
* Open the cells table view again with `C-x C-b` (ideally arrange the
  windows so that you can also see the table and histogram), press
  `<down>` until the newly added `:transform-cell` is highlighted, and
  press `<m>` repeatedly to mute/unmute the filtering. The table and
  histogram should update accordinly.
* You can press `<e>` to edit the code of the transform cell.

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

## Related projects

 * [Strukt](https://www.freerobotcollective.com/) has very similar goals - [HN thread](https://news.ycombinator.com/item?id=14471153)
 * https://handsontable.com
 * https://www.ag-grid.com
 * https://median.tech
 * [Google Sheets explore functionality](https://www.blog.google/products/g-suite/visualize-data-instantly-machine-learning-google-sheets/) - [HN thread](https://news.ycombinator.com/item?id=14469645)

## License

Copyright © 2017 Stathis Sideris

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
