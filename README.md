# conway

This repository contains a simple solver for the [Slothouber-Graatsma puzzle](http://mathworld.wolfram.com/Slothouber-GraatsmaPuzzle.html), sometimes known as the Conway puzzle cube. It is sufficiently general to handle any puzzle that involves placing a number of differently shaped rectangular blocks (in any number of dimensions) onto a rectangular grid, but is **not** optimized in any way.

To run it, you will need to install [Leiningen](http://leiningen.org/). Then, simply navigate to the project directory and execute `lein run`.

If you would like to experiment, start a REPL with `lein repl`. The puzzle is defined by the dynamic vars `*grid-shape*`, `*block-shapes*`, and `*block-counts*` defined in the `conway.core` namespace, and you can request a solution using the `solve` function in the same namespace. See the documentation strings on the `conway.core` namespace and the `solve` function for more information.
