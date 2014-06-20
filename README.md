# ~~Compose!~~ Propose!

Compose is a (experimental, work-in-progress) vector graphics library for Julia.
It is forming the basis for the statistical graphics system
[Gadfly](https://github.com/dcjones/Gadfly.jl).

This branch is an experimental rewrite of Compose aimed at building a more
efficient, elegant, and consistent system while granting it superpowers. It's
not yet ready for general use, but will hopefull get merged in the future.


This rewrite has three major goals:

  1. Compose has no way of deciding between multiple possible layouts. Compose will
     use integer linear programming to solve table layout problems while leaving
     room to add other sorts of layout optimization algorithms (e.g. TeX-style
     paragraphing).

     This will open the door to adding a more sophisticated notion of layout to
     Gadfly: keys can be split into multiple columns, axis labels can be
     rotated or split into multiple lines, plots can warn the user when
     they need more room to be drawn correctly. And all of this will be done
     automatically and without a significant hit to performance (table layout is
     NP-complete in general, but Gadfly only needs to solve relatively small
     instances).

  1. Compose was written with little regard to peformance. This shows when one
     tries to plot thousands of points in Gadfly. In this branch, geometry like
     circles, rectangles, and lines will be vectorized to avoid the overhead of
     building a tree with thousands of nodes and computing coordinate
     transforms one the nodes one-by-one.

     This change has the added benifit of making this branch a closer fit to
     d3.js than Compose. D3 support was tacked onto Compose, and as a result is
     ugly and generates suboptimal javascript in some cases. Not only should
     plotting be faster with Pose, the generated javascript should be faster as
     well.

     Furthermore, along with the ubiquitous `compose` function, a more efficient
     `compose!` function will be added to do efficient destructive updates.

   1. Miscellaneous refactoring. Since this is in no way backwards compatible,
      stuff will get renamed and rearranged whenever I feel like
      it (E.g. `canvas` is now called `context`, because `canvas` is a terrible
      name).



