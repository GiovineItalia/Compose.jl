
# Compose!

Compose is a (experimental, work-in-progress) vector graphics library for Julia.
It is forming the basis for the statistical graphics system
[Gadfly](https://github.com/dcjones/gadfly).


## Synopsis

Unilke most vector graphics libraries, compose is thoroughly declaritive. Rather
that issues a sequence of drawing commands, graphics are formed by sticking
various things together and then letting the library figure out how to draw it.
The "things" in this case fall one of three types: Property, Form, and Canves.
"Sticking together" is accomplished either with composition (intra-type sticking
together) or insertion (inter-type sticking together).

The semantics of composition and insertion are fairly simple, and once grasped
provide a consistent and powerful means of building vector. graphics.

## Example

The easiest way to get a sense of how Compose works is with an example. So,
here's how to draw a sierpinski triangle.

![Sierpinski](http://dcjones.github.com/compose/sierpinski.svg)

```julia
load("compose.jl")

function sierpinski(n)
    if n == 0
        canvas() << polygon((1,1), (0,1), (1/2, 0))
    else
        t = sierpinski(n - 1)
        canvas() |
            (canvas(1/4,   0, 1/2, 1/2) | t) |
            (canvas(  0, 1/2, 1/2, 1/2) | t) |
            (canvas(1/2, 1/2, 1/2, 1/2) | t)
    end
end

@upon SVG("sierpinski.svg", 4inch, 4(sqrt(3)/2)inch) begin
    draw(sierpinski(8) << (linewidth(0.1mm) | fill(nothing)))
end
```

Composition is accomplished with the `|` operator and insertion with `<<`. If
not otherwise specified, coordinates are given in "canvas units" in which
`(0,0)` is the top-left of the canvas and `(1,1)` the bottom right. The
Sierpinski triangle is drawn recursively simply by dividing a canvas into three
sub-canvases and drawing a sierpinski triangle in each. The recursion terminates
with a simple triangle form.

One thing to note is that there only every exists one Polygon instance, but it
is reused in many contexts. Because referential transparency throghout the
library, one instance of Polygon is as good as any other with the same points.


## Influences

Compose is indended as a futuristic version of the R library
[grid](http://www.stat.auckland.ac.nz/~paul/grid/grid.html), and so takes a few
ideas from grid. The Compose canvas is roughly equivalent to a viewport in grid,
for example. The Haskell library
[Diagrams](http://projects.haskell.org/diagrams/) was another starting point
with many admirable notions I hope to steal.

