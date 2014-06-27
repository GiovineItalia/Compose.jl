# Compose!

Compose is a vector graphics library for Julia.
It forms the basis for the statistical graphics system
[Gadfly](https://github.com/dcjones/Gadfly.jl).


## Synopsis

Unilke most vector graphics libraries, compose is thoroughly declarative. Rather
than issue a sequence of drawing commands, graphics are formed by sticking
various things together and then letting the library figure out how to draw it.
The "things" in this case fall one of three types: Property, Form, and Canvas.
"Sticking together" is primary achieved with the `compose` function.

The semantics of composition are fairly simple, and once grasped provide a
consistent and powerful means of building vector graphics.

## Example

The easiest way to get a sense of how Compose works is with an example. So,
here's how to draw a sierpinski triangle.

![Sierpinski](http://dcjones.github.com/Compose.jl/sierpinski.svg)

```julia
using Compose

function sierpinski(n)
    if n == 0
        compose(context(), polygon([(1,1), (0,1), (1/2, 0)]))
    else
        t = sierpinski(n - 1)
        compose(context(),
                (context(1/4,   0, 1/2, 1/2), t),
                (context(  0, 1/2, 1/2, 1/2), t),
                (context(1/2, 1/2, 1/2, 1/2), t))
    end
end

img = SVG("sierpinski.svg", 4inch, 4(sqrt(3)/2)inch)
draw(img, compose(sierpinski(8), linewidth(0.1mm), fill(nothing), stroke("black")))
```

A graphic in Compose is a tree of `Context` objects, each specifying a coordinate
system relative to its parent canvas. One context is made a child of another with
a call to `compose(a::Context, b::Context)`.

Contexts may also have children of type `Form`, which are rectangles, ellipses,
text, etc, and `Property`, which are line width, fill color, etc. `Form` and
`Property` nodes are always leaf nodes.

## Fancier compositions

There are fancier forms of the compose function. In particular, variadic
compose, which is roughly defined as:

```julia
compose(a, b, cs...) = compose(compose(a, b), cs...)
```

Compose over tuples or arrays:
```julia
compose((as...)) = compose(as...)
```

In effect, this lets one write a complex series of compose operations as an
S-expression. For example:

```julia
compose(a, b, ((c, d), (e, f), g))
```

Since all we are doing is building trees, this syntax tends to be pretty
convenient.

## Coordinates

Besides coordinate transformations, Compose also handles mixtures of relative
and absolute coordinates. For example, `1w - 10mm` is a well formed expression,
giving the width of the parent canvas minus ten millimeters.

## Influences

Compose is intended as a futuristic version of the R library
[grid](http://www.stat.auckland.ac.nz/~paul/grid/grid.html), and so takes a few
ideas from grid. The Compose canvas is roughly equivalent to a viewport in grid,
for example. The Haskell library
[Diagrams](http://projects.haskell.org/diagrams/) was another starting point
with many admirable notions I hope to steal.

