
# Compose!

Compose is a (experimental, work-in-progress) vector graphics library for Julia.
It is forming the basis for the statistical graphics system
[Gadfly](https://github.com/dcjones/gadfly).


## Synopsis

Unilke most vector graphics libraries, compose is thoroughly declaritive. Rather
that issues a sequence of drawing commands, graphics are formed by sticking
various things together and then letting the library figure out how to draw it.
The "things" in this case fall one of three types: Property, Form, and Canves.
"Sticking together" is primary achieved with the `compose` function.

The semantics of composition are fairly simple, and once grasped provide a
consistent and powerful means of building vector graphics.

## Example

The easiest way to get a sense of how Compose works is with an example. So,
here's how to draw a sierpinski triangle.

![Sierpinski](http://dcjones.github.com/compose/sierpinski.svg)

```julia
load("Compose")
using Compose

function sierpinski(n)
    if n == 0
        compose(canvas(), polygon((1,1), (0,1), (1/2, 0)))
    else
        t = sierpinski(n - 1)
        compose(canvas(),
                (canvas(1/4,   0, 1/2, 1/2), t),
                (canvas(  0, 1/2, 1/2, 1/2), t),
                (canvas(1/2, 1/2, 1/2, 1/2), t))
    end
end

img = SVG("sierpinski.svg", 4inch, 4(sqrt(3)/2)inch)
draw(img, compose(sierpinski(8), linewidth(0.1mm), fill(nothing)))
finish(img)

```

A graphic in Compose is a tree of `Canvas` object, each specifying a coordinate
system relative to its parent canvas. One canvas is made a child of another with
a call to `compose(a::Canvas, b::Canvas)`.

Canvases may also have children of type `Form` which are rectangles, ellipses,
text, and other things that end up being drawn. Properties, which effect how
forms are drawn, may be children of `Canvas` or `Form` objects.

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

In effect, this let's one write a complex series of compose operations as an
S-expression. For example:

```julia
compose(a, b, ((c, d), (e, f), g))
```

Since all we are doing is building trees, this syntax tends to be pretty
convenient.

## Combinations

There is a lesser operation in Compose called `combine`, defined over Properties
and Forms. It acts essentially as a union operation. The combinations of two
forms is a new form that draws both. The combination of two properties is a new
property that has the effect of both.

## Coordinates

Besides coordinate transformations, Compose also handles mixtures of relative
and abosule coordinates. For example, `1w - 10mm` is a well formed expression,
giving the width of the parent canvas minus ten millimeters.

## Influences

Compose is indended as a futuristic version of the R library
[grid](http://www.stat.auckland.ac.nz/~paul/grid/grid.html), and so takes a few
ideas from grid. The Compose canvas is roughly equivalent to a viewport in grid,
for example. The Haskell library
[Diagrams](http://projects.haskell.org/diagrams/) was another starting point
with many admirable notions I hope to steal.

