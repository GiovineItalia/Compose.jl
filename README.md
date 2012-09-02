
# Compose!

Compose is a (experimental, work-in-progress) vector graphics library for Julia,
with just three types and one function called, oddly enough, `compose!`.  Don't
worry though, you rarely have to call the one function because graphics in
Compose are specified using a vaguely Lisp-like syntax.

But enough chatter, here's how to draw a badass Sierpinski triangle you can use
to impress girls.

![Sierpinski](http://dcjones.github.com/compose/sierpinski.svg)


```julia
load("compose.jl")

function sierpinski(n)
    if n == 0; return Polygon((1, 1), (0, 1), (1/2, 0)); end

    t = sierpinski(n - 1)
    {Canvas(),
      {Canvas(1/4,   0, 1/2, 1/2), t},
      {Canvas(  0, 1/2, 1/2, 1/2), t},
      {Canvas(1/2, 1/2, 1/2, 1/2), t}}
end

@upon SVG("sierpinski.svg", 4inch, 4(sqrt(3)/2)inch) begin
    draw({sierpinski(6), LineWidth(0.1mm)})
end
```

Sweet, huh?


## Principles

That example was probably incoherent, but hopefully intruiging or bewildering
enough to propel you through a bit of exposition.

Unlike most vector graphics libraries, Compose is thoroughly declarative.
Graphics are specified, or composed, without the need to give a specific
sequence of drawing operations.  The graphic consists of combinations of objects
of three types: `Canvas`, `Form`, and `Property`. A canvas defines a particular
rectangular region of a graphic and a coordinate system within that rectangle, a
form is something drawn on the image (e.g., lines, rectangles, circles), and a
property modifies how forms are drawn (e.g.  color, line thickness).

These types can be composed with the `compose!` function (the `!`, in addition
to being an expression of the overwhelming euphoria one feels when using this
library, is convention in Julia, by way of Lisp, denoting a function that might
modify it arguments). In its basic form, `compose!` function takes two arguments
and sticks the second argument onto the first, then returns the (modified) first
argument. What exactly "sticks onto" means depends on the types involved. In
typical Julian fashion, multiple dispatch plays a big role.

The laws of composition are as follows:

1. A property may be composed with another property, forming the union of the
two. E.g. `compose(Fill("red"), Stroke("green"))` sets the fill and stroke
color.

2. A form may be composed with a form, forming the union of the two. E.g.
`compose(Lines((0,0), (1,1)), Lines((0,1), (1,0)))` is a form with two lines.

3. A form may be composed with a property, modifying how that form is drawn.

4. A canvas may be composed with a form, placing the form in the canvas.
E.g. `compose(Canvas(), Rectangle())` places a rectangle in the canvas.

5. A canvas may be composed with another canvas, placing the latter in the
former.

6. A canvas may be composed with a property, applying that property to every
form contained in the canvas.

That is the complete semantics of the system. The rest is just syntactic
conveniences.


## Compose as an S-Expression Evaluator

Calling `compose!(a,b)` over and over gets tiring, but since all drawing
consists of calls to `compose!`, we can just make them all implicit. To do this,
we used a Lisp-style syntax.

We allow `compose!` to be called on a sequence, with the following (recursive)
rule:

```julia
compose!(a, b, c, ...) <=> compose!(compose!(a, b), c, ...)
```

If we want to place two form `a` and `b` in a canvas `c`, we can now just write:

```julia
compose!(c, a, b)
```

We can take this one step further and embed sub-expressions. So, if we want to
place `a` and `b` in `c`, then place `c` in another canvas `d`, we can do this:

```julia
compose!(d, {c, a, b})
```

Recall, `{}` gives a untyped array in Julia. With this, the `compose!` function
becomes so versatile that it puts itself out of job: you basically never have to
call it explicitly. Just build up a graphic as nested vectors then call draw on
it.


## Backend

Compose can use multiple backends. Currently only Cairo is supported, producing
SVG, Postscript, PDF, and PNG images.

TODO: Write this section. (i.e., explain `draw` and `@upon`).

## Units

Compose has a sophisticated notion of units.

TODO: Write this section.


## More Examples

### Golden Rectangle

![Golden Rectangle](http://dcjones.github.com/compose/golden_rect.svg)

```julia
load("compose.jl")

const phi = (1 + sqrt(5)) / 2

function golden_rect(n::Int)
    if n == 0; return nothing; end
    {Canvas(),
      {Rectangle(), Fill(LCHab(90, 80, 70 - 15n))},
      {Canvas(0, -1/phi, 1h, 1/phi, Rotation(pi/2, 0, 1)),
         golden_rect(n - 1)}}
end

@upon SVG("golden_rect.svg", phi * 3inch, 3inch) begin
    draw(pad!({golden_rect(10), Fill(nothing),
               Stroke("white"), LineWidth(0.2mm)}, 1mm))
end
```

### Pointless Interactivity

(This example doesn't work within the README on github. Click it!)

![Pointless Interactivity](http://dcjones.github.com/compose/interact.svg)

```julia

load("compose.jl")

choose_color_js =
"
colors = $(json([LCHab(80, 70, h) for h in 0:15:360]));
evt.target.setAttribute(\"fill\", colors[Math.floor(colors.length * Math.random())]);
"

n = 30
m = 30
cs = grid(n, m)
for i in 1:n
    for j in 1:m
        compose!(cs[i,j], {Polygon((1,0), (1,1), (0,1)), 
                 OnMouseOver(choose_color_js),
                 Fill("grey70")})
    end
end

@upon SVG("interact.svg", 4inch, 4inch) begin
    draw(pad!({Canvas(), cs..., Stroke(nothing)}, 1mm))
end

```

## Influences

Compose is indended as a futuristic version of the R library grid, and so takes
a few ideas from grid. The Compose canvas is roughly equivalent to a viewport in
grid, for example. The Haskell library Diagrams was another starting point with
many admirable notions I hope to steal.


## Future Work

Currently this library is minimally useful. Some things I will get to sooner or
later:

* Drawing text.
* A custom SVG backend to make scripting and animation possible.
* I'd like to perform some sort of unit and/or image size inference. With such a
  general notion of units, this is pretty tricky.


