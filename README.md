
# Compose!

Compose is a vector graphics library for Julia, with just three types and one
function (called `compose!`, oddly enough).

But enough chatter, here's how to draw a badass Sierpinski triangle you can use
to impress girls.

    function sierpinski(n)
        if n == 0; return Polygon((2, 1), (0, 1), (1, 0)); end

        t = sierpinski(n - 1)
        compose!(Canvas(),
                (Canvas(1, 0, 2, 1), t),
                (Canvas(0, 1, 2, 1), t),
                (Canvas(2, 1, 2, 1), t))
    end


    @upon SVG("sierpinski.svg", 7inch, 7(sqrt(3)/2)inch) begin
        draw(compose!(sierpinski(10), Fill(nothing), LineWidth(0.1mm)))  
    end


TODO: sierpinski svg

Sweet, huh?


## Principles

Unlike most vector graphics libraries, Compose is thoroughly declarative.
Graphics are specified, or composed, without the need to give a specific
sequence of drawing operations. This 

The graphic consists of combinations of objects of three types: `Canvas`,
`Form`, and `Property`. A canvas defines a particular region of the graphic, a
form is something drawn on the image (e.g., lines, rectangles, circles), and a
property modifies how forms are drawn (e.g. color, line thickness).

These types can be composed with the `compose!` function (the `!`, in addition
to being an expression of the overwhelming euphoria one feels when using this
library, is convention in Julia, by way of Lisp, denoting a function that might
modify it arguments). In its basic form, `compose!` function takes two arguments
and sticks the second argument onto the first, then returns the (modified) first
argument. What exactly "sticks onto" means depends on the types involved. In
typical Julian fashion, multiple dispatch plays a big role.

A property may be composed with another property, producing a property that
consists of both. So `p = compose(Fill("red"), Stroke("green"))` makes a
property that changes both the fill and stroke color.

Here is a handy chart to help you remember the composition rules.

TODO

## Fancy Compositions

Compose provides some other forms of `compose!` as syntactic short cuts that
expand into a series of calls to the simple form.

### Chained Compositions

Suppose you have a canvas `c` onto which you want to compose a number of forms
`f`, `g`, `h`. No problem, dude! Just use the variadic version:

    compose!(c, f, g, h)

This is equivalent to

    compose!(compose!(compose!(c, f), g), h)


### Compositions as S-Expressions

Generating a graphic in Compose consists of a series of compositions, just as
evaluating a Lisp program consists of a series of function applications. The
semantic simplicity in both systems allows for a simple syntax: S-expressions.

The third form of `compose!` is a simple S-expression parser. It takes a tuple
or vector of graphics elements, and repeatedly calls `compose!` to 

In our first example, this allowed us to replace

        compose!(Canvas(),
                compose!(Canvas(1, 0, 2, 1), t),
                compose!(Canvas(0, 1, 2, 1), t),
                compose!(Canvas(2, 1, 2, 1), t))

With just

        compose!(Canvas(),
                (Canvas(1, 0, 2, 1), t),
                (Canvas(0, 1, 2, 1), t),
                (Canvas(2, 1, 2, 1), t))

Grouped elements are implicitly composed.

