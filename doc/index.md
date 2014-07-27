---
title: Compose
author: Daniel C. Jones
...


```{.julia hide="true"}
using Compose, Color

function drawlogo()
    color1 = LCHab(68, 74, 192)
    color2 = LCHab(92, 10, 77)
    #color3 = LCHab(78, 84, 29)
    apeture = 90
    thickness = 1.5mm
    Δθ = deg2rad(45)

    x1, y1 = cosd(apeture),  sind(apeture)
    x2, y2 = cosd(-apeture), sind(-apeture)
    x3, y3 = x2*cx - x2*thickness, y2*cy - y2*thickness
    x4, y4 = x1*cx - x1*thickness, y1*cy - y1*thickness

    c = compose(context(units=UnitBox(-1, -1, 2, 2)),
                path([:M, x1, y1,
                      :A, 1, 1, 0, true, true, x2, y2,
                      :L, x3, y3,
                      :A, 1cx - thickness, 1cy - thickness,
                          0, true, false, x4, y4,
                      :z]))

    logo = ctxpromise() do drawctx
        n = ifloor(min(drawctx.box.width / (thickness / 1mm) / 2,
                       drawctx.box.height / (thickness / 1mm) / 2))
        @show n
        root = context()
        for i in 1:n
            rot = Δθ * (i - 1)
            scale = (i - 1) * thickness
            compose!(root,
                     (context(scale, scale, 1cx - 2scale, 1cy - 2scale,
                              rotation=Rotation(Δθ * (i-1))),
                      fill(isodd(i) ? color1 : color2), c))
        end

        return root

    end
    return compose(context(), logo,
                   (context(), rectangle(), fill("#333")),
                   stroke("#333"), linewidth(0.5mm))
end

draw(SVG(3inch, 3inch), drawlogo())
```

Compose is a declarative vector graphics system written in Julia. It's designed
to simplify the creation of complex graphics and serves as the the basis of the
[Gadfly](https://github.com/dcjones/Gadfly.jl) data visualization package.

Compose is declarative
----------------------

In a declarative graphics system, a figure is built without specifying the
precise sequence of drawing commands but by arranging shapes and attaching
properties. This makes it easy to break a complex graphic into managable parts
and then figure out how to combine the parts.

Everything is a tree
--------------------

Graphics in Compose are defined using a tree structure. It's not unlike SVG in
this regard, but has simpler semantics. There are three important types that
make up the nodes of the tree:

  * `Context`: An internal node.
  * `Form`: A leaf node that defines some geometry, like a line or a polygon.
  * `Property`: A leaf node that modifies how its parent's subtree is drawn,
    like fill color, font family, or line width.

The all-important function in Compose, is called, not surprisingly, `compose`.
Calling `compose(a, b)` will return a new tree rooted at `a` and with `b`
attached as a child.

That's enough to start drawing some simple shapes.

```{.julia hide="true"}
set_default_graphic_size(4cm, 4cm)
```

```julia
using Compose

compose(compose(context(), rectangle()), fill("tomato"))
```

The compose function accepts S-expressions
------------------------------------------

In the first example, we had to call `compose` twice just to draw a lousy red
square. Fortunately `compose` has a few tricks up its sleave. As everyone from
lisp hackers and [phylogeneticists](http://en.wikipedia.org/wiki/Newick_format)
knows, trees can be defined most tersely using S-expressions. We can rewrite our
first example like:

```{.julia execute="false"}
# equivalent to compose(compose(context(), rectangle()), fill("tomato")))
compose(context(), rectangle(), fill("tomato"))
```

Furthermore, more complex trees can be formed by grouping subtrees into
parethesis or brackets.


```julia
compose(context(),
        (context(), circle(), fill("bisque")),
        (context(), rectangle(), fill("tomato")))
```

Trees can be visualized with introspect
---------------------------------------

A useful function for visualizing the graphic that you've constructed is
`introspect`. It takes a `Context` defining a graphic and returns a new graphic
with a schematic of the tree.

```{.julia hide="true"}
set_default_graphic_size(6cm, 6cm)
```

```julia
tomato_bisque =
    compose(context(),
            (context(), circle(), fill("bisque")),
            (context(), rectangle(), fill("tomato")))

introspect(tomato_bisque)
```

This is a little cryptic, but you can use this limited edition decoder ring:

```{.julia hide="true"}
set_default_graphic_size(6cm, 4cm)
```

```{.julia hide="true"}
figsize = 6mm
t = table(3, 2, 1:3, 2:2, y_prop=[1.0, 1.0, 1.0])
t[1,1] = [compose(context(minwidth=figsize + 2mm, minheight=figsize),
                  circle(0.5, 0.5, figsize/2), fill(LCHab(92, 10, 77)))]
t[2,1] = [compose(context(minwidth=figsize + 2mm, minheight=figsize),
                  rectangle(0.5cx - figsize/2, 0.5cy - figsize/2, figsize, figsize),
                  fill(LCHab(68, 74, 192)))]
t[3,1] = [compose(context(minwidth=figsize + 2mm, minheight=figsize),
                  polygon([(0.5cx - figsize/2, 0.5cy - figsize/2),
                           (0.5cx + figsize/2, 0.5cy - figsize/2),
                           (0.5, 0.5cy + figsize/2)]),
                  fill(LCHab(68, 74, 29)))]
t[1,2] = [compose(context(), text(0, 0.5, "Context", hleft, vcenter))]
t[2,2] = [compose(context(), text(0, 0.5, "Form", hleft, vcenter))]
t[3,2] = [compose(context(), text(0, 0.5, "Property", hleft, vcenter))]
compose(context(), t, fill(LCHab(92, 10, 77)), fontsize(10pt))
```


Contexts specify a coordinate system for their children
--------------------------------------------------------

In addition to forming internal nodes to group `Form` and `Property` children, a
`Context` defines a coordinate system using the `context(x0, y0, width, height)`
form.

```{.julia hide="true"}
set_default_graphic_size(4cm, 4cm)
```

```julia
compose(context(), fill("tomato"),
        (context(0.0, 0.0, 0.5, 0.5), circle()),
        (context(0.5, 0.5, 0.5, 0.5), circle()))
```

The context's box (i.e. `(x0, y0, width, height)`) is given in terms of its
parent's coordinate system and defaults to `(0, 0, 1, 1)`.

This is an easy mechanism to translate the coordinates of 




Measures can be a combination of absolute and relative units
------------------------------------------------------------

Complex visualizations often are defined using a combination of relative and
absolute units. Compose makes these easy.



Contexts can impose a coordinate system on their children
---------------------------------------------------------




Forms and Properties can be vectorized
--------------------------------------




