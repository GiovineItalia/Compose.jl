var documenterSearchIndex = {"docs": [

{
    "location": "index.html#",
    "page": "Home",
    "title": "Home",
    "category": "page",
    "text": "Author = [\"Daniel C. Jones\", \"Gio Borje\", \"Tamas Nagy\"]"
},

{
    "location": "index.html#Compose-1",
    "page": "Home",
    "title": "Compose",
    "category": "section",
    "text": "Compose is a declarative vector graphics system written in Julia. It's designed to simplify the creation of complex graphics and serves as the basis of the Gadfly data visualization package."
},

{
    "location": "index.html#Compose-is-declarative-1",
    "page": "Home",
    "title": "Compose is declarative",
    "category": "section",
    "text": "In a declarative graphics system, a figure is built without specifying the precise sequence of drawing commands but by arranging shapes and attaching properties. This makes it easy to break a complex graphic into manageable parts and then figure out how to combine the parts."
},

{
    "location": "index.html#Everything-is-a-tree-1",
    "page": "Home",
    "title": "Everything is a tree",
    "category": "section",
    "text": "Graphics in Compose are defined using a tree structure. It's not unlike SVG in this regard, but has simpler semantics. There are three important types that make up the nodes of the tree:Context: An internal node.\nForm: A leaf node that defines some geometry, like a line or a polygon.\nProperty: A leaf node that modifies how its parent's subtree is drawn, like fill color, font family, or line width.The all-important function in Compose, is called, not surprisingly, compose. Calling compose(a, b) will return a new tree rooted at a and with b attached as a child.That's enough to start drawing some simple shapes.using Compose\nset_default_graphic_size(4cm, 4cm)\nset_default_jsmode(:exclude)using Compose\n\ncomposition = compose(compose(context(), rectangle()), fill(\"tomato\"))\ndraw(SVG(\"tomato.svg\", 4cm, 4cm), composition)\nnothing # hideThe last line renders the composition to specificied backend, here the SVG backend. This can also be written like composition |> SVG(\"tomato.svg\", 4cm, 4cm).(Image: )"
},

{
    "location": "index.html#The-compose-function-accepts-S-expressions-1",
    "page": "Home",
    "title": "The compose function accepts S-expressions",
    "category": "section",
    "text": "In the first example, we had to call compose twice just to draw a lousy red square. Fortunately compose has a few tricks up its sleeve. As everyone from lisp hackers and phylogeneticists knows, trees can be defined most tersely using S-expressions. We can rewrite our first example like:# equivalent to compose(compose(context(), rectangle()), fill(\"tomato\")))\ncompose(context(), rectangle(), fill(\"tomato\"))Furthermore, more complex trees can be formed by grouping subtrees with parenthesis or brackets.composition = compose(context(),\n        (context(), circle(), fill(\"bisque\")),\n        (context(), rectangle(), fill(\"tomato\")))\ncomposition |> SVG(\"tomato_bisque.svg\", 4cm, 4cm)\nnothing # hide(Image: )"
},

{
    "location": "index.html#Trees-can-be-visualized-with-introspect-1",
    "page": "Home",
    "title": "Trees can be visualized with introspect",
    "category": "section",
    "text": "A useful function for visualizing the graphic that you've constructed is introspect. It takes a Context defining a graphic and returns a new graphic with a schematic of the tree.using Compose # hide\nset_default_graphic_size(6cm, 6cm) # hide\n\ntomato_bisque =\n    compose(context(),\n            (context(), circle(), fill(\"bisque\")),\n            (context(), rectangle(), fill(\"tomato\")))\n\nintrospect(tomato_bisque)This is a little cryptic, but you can use this limited edition decoder ring:using Compose, Colors, Measures # hide\nset_default_graphic_size(6cm, 4cm) # hide\n\nfigsize = 6mm\nt = table(3, 2, 1:3, 2:2, y_prop=[1.0, 1.0, 1.0])\nt[1,1] = [compose(context(minwidth=figsize + 2mm, minheight=figsize),\n                  circle(0.5, 0.5, figsize/2), fill(LCHab(92, 10, 77)))]\nt[2,1] = [compose(context(minwidth=figsize + 2mm, minheight=figsize),\n                  rectangle(0.5cx - figsize/2, 0.5cy - figsize/2, figsize, figsize),\n                  fill(LCHab(68, 74, 192)))]\nt[3,1] = [compose(context(minwidth=figsize + 2mm, minheight=figsize),\n                  polygon([(0.5cx - figsize/2, 0.5cy - figsize/2),\n                           (0.5cx + figsize/2, 0.5cy - figsize/2),\n                           (0.5, 0.5cy + figsize/2)]),\n                  fill(LCHab(68, 74, 29)))]\nt[1,2] = [compose(context(), text(0, 0.5, \"Context\", hleft, vcenter))]\nt[2,2] = [compose(context(), text(0, 0.5, \"Form\", hleft, vcenter))]\nt[3,2] = [compose(context(), text(0, 0.5, \"Property\", hleft, vcenter))]\ncompose(context(), t, fill(LCHab(92, 10, 77)), fontsize(10pt))"
},

{
    "location": "index.html#Contexts-specify-a-coordinate-system-for-their-children-1",
    "page": "Home",
    "title": "Contexts specify a coordinate system for their children",
    "category": "section",
    "text": "In addition to forming internal nodes to group Form and Property children, a Context can define a coordinate system using the context(x0, y0, width, height) form. Here we'll reposition some circles by composing them with contexts using different coordinate systems.using Composecomposition = compose(context(), fill(\"tomato\"),\n        (context(0.0, 0.0, 0.5, 0.5), circle()),\n        (context(0.5, 0.5, 0.5, 0.5), circle()))\ncomposition |> SVG(\"tomatos.svg\", 4cm, 4cm)\nnothing # hide(Image: )The context's box (i.e. (x0, y0, width, height)) is given in terms of its parent's coordinate system and defaults to (0, 0, 1, 1). All the children of a context will use coordinates relative to that box.This is an easy mechanism to translate the coordinates of a subtree in the graphic, but coordinates can be scaled and shifted as well by passing a UnitBox to the units attribute.composition = compose(context(),\n        (context(units=UnitBox(0, 0, 1000, 1000)),\n         polygon([(0, 1000), (500, 1000), (500, 0)]),\n         fill(\"tomato\")),\n        (context(),\n         polygon([(1, 1), (0.5, 1), (0.5, 0)]),\n         fill(\"bisque\")))\ncomposition |> SVG(\"tomato_bisque_triangle.svg\", 4cm, 4cm)\nnothing # hide(Image: )"
},

{
    "location": "index.html#Measures-can-be-a-combination-of-absolute-and-relative-units-1",
    "page": "Home",
    "title": "Measures can be a combination of absolute and relative units",
    "category": "section",
    "text": "Complex visualizations often are defined using a combination of relative and absolute units. Compose makes these easy. In fact there are three sorts of units used in Compose:Context units: If no unit is explicitly attached to a number, it is assumed to be in “context units”, which are relative to the parent Context's box and coordinate system. (Constants: cx, cy)\nWidth/Height units: Sometimes you'll want place geometry in relative coordinates, but bypassing the parent context's coordinate system. Width/height work so that (0w, 0h) is always the top-left corner of the contxt, and (1w, 1h) is always the bottom-right. (Constants: w, h)\nAbsolute units: Absolute units are inches, centimeters, points, etc. (Constants: inch, cm, mm, pt)Any linear combination of these types of units is allowed. For example: 0.5w + 2cm - 5cx is a valid measure that can be used anywhere."
},

{
    "location": "index.html#Forms-and-Properties-can-be-vectorized-1",
    "page": "Home",
    "title": "Forms and Properties can be vectorized",
    "category": "section",
    "text": "Often one needs to produce many copies of a similar shape. Most of the forms an properties have a scalar and vector forms to simplify this sort of mass production.We'll use circle as an example, which has two constructors:circle(x=0.5w, y=0.5h, r=0.5w)\ncircle(xs::AbstractArray, ys::AbstractArray, rs::AbstractArray)The first of these creates only circle centered at (x, y) with radius r. The second form can succinctly create many circles (using the Colors package to specify the LHCab colorspace):using Compose, Colorscomposition = compose(context(),\n        circle([0.25, 0.5, 0.75], [0.25, 0.5, 0.75], [0.1, 0.1, 0.1]),\n        fill(LCHab(92, 10, 77)))\ncomposition |> SVG(\"circles.svg\", 4cm, 4cm)\nnothing # hide(Image: )The arrays in passed to xs, ys, and rs need not be the same length. Shorter arrays will be cycled. This let's us shorten this last example by only specifying the radius just once.composition = compose(context(),\n        circle([0.25, 0.5, 0.75], [0.25, 0.5, 0.75], [0.1]),\n        fill(LCHab(92, 10, 77)))\ncomposition |> SVG(\"cycled_circles.svg\", 4cm, 4cm)\nnothing # hide(Image: )The fill is a property can also be vectorized here to quickly assign different colors to each circle.circles_fill_vectorized = compose(context(),\n        circle([0.25, 0.5, 0.75], [0.25, 0.5, 0.75], [0.1]),\n        fill([LCHab(92, 10, 77), LCHab(68, 74, 192), LCHab(78, 84, 29)]))\ncircles_fill_vectorized |> SVG(\"circles_fill_vectorized.svg\", 4cm, 4cm)\nnothing # hide(Image: )If vector properties are used with vector forms, they must be of equal length."
},

{
    "location": "index.html#Compose-can-produce-arbitrary-directed-graphs-1",
    "page": "Home",
    "title": "Compose can produce arbitrary directed graphs",
    "category": "section",
    "text": "Though we've so far explained compose as producing trees, there's nothing stopping one from producing an arbitrary directed graph. This can be quite useful in some cases.In this example, only one triangle object is ever initialized, despite many triangles being drawn, which is possible because the graph produced by siepinski is not a tree. The triangle polygon has many parent nodes than “re-contextualize” that triangle by repositioning it.using Compose, Colors # hide\nset_default_graphic_size(8cm, 8*(sqrt(3)/2)*cm) # hide\n\nfunction sierpinski(n)\n    if n == 0\n        compose(context(), polygon([(1,1), (0,1), (1/2, 0)]))\n    else\n        t = sierpinski(n - 1)\n        compose(context(),\n                (context(1/4,   0, 1/2, 1/2), t),\n                (context(  0, 1/2, 1/2, 1/2), t),\n                (context(1/2, 1/2, 1/2, 1/2), t))\n    end\nend\n\ncomposition = compose(sierpinski(6), fill(LCHab(92, 10, 77)))\ncomposition |> SVG(\"sierpinski.svg\", 8cm, 8*(sqrt(3)/2)*cm)\nnothing # hide(Image: )There are no safeguards to check for cycles. You can produce a graph with a cycle and Compose will run in an infinite loop trying to draw it. In most applications, this isn't a concern."
},

{
    "location": "index.html#Fancier-compositions-1",
    "page": "Home",
    "title": "Fancier compositions",
    "category": "section",
    "text": "There are fancier forms of the compose function, in particular, variadic compose, which is roughly defined as:compose(a, b, cs...) = compose(compose(a, b), cs...)Compose over tuples or arrays:compose((as...)) = compose(as...)In effect, this lets one write a complex series of compose operations as an S-expression. For example:compose(a, b, ((c, d), (e, f), g))Since all we are doing is building trees, this syntax tends to be pretty convenient."
},

{
    "location": "index.html#Forms-1",
    "page": "Home",
    "title": "Forms",
    "category": "section",
    "text": "These are basic constructors for the in-built forms - see src/form.jl for more constructors.polygon(points)\nrectangle(x0, y0, width, height)\ncircle(x, y, r)\nellipse(x, y, x_radius, y_radius)\ntext(x, y, value)\nline(points)\ncurve(anchor0, ctrl0, ctrl1, anchor1)\nbitmap(mime, data, x0, y0, width, height)"
},

{
    "location": "index.html#Coordinates-1",
    "page": "Home",
    "title": "Coordinates",
    "category": "section",
    "text": "Besides coordinate transformations, Compose also handles mixtures of relative and absolute coordinates. For example, 1w - 10mm is a well formed expression, giving the width of the parent canvas minus ten millimeters."
},

{
    "location": "index.html#Text-1",
    "page": "Home",
    "title": "Text",
    "category": "section",
    "text": "Symbols can be used in text strings by inserting HTML codes.  More general formatting for the SVG backend is documented here, whereas the Cairo backend uses a Pango markup language.using Compose # hide\ncents_ina_dollar = compose(context(), text(0.5, 0.5,\"100&#162; in a &#36;\"))\ncents_ina_dollar |> SVG(\"dollar.svg\",5cm,1cm)\nnothing # hide(Image: )"
},

{
    "location": "index.html#Influences-1",
    "page": "Home",
    "title": "Influences",
    "category": "section",
    "text": "Compose is intended as a futuristic version of the R library grid, and so takes a few ideas from grid. The Compose canvas is roughly equivalent to a viewport in grid, for example. Compose was also inspired by the admirable Haskell library Diagrams."
},

{
    "location": "library.html#Compose.circle",
    "page": "Library",
    "title": "Compose.circle",
    "category": "Function",
    "text": "circle(xs, ys, rs)\n\nArguments can be passed in arrays in order to perform multiple drawing operations.\n\n\n\n"
},

{
    "location": "library.html#Compose.circle",
    "page": "Library",
    "title": "Compose.circle",
    "category": "Function",
    "text": "circle(x, y, r)\n\nDefine a circle with its center at (x,y) and a radius of r.\n\n\n\n"
},

{
    "location": "library.html#Compose.circle-Tuple{}",
    "page": "Library",
    "title": "Compose.circle",
    "category": "Method",
    "text": "circle()\n\nDefine a circle in the center of the current context with a diameter equal to the width of the context.\n\n\n\n"
},

{
    "location": "library.html#Compose.polygon",
    "page": "Library",
    "title": "Compose.polygon",
    "category": "Function",
    "text": "polygon(points)\n\nDefine a polygon. points is an array of (x,y) tuples that specify the corners of the polygon.\n\n\n\n"
},

{
    "location": "library.html#Compose.rectangle",
    "page": "Library",
    "title": "Compose.rectangle",
    "category": "Function",
    "text": "rectangle(x0, y0, width, height)\n\nDefine a rectangle of size widthxheight with its top left corner at the point (x, y).\n\n\n\n"
},

{
    "location": "library.html#Compose.rectangle",
    "page": "Library",
    "title": "Compose.rectangle",
    "category": "Function",
    "text": "rectangle(x0s, y0s, widths, heights)\n\nArguments can be passed in arrays in order to perform multiple drawing operations at once.\n\n\n\n"
},

{
    "location": "library.html#Compose.rectangle-Tuple{}",
    "page": "Library",
    "title": "Compose.rectangle",
    "category": "Method",
    "text": "rectangle()\n\nDefine a rectangle that fills the current context completely.\n\n\n\n"
},

{
    "location": "library.html#Compose.text",
    "page": "Library",
    "title": "Compose.text",
    "category": "Function",
    "text": "text(x, y, value [,halgin::HAlignment [,valgin::VAlignment [,rot::Rotation]]])\n\nDraw the text value at the position (x,y) relative to the current context.\n\nThe default alignment of the text is hleft vbottom. The vertical and horizontal alignment is specified by passing hleft, hcenter or hright and vtop, vcenter or vbottom as values for halgin and valgin respectively.\n\n\n\n"
},

{
    "location": "library.html#Compose.text",
    "page": "Library",
    "title": "Compose.text",
    "category": "Function",
    "text": "text(xs, ys, values [,halgins::HAlignment [,valgins::VAlignment [,rots::Rotation]]])\n\nArguments can be passed in arrays in order to perform multiple drawing operations at once.\n\n\n\n"
},

{
    "location": "library.html#Compose.FormBatch",
    "page": "Library",
    "title": "Compose.FormBatch",
    "category": "Type",
    "text": "A form batch is a vectorized form with n primitives transformed into a simpler representation: one primitive repositioned n times.\n\nOn certain backends this leads to more efficient drawing. For example, SVG can be shortened by using <def> and <use> tags, and raster graphics can render the form primitive to a back buffer and blit it into place for faster drawing.\n\nBatching is an optimization transform that happens at draw time. There's currently no mechanism to manually batch. E.g. contexts cannot have FormBatch children.\n\n\n\n"
},

{
    "location": "library.html#Compose.batch-Tuple{Compose.Form{P}}",
    "page": "Library",
    "title": "Compose.batch",
    "category": "Method",
    "text": "Attempt to batch a form. Return a Nullable{FormBatch} which is null if the Form could not be batched, and non-null if the original form can be replaced with teh resulting FormBatch.\n\n\n\n"
},

{
    "location": "library.html#Compose.canbatch-Tuple{Compose.Backend}",
    "page": "Library",
    "title": "Compose.canbatch",
    "category": "Method",
    "text": "Some backends can more efficiently draw forms by batching. If so, they shuld define a similar method that returns true.\n\n\n\n"
},

{
    "location": "library.html#Compose.count_unique_primitives-Tuple{Compose.Property,Int64}",
    "page": "Library",
    "title": "Compose.count_unique_primitives",
    "category": "Method",
    "text": "Count the number of unique primitives in a property, stopping when max_count is exceeded.\n\n\n\n"
},

{
    "location": "library.html#Compose.excise_vector_children!-Tuple{Compose.Context}",
    "page": "Library",
    "title": "Compose.excise_vector_children!",
    "category": "Method",
    "text": "Remove and return vector forms and vector properties from the Context.\n\n\n\n"
},

{
    "location": "library.html#Compose.filter_redundant_offsets!-Tuple{Array{Tuple{Measures.Length{:mm,Float64},Measures.Length{:mm,Float64}},1}}",
    "page": "Library",
    "title": "Compose.filter_redundant_offsets!",
    "category": "Method",
    "text": "Produce a new array of offsets in which near duplicate values have been removed.\n\n\n\n"
},

{
    "location": "library.html#Compose.optimize_batching-Tuple{Compose.Context}",
    "page": "Library",
    "title": "Compose.optimize_batching",
    "category": "Method",
    "text": "Attempt to transform a tree into an equivalent tree that can more easily be batched.\n\nWhat this does is look for patterns in which a long vector form is accompanied by a large vector property that has a relatively small number of unique values. If there are n unique values, we can split it into n contexts, each with a shorter vector form and only scalar properties.\n\n\n\n"
},

{
    "location": "library.html#",
    "page": "Library",
    "title": "Library",
    "category": "page",
    "text": "Modules = [Compose]"
},

]}
