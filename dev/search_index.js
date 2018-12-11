var documenterSearchIndex = {"docs": [

{
    "location": "#",
    "page": "Home",
    "title": "Home",
    "category": "page",
    "text": "Author = [\"Daniel C. Jones\", \"Gio Borje\", \"Tamas Nagy\"]"
},

{
    "location": "#Compose-1",
    "page": "Home",
    "title": "Compose",
    "category": "section",
    "text": "Compose is a declarative vector graphics system written in Julia. It\'s designed to simplify the creation of complex graphics and serves as the basis of the Gadfly data visualization package."
},

{
    "location": "#Package-features-1",
    "page": "Home",
    "title": "Package features",
    "category": "section",
    "text": "Renders publication quality graphics to SVG, PNG, Postscript, PDF and PGF\nIntuitive and consistent interface\nWorks with Jupyter notebooks via IJulia out of the box"
},

{
    "location": "#Installation-1",
    "page": "Home",
    "title": "Installation",
    "category": "section",
    "text": "The latest release of Compose can be installed from the Julia REPL prompt withjulia> Pkg.add(\"Compose\")This installs the package and any missing dependencies.  From there, the simplest of graphics can be rendered to your default internet browser withjulia> using Compose\njulia> compose(context(), circle(), fill(\"gold\"))Now that you have it installed, check out the Tutorial and the Forms gallery."
},

{
    "location": "#Influences-1",
    "page": "Home",
    "title": "Influences",
    "category": "section",
    "text": "Compose is intended as a futuristic version of the R library grid, and so takes a few ideas from grid. The Compose canvas is roughly equivalent to a viewport in grid, for example. Compose was also inspired by the admirable Haskell library Diagrams."
},

{
    "location": "tutorial/#",
    "page": "Tutorial",
    "title": "Tutorial",
    "category": "page",
    "text": "Author = [\"Daniel C. Jones\", \"Gio Borje\", \"Tamas Nagy\"]"
},

{
    "location": "tutorial/#Tutorial-1",
    "page": "Tutorial",
    "title": "Tutorial",
    "category": "section",
    "text": ""
},

{
    "location": "tutorial/#Compose-is-declarative-1",
    "page": "Tutorial",
    "title": "Compose is declarative",
    "category": "section",
    "text": "In a declarative graphics system, a figure is built without specifying the precise sequence of drawing commands but by arranging shapes and attaching properties. This makes it easy to break a complex graphic into manageable parts and then figure out how to combine the parts."
},

{
    "location": "tutorial/#Everything-is-a-tree-1",
    "page": "Tutorial",
    "title": "Everything is a tree",
    "category": "section",
    "text": "Graphics in Compose are defined using a tree structure. It\'s not unlike SVG in this regard, but has simpler semantics. There are three important types that make up the nodes of the tree:Context: An internal node.\nForm: A leaf node that defines some geometry, like a line or a polygon.\nProperty: A leaf node that modifies how its parent\'s subtree is drawn, like fill color, font family, or line width.The all-important function in Compose, is called, not surprisingly, compose. Calling compose(a, b) will return a new tree rooted at a and with b attached as a child.That\'s enough to start drawing some simple shapes.using Compose\nset_default_graphic_size(4cm, 4cm)\nset_default_jsmode(:exclude)using Compose\n\ncomposition = compose(compose(context(), rectangle()), fill(\"tomato\"))\ndraw(SVG(\"tomato.svg\", 4cm, 4cm), composition)\nnothing # hide(Image: )The last line renders the composition to specificied backend, here the SVG backend. This can also be written like composition |> SVG(\"tomato.svg\", 4cm, 4cm). Alternatively, if multiple compositions of the same size are to be generated, this can be abbreviated even further toset_default_graphic_size(4cm, 4cm)\ncomposition |> SVG(\"tomato.svg\")\ncomposition2 |> SVG(\"celery.svg\")\ncomposition3 |> SVG(\"rutabaga.svg\")  # etc..."
},

{
    "location": "tutorial/#The-compose-function-accepts-S-expressions-1",
    "page": "Tutorial",
    "title": "The compose function accepts S-expressions",
    "category": "section",
    "text": "In the first example, we had to call compose twice just to draw a lousy red square. Fortunately compose has a few tricks up its sleeve. As everyone from lisp hackers and phylogeneticists knows, trees can be defined most tersely using S-expressions. We can rewrite our first example like:# equivalent to compose(compose(context(), rectangle()), fill(\"tomato\")))\ncompose(context(), rectangle(), fill(\"tomato\"))Furthermore, more complex trees can be formed by grouping subtrees with parenthesis or brackets.composition = compose(context(),\n        (context(), circle(), fill(\"bisque\")),\n        (context(), rectangle(), fill(\"tomato\")))\ncomposition |> SVG(\"tomato_bisque.svg\")\nnothing # hide(Image: )"
},

{
    "location": "tutorial/#Trees-can-be-visualized-with-introspect-1",
    "page": "Tutorial",
    "title": "Trees can be visualized with introspect",
    "category": "section",
    "text": "A useful function for visualizing the graphic that you\'ve constructed is introspect. It takes a Context defining a graphic and returns a new graphic with a schematic of the tree.using Compose # hide\nset_default_graphic_size(6cm, 6cm) # hide\n\ntomato_bisque =\n    compose(context(),\n            (context(), circle(), fill(\"bisque\")),\n            (context(), rectangle(), fill(\"tomato\")))\n\nintrospect(tomato_bisque)This is a little cryptic, but you can use this limited edition decoder ring:using Compose, Colors, Measures\nset_default_graphic_size(6cm, 4cm)\n\nfigsize = 6mm\nt = table(3, 2, 1:3, 2:2, y_prop=[1.0, 1.0, 1.0])\nt[1,1] = [compose(context(minwidth=figsize + 2mm, minheight=figsize),\n                  circle(0.5, 0.5, figsize/2), fill(LCHab(92, 10, 77)))]\nt[2,1] = [compose(context(minwidth=figsize + 2mm, minheight=figsize),\n                  rectangle(0.5cx - figsize/2, 0.5cy - figsize/2, figsize, figsize),\n                  fill(LCHab(68, 74, 192)))]\nt[3,1] = [compose(context(minwidth=figsize + 2mm, minheight=figsize),\n                  polygon([(0.5cx - figsize/2, 0.5cy - figsize/2),\n                           (0.5cx + figsize/2, 0.5cy - figsize/2),\n                           (0.5cx, 0.5cy + figsize/2)]),\n                  fill(LCHab(68, 74, 29)))]\nt[1,2] = [compose(context(), text(0, 0.5, \"Context\", hleft, vcenter))]\nt[2,2] = [compose(context(), text(0, 0.5, \"Form\", hleft, vcenter))]\nt[3,2] = [compose(context(), text(0, 0.5, \"Property\", hleft, vcenter))]\ncompose(context(), t, fill(LCHab(92, 10, 77)), fontsize(10pt))"
},

{
    "location": "tutorial/#Contexts-specify-a-coordinate-system-for-their-children-1",
    "page": "Tutorial",
    "title": "Contexts specify a coordinate system for their children",
    "category": "section",
    "text": "In addition to forming internal nodes to group Form and Property children, a Context can define a coordinate system using the context(x0, y0, width, height) form. Here we\'ll reposition some circles by composing them with contexts using different coordinate systems.using Compose\nset_default_graphic_size(4cm, 4cm)composition = compose(context(), fill(\"tomato\"),\n        (context(0.0, 0.0, 0.5, 0.5), circle()),\n        (context(0.5, 0.5, 0.5, 0.5), circle()))\ncomposition |> SVG(\"tomatos.svg\")\nnothing # hide(Image: )The context\'s box (i.e. (x0, y0, width, height)) is given in terms of its parent\'s coordinate system and defaults to (0, 0, 1, 1). All the children of a context will use coordinates relative to that box.This is an easy mechanism to translate the coordinates of a subtree in the graphic, but coordinates can be scaled and shifted as well by passing a UnitBox to the units attribute.composition = compose(context(),\n        (context(units=UnitBox(0, 0, 1000, 1000)),\n         polygon([(0, 1000), (500, 1000), (500, 0)]),\n         fill(\"tomato\")),\n        (context(),\n         polygon([(1, 1), (0.5, 1), (0.5, 0)]),\n         fill(\"bisque\")))\ncomposition |> SVG(\"tomato_bisque_triangle.svg\")\nnothing # hide(Image: )"
},

{
    "location": "tutorial/#Measures-can-be-a-combination-of-absolute-and-relative-units-1",
    "page": "Tutorial",
    "title": "Measures can be a combination of absolute and relative units",
    "category": "section",
    "text": "Complex visualizations often are defined using a combination of relative and absolute units. Compose makes these easy. In fact there are three sorts of units used in Compose:Context units: If no unit is explicitly attached to a number, it is assumed to be in “context units”, which are relative to the parent Context\'s box and coordinate system. (Constants: cx, cy)\nWidth/Height units: Sometimes you\'ll want place geometry in relative coordinates, but bypassing the parent context\'s coordinate system. Width/height work so that (0w, 0h) is always the top-left corner of the contxt, and (1w, 1h) is always the bottom-right. (Constants: w, h)\nAbsolute units: Absolute units are inches, centimeters, points, etc. (Constants: inch, cm, mm, pt)Any linear combination of these types of units is allowed. For example: 0.5w + 2cm - 5cx is a valid measure that can be used anywhere."
},

{
    "location": "tutorial/#Forms-and-Properties-can-be-vectorized-1",
    "page": "Tutorial",
    "title": "Forms and Properties can be vectorized",
    "category": "section",
    "text": "Often one needs to produce many copies of a similar shape. Most of the forms an properties have a scalar and vector forms to simplify this sort of mass production.We\'ll use circle as an example, which has two constructors:circle(x=0.5w, y=0.5h, r=0.5w)\ncircle(xs::AbstractArray, ys::AbstractArray, rs::AbstractArray)The first of these creates only circle centered at (x, y) with radius r. The second form can succinctly create many circles (using the Colors package to specify the LHCab colorspace):using Compose, Colors\nset_default_graphic_size(4cm, 4cm)composition = compose(context(),\n        circle([0.25, 0.5, 0.75], [0.25, 0.5, 0.75], [0.1, 0.1, 0.1]),\n        fill(LCHab(92, 10, 77)))\ncomposition |> SVG(\"circles.svg\")\nnothing # hide(Image: )The arrays in passed to xs, ys, and rs need not be the same length. Shorter arrays will be cycled. This let\'s us shorten this last example by only specifying the radius just once.composition = compose(context(),\n        circle([0.25, 0.5, 0.75], [0.25, 0.5, 0.75], [0.1]),\n        fill(LCHab(92, 10, 77)))\ncomposition |> SVG(\"cycled_circles.svg\")\nnothing # hide(Image: )The fill is a property can also be vectorized here to quickly assign different colors to each circle.circles_fill_vectorized = compose(context(),\n        circle([0.25, 0.5, 0.75], [0.25, 0.5, 0.75], [0.1]),\n        fill([LCHab(92, 10, 77), LCHab(68, 74, 192), LCHab(78, 84, 29)]))\ncircles_fill_vectorized |> SVG(\"circles_fill_vectorized.svg\")\nnothing # hide(Image: )If vector properties are used with vector forms, they must be of equal length."
},

{
    "location": "tutorial/#Compose-can-produce-arbitrary-directed-graphs-1",
    "page": "Tutorial",
    "title": "Compose can produce arbitrary directed graphs",
    "category": "section",
    "text": "Though we\'ve so far explained compose as producing trees, there\'s nothing stopping one from producing an arbitrary directed graph. This can be quite useful in some cases.In this example, only one triangle object is ever initialized, despite many triangles being drawn, which is possible because the graph produced by siepinski is not a tree. The triangle polygon has many parent nodes than “re-contextualize” that triangle by repositioning it.using Compose, Colors # hide\nset_default_graphic_size(8cm, 8*(sqrt(3)/2)*cm) # hide\n\nfunction sierpinski(n)\n    if n == 0\n        compose(context(), polygon([(1,1), (0,1), (1/2, 0)]))\n    else\n        t = sierpinski(n - 1)\n        compose(context(),\n                (context(1/4,   0, 1/2, 1/2), t),\n                (context(  0, 1/2, 1/2, 1/2), t),\n                (context(1/2, 1/2, 1/2, 1/2), t))\n    end\nend\n\ncomposition = compose(sierpinski(6), fill(LCHab(92, 10, 77)))\ncomposition |> SVG(\"sierpinski.svg\", 8cm, 8*(sqrt(3)/2)*cm)\nnothing # hide(Image: )There are no safeguards to check for cycles. You can produce a graph with a cycle and Compose will run in an infinite loop trying to draw it. In most applications, this isn\'t a concern."
},

{
    "location": "tutorial/#Fancier-compositions-1",
    "page": "Tutorial",
    "title": "Fancier compositions",
    "category": "section",
    "text": "There are fancier forms of the compose function, in particular, variadic compose, which is roughly defined as:compose(a, b, cs...) = compose(compose(a, b), cs...)Compose over tuples or arrays:compose((as...)) = compose(as...)In effect, this lets one write a complex series of compose operations as an S-expression. For example:compose(a, b, ((c, d), (e, f), g))Since all we are doing is building trees, this syntax tends to be pretty convenient."
},

{
    "location": "tutorial/#[Forms](@ref-forms_gallery)-1",
    "page": "Tutorial",
    "title": "Forms",
    "category": "section",
    "text": "These are basic constructors for the in-built forms - see the Forms gallery for examples.polygon(points)\nrectangle(x0, y0, width, height)\ncircle(x, y, r)\nellipse(x, y, x_radius, y_radius)\ntext(x, y, value)\nline(points)\ncurve(anchor0, ctrl0, ctrl1, anchor1)\nbitmap(mime, data, x0, y0, width, height)\narc(x, y, r, angle1, angle2, sector)\nsector(x, y, r, angle1, angle2)"
},

{
    "location": "tutorial/#Coordinates-1",
    "page": "Tutorial",
    "title": "Coordinates",
    "category": "section",
    "text": "Besides coordinate transformations, Compose also handles mixtures of relative and absolute coordinates. For example, 1w - 10mm is a well formed expression, giving the width of the parent canvas minus ten millimeters."
},

{
    "location": "tutorial/#Text-1",
    "page": "Tutorial",
    "title": "Text",
    "category": "section",
    "text": "Symbols can be used in text strings by inserting HTML codes.  More general formatting for the SVG backend is documented here, whereas the Cairo backend uses a Pango markup language.using Compose # hide\ncents_ina_dollar = compose(context(), text(0.5, 0.5,\"100&#162; in a &#36;\"))\ncents_ina_dollar |> SVG(\"dollar.svg\",5cm,1cm)\nnothing # hide(Image: )Use the font and fontsize properties to change the appearance of type:using Compose # hide\ncompose(context(),\n       (context(), text(0.2,0.5,\"big\"), fontsize(18pt)),\n       (context(), text(0.4,0.5,\"small\"), fontsize(6pt)),\n       (context(), text(0.6,0.5,\"bold\"), font(\"Helvetica-Bold\")),\n       (context(), text(0.8,0.5,\"oblique\"), font(\"Helvetica-Oblique\"))) |>\n    SVG(\"font_fontsize.svg\",15cm,1cm)\nnothing # hide(Image: )"
},

{
    "location": "gallery/forms/#",
    "page": "Forms",
    "title": "Forms",
    "category": "page",
    "text": "Author = [\"Mattriks\"]"
},

{
    "location": "gallery/forms/#forms_gallery-1",
    "page": "Forms",
    "title": "Forms",
    "category": "section",
    "text": ""
},

{
    "location": "gallery/forms/#[arc](@ref)-1",
    "page": "Forms",
    "title": "arc",
    "category": "section",
    "text": "using Compose\nset_default_graphic_size(8cm,4cm)\ncolv = [\"red\",\"orange\",\"green\",\"blue\", \"purple\"]\na = range(-π/4, stop=7π/4, length=6)+ 0.2*randn(6)\na[6] = a[1]\n\nimg1 = compose(context(),\n    (context(), arc([0.5], [0.5], [0.3], [4.5π/4,π/4] , [7.5π/4,3π/4], [true,false]),\n        stroke(\"black\"), fill([\"red\",\"white\"])) )\nimg2 = compose(context(),\n    (context(), arc([0.5], [0.5], [0.3], a[1:5], a[2:6]), \n        stroke(colv), fill(\"transparent\"), linewidth(2mm)) )\nhstack(img1, img2)"
},

{
    "location": "gallery/forms/#[bitmap](@ref)-1",
    "page": "Forms",
    "title": "bitmap",
    "category": "section",
    "text": "using Compose\nset_default_graphic_size(14cm,4cm)\nrawimg = read(joinpath(@__DIR__,\"..\",\"assets/smiley.png\"));\nX = 0.9*rand(10,2)\n\nimg = compose(context(), \n    (context(), rectangle(), fill(\"transparent\"), stroke(\"orange\")),\n    (context(), bitmap([\"image/png\"], [rawimg], X[:,1], X[:,2], [0.1], [0.1]))\n)"
},

{
    "location": "gallery/forms/#[circle](@ref)-1",
    "page": "Forms",
    "title": "circle",
    "category": "section",
    "text": "using Colors, Compose\nset_default_graphic_size(14cm,4cm)\ncolv = HSVA.([0:30:179;], 1, 1, 0.5)\nimg = compose(context(units=UnitBox(0,0,40,8)),\n    (context(), circle([5.0:6:35;], [4], [4]), fill(colv), stroke(\"black\"))\n)"
},

{
    "location": "gallery/forms/#[curve](@ref)-1",
    "page": "Forms",
    "title": "curve",
    "category": "section",
    "text": "using Colors, Compose\nset_default_graphic_size(14cm, 4cm)\nepoint(x) = [(x,y) for y in rand(10)]\ncpoint(t=0) = [(t+x,y) for (x,y) in zip(rand(10), 0.5*rand(10))]\ncolv = range(colorant\"blue\",stop=colorant\"orange\", length=10)\n\nimg = compose(context(units=UnitBox(0,0,2,1)),\n    (context(), curve([(0.5,1.0)], cpoint(), cpoint(), epoint(0.5)), stroke(colv)),\n    (context(), curve([(1.5,1.0)], cpoint(1), cpoint(1), epoint(1.5)), stroke(colv)) \n)"
},

{
    "location": "gallery/forms/#[ellipse](@ref)-1",
    "page": "Forms",
    "title": "ellipse",
    "category": "section",
    "text": "using Colors, Compose\nset_default_graphic_size(14cm, 4cm)\ncolv1 = HSVA.([0:30:179;], 1, 1, 0.5)\nr = 2*[1:6;]/24\n\ncolv2 = HSVA.([0:15:179;], 1, 1, 0.3)\nθ = collect(range(0, stop=1.9π, length=10))\nrl = 0.5*rand(10)\nrw = 0.3*rand(10)\nrot = Rotation.(θ, 1.5, 0.5)\nellipsef(i::Int) = ellipse(1.5, 0.5, rl[i], rw[i])\n\nimg = compose(context(units=UnitBox(0,0,2,1)),\n        (context(), ellipse(r,[0.5],r,reverse(r)), stroke(\"black\"), fill(colv1)),\n[(context(rotation=rot[i]), ellipsef(i), fill(colv2[i]), stroke(\"black\")) for i in 1:10]...\n)"
},

{
    "location": "gallery/forms/#[line](@ref)-1",
    "page": "Forms",
    "title": "line",
    "category": "section",
    "text": "using Compose\nset_default_graphic_size(10cm, 10cm)\nθ = collect(range(0, stop=2π, length=60))\npoint_array = [[(0,0.75), (x,y)] for (x,y) in zip(cos.(θ), sin.(θ))]\n\nimg = compose(context(), \n    (context(), rectangle(), fill(\"salmon\"), fillopacity(0.3)),\n    (context(0.12, 0.12, 0.76, 0.76, units=UnitBox(-1,-1,2,2)),  \n        line(point_array), stroke(\"gold\"), linewidth(1mm))\n)"
},

{
    "location": "gallery/forms/#[ngon](@ref),-[star](@ref),-[xgon](@ref)-1",
    "page": "Forms",
    "title": "ngon, star, xgon",
    "category": "section",
    "text": "using Compose\nset_default_graphic_size(14cm, 5cm)\nrainbow = [\"orange\",\"green\",\"indigo\",\n    \"darkviolet\",\"indigo\",\"blue\",\"green\",\"yellow\",\"orange\",\"red\"]\nproperties = [fillopacity(0.5), fill(rainbow), stroke(\"black\")]\nnpoints = [7,5,3,2,3,4,5,6,7,8]\nX = range(0.06, stop=0.94, length=10)\nradii = 0.035*[-ones(3); ones(7)]\np = compose(context(),\n    (context(), ngon(X, [0.16], radii, npoints),\n        star(X, [0.5], radii, npoints),\n        xgon(X, [0.84], radii, npoints), properties...))"
},

{
    "location": "gallery/forms/#[polygon](@ref)-1",
    "page": "Forms",
    "title": "polygon",
    "category": "section",
    "text": "using Statistics, Compose\nset_default_graphic_size(10cm,10cm)\nX = randn(50,2)\nX = 0.3*(X .- mean(X,dims=1))./std(X,dims=1)\nhp = hypot.(X[:,1],X[:,2])\ni = hp .> Statistics.quantile(hp, 0.82)\nZ = X[i,:]\nθ = atan.(Z[:,1], Z[:,2])\nord = sortperm(θ)\npolypoints = [(x,y) for (x,y) in zip(Z[ord,1],Z[ord,2])]  \n\nimg = compose(context(units=UnitBox(-1,-1, 2,2)),\n  (context(), line([(-1,-1), (-1,1), (1,1)]), stroke(\"black\")),\n  (context(), circle(0,0,0.02), fill(\"red\")),\n  (context(), circle(X[:,1],X[:,2],[0.02]), fill(\"transparent\"),stroke(\"deepskyblue\")),\n  (context(), polygon(polypoints), fill(\"red\"), fillopacity(0.1))\n)"
},

{
    "location": "gallery/forms/#[rectangle](@ref)-1",
    "page": "Forms",
    "title": "rectangle",
    "category": "section",
    "text": "using Colors, Compose\nset_default_graphic_size(14cm,4cm)\ncolv = HSVA.([0:15:179;], 1, 1, 0.3)\nX = 0.9*rand(10,2)\nrl = 0.3*rand(10).+0.03\nrw = 0.3*rand(10).+0.03\n\nimg = compose(context(), \n    (context(), rectangle(), fill(\"transparent\"), stroke(\"orange\")),\n     (context(), rectangle(X[:,1], X[:,2], rl, rw), fill(colv), stroke(\"black\")) \n)"
},

{
    "location": "gallery/forms/#[sector](@ref)-1",
    "page": "Forms",
    "title": "sector",
    "category": "section",
    "text": "using Compose\nset_default_graphic_size(14cm, 4cm)\ncolv = [\"red\",\"orange\",\"green\",\"blue\", \"purple\"]\na = range(-π/4, stop=7π/4, length=6)+ 0.2*randn(6)\na[6] = a[1]\n\nsectorobj = sector([0.5], [0.5], [0.3], a[1:5], a[2:6])\nimg1 = compose(context(),\n    (context(), sectorobj, fill(colv)) )\nimg2 = compose(context(),\n    (context(), sectorobj, stroke(\"white\"), fill(colv), linewidth(1.4mm)) )\nimg3 = compose(context(),\n    (context(), sectorobj, stroke(colv), fill(\"transparent\"), linewidth(1.4mm)) )\nhstack(img1, img2, img3)"
},

{
    "location": "gallery/forms/#[text](@ref)-1",
    "page": "Forms",
    "title": "text",
    "category": "section",
    "text": "using Colors, Compose\nset_default_graphic_size(10cm,10cm)\nlabels=rand(string.(names(Base)[280:end]), 30)\nθ = collect(range(0, stop=58π/30, length=30))\nX = 1 .+ 0.7*[cos.(θ) sin.(θ)]\ncolv = range(colorant\"blue\",stop=colorant\"orange\", length=30)\nrot = Rotation.(θ, X[:,1], X[:,2])\n\nimg = compose(context(units=UnitBox(0,0,2,2)),\n  (context(), text(1, 1, \"Julia\", hcenter, vcenter), stroke(\"red\"), fontsize(30pt)),\n  (context(), text(X[:,1], X[:,2], labels, [hcenter], [vcenter], rot), stroke(colv))\n)"
},

{
    "location": "gallery/properties/#",
    "page": "Properties",
    "title": "Properties",
    "category": "page",
    "text": "Author = [\"Mattriks\"]"
},

{
    "location": "gallery/properties/#properties_gallery-1",
    "page": "Properties",
    "title": "Properties",
    "category": "section",
    "text": ""
},

{
    "location": "gallery/properties/#[arrow](@ref)-1",
    "page": "Properties",
    "title": "arrow",
    "category": "section",
    "text": "using Compose\nset_default_graphic_size(14cm,5cm)\nθ, r = 2π*rand(3),  0.1.+0.08*rand(3)\nc, s = r.*cos.(θ), r.*sin.(θ)\npoint_array = [[(0.5,0.5), 0.5.+(x,y)] for (x,y) in zip(c,s) ]\nimg = compose(context(), arrow(), stroke(\"black\"), fill(nothing),\n        (context(), arc(0.18, 0.5, 0.08, -π/4, 1π)),\n        (context(), line(point_array), stroke([\"red\",\"green\",\"deepskyblue\"])),\n        (context(), curve((0.7,0.5), (0.8,-0.5), (0.8,1.5), (0.9,0.5)))\n)"
},

{
    "location": "gallery/properties/#[fill](@ref),-[fillopacity](@ref)-1",
    "page": "Properties",
    "title": "fill, fillopacity",
    "category": "section",
    "text": "using Compose\nset_default_graphic_size(14cm,4cm)\nimg = compose(context(),\n  (context(), circle(0.5, 0.5, 0.08), fillopacity(0.3), fill(\"orange\")),\n  (context(), circle([0.1, 0.26], [0.5], [0.1]), fillopacity(0.3), fill(\"blue\")),\n  (context(), circle([0.42, 0.58], [0.5], [0.1]), fillopacity(0.3), fill([\"yellow\",\"green\"])),\n  (context(), circle([0.74, 0.90], [0.5], [0.1]), fillopacity([0.5,0.3]), fill([\"yellow\",\"red\"]) )     \n)"
},

{
    "location": "gallery/transforms/#",
    "page": "Transformations",
    "title": "Transformations",
    "category": "page",
    "text": "Author = [\"Mattriks\"]"
},

{
    "location": "gallery/transforms/#transforms_gallery-1",
    "page": "Transformations",
    "title": "Transformations",
    "category": "section",
    "text": ""
},

{
    "location": "gallery/transforms/#[Mirror](@ref)-1",
    "page": "Transformations",
    "title": "Mirror",
    "category": "section",
    "text": "using Compose\nset_default_graphic_size(15cm,5cm)\n\nf_points = [(.1, .1), (.9, .1), (.9, .2), (.2, .2), (.2, .4), (.6, .4), (.6, .5),\n    (.2, .5), (.2, .9), (.1, .9), (.1, .1)]\nf_points = (x->0.4.*x.+0.1).(f_points)\nfpoly(ϕ::Float64) = (context(rotation=Rotation(ϕ,0.3,0.46)), polygon(f_points))\n\nimgfa(θ, ϕ=0.0, x=0.5,y=0.5) = compose(context(), \n  fill(\"salmon\"), fillopacity(1.0), fpoly(ϕ),\n  (context(rotation=Rotation(θ,x,y)), line([(x-0.5,y),(x+0.5,y)]), circle(x,y, 0.02)),\n  (context(mirror=Mirror(θ, x, y)), fpoly(ϕ))\n)\n\nFmir = hstack(imgfa(-π/4), imgfa(-π/2.2), imgfa(π/4, 1π))\nimg = compose(context(), rectangle(), fill(nothing), stroke(\"black\"), Fmir)"
},

{
    "location": "gallery/transforms/#[Rotation](@ref)-1",
    "page": "Transformations",
    "title": "Rotation",
    "category": "section",
    "text": "using Compose\nset_default_graphic_size(15cm,5cm)\n\n# This example also illustrates nested contexts\nf_points = [(.1, .1), (.9, .1), (.9, .2), (.2, .2), (.2, .4),\n    (.6, .4), (.6, .5), (.2, .5), (.2, .9), (.1, .9), (.1, .1)]\nrect(c::String) = (context(), rectangle(), stroke(c))\ncirc(c::String, s::Float64=0.4) =  (context(), circle([x],[y],[0.03,s]), stroke(c))\nfpoly(c::String) = (context(), polygon(f_points), fill(c), fillopacity(1.0))\ncontextC(θ::Float64) = (context(0.5,0.5,1.5,1.5, units=UnitBox(0,0,1,1),\n        rotation=Rotation(θ,x,y)), fpoly(\"steelblue\"), circ(\"orange\"))\nimgf(θ::Float64) =  compose(context(),\n        (context(0.15, 0.15, 0.7, 0.7, units=UnitBox(0,0,2,2)), rect(\"red\"),\n        contextC(θ)) # context C in context B in context A\n    )\n\nx, y, θ = 0.5, 0.25, π/3\nFrot =  hstack(imgf(-θ), imgf(0.), imgf(θ))\nimg = compose(context(), rectangle(), fill(nothing), stroke(\"black\"), Frot)"
},

{
    "location": "gallery/transforms/#[Shear](@ref)-1",
    "page": "Transformations",
    "title": "Shear",
    "category": "section",
    "text": "using Compose\nset_default_graphic_size(15cm, 5cm)\n\nf_points = [(.1, .1), (.9, .1), (.9, .2), (.2, .2), (.2, .4), (.6, .4),\n    (.6, .5), (.2, .5), (.2, .9), (.1, .9), (.1, .1)]\nf_points = (x->0.5.*x.+0.3).(f_points)\nctxl(θ, x, y) = (context(rotation=Rotation(θ, x, y)), circle(x,y, 0.01), \n    line([(x-0.5,y),(x+0.5,y)]))\nfpoly(c::String) = (context(),  polygon(f_points), fill(c) )\nctxf(θ, ϕ, s, x, y,c) = (context(rotation=Rotation(-θ, x, y), \n        shear=Shear(s, ϕ, x, y)), fpoly(c))\n\nx, y, θ  = 0.5, 0.5, -π/6\nimg1 = compose(context(), stroke(\"black\"), ctxl(θ,x,y),  ctxf(0,0,0,x,y, \"yellow\"),\n    (context(), arc(x,y,0.3,π+θ,π-0.15), arrow()) )\nimg2 = compose(context(), stroke(\"black\"), ctxl(0,x,y),\n    ctxf(θ,0,1.8,x,y,\"transparent\"), ctxf(θ,0,0,x,y,\"yellow\"),\n    text(0.5, 0.1, \"x\' = x+y*shear\", hcenter, vcenter) )\nimg3 = compose(context(), stroke(\"black\"), \n    ctxl(θ, x, y), ctxf(0,θ,1.8,x,y,\"yellow\") )\n\nhstack(img1, img2, img3)"
},

{
    "location": "library/#Compose.Mirror-Tuple{Number,Any,Any}",
    "page": "Library",
    "title": "Compose.Mirror",
    "category": "method",
    "text": "Mirror(θ, x, y)\n\nMirror line passing through point (x,y) at angle θ (in radians).\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.Mirror-Tuple{Number}",
    "page": "Library",
    "title": "Compose.Mirror",
    "category": "method",
    "text": "Mirror(θ)\n\nMirror(θ)=Mirror(θ, 0.5w, 0.5h)\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.Rotation-Tuple{Number,Any,Any}",
    "page": "Library",
    "title": "Compose.Rotation",
    "category": "method",
    "text": "Rotation(θ, x, y)\n\nRotate all forms in context around point (x,y) by angle θ in radians.  \n\n\n\n\n\n"
},

{
    "location": "library/#Compose.Rotation-Tuple{Number}",
    "page": "Library",
    "title": "Compose.Rotation",
    "category": "method",
    "text": "Rotation(θ)\n\nRotation(θ)=Rotation(θ, 0.5w, 0.5h)\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.Shear-Tuple{Number,Number,Any,Any}",
    "page": "Library",
    "title": "Compose.Shear",
    "category": "method",
    "text": "Shear(s, θ, x, y)\n\nShear line passing through point (x,y) at angle θ (in radians), with shear s.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.Shear-Tuple{Number,Number}",
    "page": "Library",
    "title": "Compose.Shear",
    "category": "method",
    "text": "Shear(s, θ)\n\nShear(s, θ)=Shear(s, θ, 0.5w, 0.5h)\n\n\n\n\n\n"
},

{
    "location": "library/#Base.fill-Tuple{AbstractArray}",
    "page": "Library",
    "title": "Base.fill",
    "category": "method",
    "text": "fill(cs::AbstractArray)\n\nArguments can be passed in arrays in order to perform multiple drawing operations at once.\n\n\n\n\n\n"
},

{
    "location": "library/#Base.fill-Tuple{Union{AbstractString, Colorant}}",
    "page": "Library",
    "title": "Base.fill",
    "category": "method",
    "text": "fill(c)\n\nDefine a fill color, where c can be a Colorant or String.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.arc",
    "page": "Library",
    "title": "Compose.arc",
    "category": "function",
    "text": "arc(x, y, r, θ1, θ2, sector)\n\nDefine an arc with its center at (x,y), radius of r, between θ1 and θ2.   sector (optional) is true or false, true for a pie sector, false for an arc. Arcs are drawn clockwise from θ1 to θ2.    \n\n\n\n\n\n"
},

{
    "location": "library/#Compose.arc",
    "page": "Library",
    "title": "Compose.arc",
    "category": "function",
    "text": "arc(xs::AbstractVector, ys::AbstractVector, rs::AbstractVector, θ1s::AbstractVector, θ2s::AbstractVector, sectors::AbstractVector)\n\nArguments can be passed in arrays in order to perform multiple drawing operations.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.arrow-Tuple{AbstractArray}",
    "page": "Library",
    "title": "Compose.arrow",
    "category": "method",
    "text": "arrow(values::AbstractArray)\n\nArguments can be passed in arrays in order to perform multiple drawing operations at once.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.arrow-Tuple{Bool}",
    "page": "Library",
    "title": "Compose.arrow",
    "category": "method",
    "text": "arrow(value::Bool)\n\n`arrow()` is a property of arcs, lines and curves. The color of the arrowhead is the same as `stroke()`, but for svg the results will be browser-dependent.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.arrow-Tuple{}",
    "page": "Library",
    "title": "Compose.arrow",
    "category": "method",
    "text": "arrow()\n\n`arrow() = arrow(true)`\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.bitmap",
    "page": "Library",
    "title": "Compose.bitmap",
    "category": "function",
    "text": "bitmap(mimes::AbstractArray, datas::AbstractArray, x0s::AbstractArray, y0s::AbstractArray, widths::AbstractArray, heights::AbstractArray)\n\nArguments can be passed in arrays in order to perform multiple drawing operations.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.bitmap",
    "page": "Library",
    "title": "Compose.bitmap",
    "category": "function",
    "text": "bitmap(mime, data, x0, y0, width, height)\n\nDefine a bitmap of size widthxheight with its top left corner at the point (x, y).\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.circle",
    "page": "Library",
    "title": "Compose.circle",
    "category": "function",
    "text": "circle(xs::AbstractArray, ys::AbstractArray, rs::AbstractArray)\n\nArguments can be passed in arrays in order to perform multiple drawing operations.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.circle",
    "page": "Library",
    "title": "Compose.circle",
    "category": "function",
    "text": "circle(x, y, r)\n\nDefine a circle with its center at (x,y) and a radius of r.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.circle-Tuple{}",
    "page": "Library",
    "title": "Compose.circle",
    "category": "method",
    "text": "circle()\n\nDefine a circle in the center of the current context with a diameter equal to the width of the context.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.curve",
    "page": "Library",
    "title": "Compose.curve",
    "category": "function",
    "text": "curve(anchor0s::AbstractArray, ctrl0s::AbstractArray, ctrl1s::AbstractArray, anchor1s::AbstractArray)\n\nArguments can be passed in arrays in order to perform multiple drawing operations.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.curve",
    "page": "Library",
    "title": "Compose.curve",
    "category": "function",
    "text": "curve(anchor0, ctrl0, ctrl1, anchor1)\n\nDefine a bezier curve between anchor0 and anchor1 with control points ctrl0 and ctrl1.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.ellipse",
    "page": "Library",
    "title": "Compose.ellipse",
    "category": "function",
    "text": "ellipse(xs::AbstractArray, ys::AbstractArray, x_radiuses::AbstractArray, y_radiuses::AbstractArray)\n\nArguments can be passed in arrays in order to perform multiple drawing operations.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.ellipse",
    "page": "Library",
    "title": "Compose.ellipse",
    "category": "function",
    "text": "ellipse(x, y, x_radius, y_radius)\n\nDefine an ellipse with its center at (x,y) with radii x_radius and y_radius.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.ellipse-Tuple{}",
    "page": "Library",
    "title": "Compose.ellipse",
    "category": "method",
    "text": "ellipse()\n\nDefine an ellipse in the center of the current context with x_radius=0.5w and y_radius=0.5h.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.fillopacity-Tuple{AbstractArray}",
    "page": "Library",
    "title": "Compose.fillopacity",
    "category": "method",
    "text": "fillopacity(values::AbstractArray)\n\nArguments can be passed in arrays in order to perform multiple drawing operations at once.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.fillopacity-Tuple{Float64}",
    "page": "Library",
    "title": "Compose.fillopacity",
    "category": "method",
    "text": "fillopacity(value)\n\nDefine a fill opacity, where 0≤value≤1.  For svg, nested contexts  will inherit from parent contexts e.g. (context(), fillopacity(a), (context(), fill(c::String), circle())).\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.line",
    "page": "Library",
    "title": "Compose.line",
    "category": "function",
    "text": "line(point_arrays::AbstractArray)\n\nArguments can be passed in arrays in order to perform multiple drawing operations at once.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.line-Union{Tuple{AbstractArray{T,N} where N}, Tuple{T}, Tuple{AbstractArray{T,N} where N,Any}} where T<:Union{Tuple{Union{Number, Measure},Union{Number, Measure}}, Tuple{Vararg{Measure,N}} where N}",
    "page": "Library",
    "title": "Compose.line",
    "category": "method",
    "text": "line(points)\n\nDefine a line. points is an array of (x,y) tuples.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.ngon",
    "page": "Library",
    "title": "Compose.ngon",
    "category": "function",
    "text": "ngon(x, y, r, n::Int)\n\nDefine a n-sided polygon with its center at (x,y), and radius of r.  For an upside-down ngon, use -r.  \n\n\n\n\n\n"
},

{
    "location": "library/#Compose.ngon",
    "page": "Library",
    "title": "Compose.ngon",
    "category": "function",
    "text": "ngon(xs::AbstractVector, ys::AbstractVector, rs::AbstractVector, ns::AbstractVector{Int})\n\nArguments can be passed in arrays in order to perform multiple drawing operations at once.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.polygon",
    "page": "Library",
    "title": "Compose.polygon",
    "category": "function",
    "text": "polygon(point_arrays::AbstractArray)\n\nArguments can be passed in arrays in order to perform multiple drawing operations at once.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.polygon-Union{Tuple{AbstractArray{T,N} where N}, Tuple{T}, Tuple{AbstractArray{T,N} where N,Any}} where T<:Union{Tuple{Union{Number, Measure},Union{Number, Measure}}, Tuple{Vararg{Measure,N}} where N}",
    "page": "Library",
    "title": "Compose.polygon",
    "category": "method",
    "text": "polygon(points)\n\nDefine a polygon. points is an array of (x,y) tuples that specify the corners of the polygon.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.rectangle",
    "page": "Library",
    "title": "Compose.rectangle",
    "category": "function",
    "text": "rectangle(x0s::AbstractArray, y0s::AbstractArray, widths::AbstractArray, heights::AbstractArray)\n\nArguments can be passed in arrays in order to perform multiple drawing operations at once.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.rectangle",
    "page": "Library",
    "title": "Compose.rectangle",
    "category": "function",
    "text": "rectangle(x0, y0, width, height)\n\nDefine a rectangle of size widthxheight with its top left corner at the point (x, y).\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.rectangle-Tuple{}",
    "page": "Library",
    "title": "Compose.rectangle",
    "category": "method",
    "text": "rectangle()\n\nDefine a rectangle that fills the current context completely.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.sector-NTuple{5,AbstractArray{T,1} where T}",
    "page": "Library",
    "title": "Compose.sector",
    "category": "method",
    "text": "sector(xs::AbstractVector, ys::AbstractVector, rs::AbstractVector, θ1s::AbstractVector, θ2s::AbstractVector)\n\nArguments can be passed in arrays in order to perform multiple drawing operations.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.sector-NTuple{5,Any}",
    "page": "Library",
    "title": "Compose.sector",
    "category": "method",
    "text": "sector(x, y, r, θ1, θ2)\n\nDefine a pie sector with its center at (x,y), radius of r, between θ1 and θ2.  \n\n\n\n\n\n"
},

{
    "location": "library/#Compose.star",
    "page": "Library",
    "title": "Compose.star",
    "category": "function",
    "text": "star(x, y, r, n::Int, ratio)\n\nDefine a n-pointed star with its center at (x,y), outer radius of r, and inner radius equal to r*ratio. For an upside-down star, use -r.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.star",
    "page": "Library",
    "title": "Compose.star",
    "category": "function",
    "text": "star(xs::AbstractVector, ys::AbstractVector, rs::AbstractVector, ns::AbstractVector{Int}, ratios::AbstractVector{Float64})\n\nArguments can be passed in arrays in order to perform multiple drawing operations at once.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.text",
    "page": "Library",
    "title": "Compose.text",
    "category": "function",
    "text": "text(xs::AbstractArray, ys::AbstractArray, values::AbstractArray [,haligns::HAlignment [,valigns::VAlignment [,rots::Rotation]]])\n\nArguments can be passed in arrays in order to perform multiple drawing operations at once.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.text",
    "page": "Library",
    "title": "Compose.text",
    "category": "function",
    "text": "text(x, y, value [,halign::HAlignment [,valign::VAlignment [,rot::Rotation]]])\n\nDraw the text value at the position (x,y) relative to the current context.\n\nThe default alignment of the text is hleft vbottom. The vertical and horizontal alignment is specified by passing hleft, hcenter or hright and vtop, vcenter or vbottom as values for halgin and valgin respectively.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.xgon",
    "page": "Library",
    "title": "Compose.xgon",
    "category": "function",
    "text": "xgon(x, y, r, n::Int, ratio)\n\nDefine a cross with n arms with its center at (x,y), outer radius of r, and inner radius equal to r*ratio. For an upside-down xgon, use -r.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.xgon",
    "page": "Library",
    "title": "Compose.xgon",
    "category": "function",
    "text": "xgon(xs::AbstractVector, ys::AbstractVector, rs::AbstractVector, ns::AbstractVector{Int}, ratios::AbstractVector{Float64})\n\nArguments can be passed in arrays in order to perform multiple drawing operations at once.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.FormBatch",
    "page": "Library",
    "title": "Compose.FormBatch",
    "category": "type",
    "text": "A form batch is a vectorized form with n primitives transformed into a simpler representation: one primitive repositioned n times.\n\nOn certain backends this leads to more efficient drawing. For example, SVG can be shortened by using <def> and <use> tags, and raster graphics can render the form primitive to a back buffer and blit it into place for faster drawing.\n\nBatching is an optimization transform that happens at draw time. There\'s currently no mechanism to manually batch. E.g. contexts cannot have FormBatch children.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.batch-Union{Tuple{Form{P}}, Tuple{P}} where P",
    "page": "Library",
    "title": "Compose.batch",
    "category": "method",
    "text": "Attempt to batch a form. Return a Nothing singleton if the Form could not be batched, and FormBatch object if the original form can be replaced.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.canbatch-Tuple{Compose.Backend}",
    "page": "Library",
    "title": "Compose.canbatch",
    "category": "method",
    "text": "Some backends can more efficiently draw forms by batching. If so, they shuld define a similar method that returns true.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.count_unique_primitives-Tuple{Compose.Property,Int64}",
    "page": "Library",
    "title": "Compose.count_unique_primitives",
    "category": "method",
    "text": "Count the number of unique primitives in a property, stopping when max_count is exceeded.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.excise_vector_children!-Tuple{Context}",
    "page": "Library",
    "title": "Compose.excise_vector_children!",
    "category": "method",
    "text": "Remove and return vector forms and vector properties from the Context.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.filter_redundant_offsets!-Tuple{Array{Tuple{Measures.Length{:mm,Float64},Measures.Length{:mm,Float64}},1}}",
    "page": "Library",
    "title": "Compose.filter_redundant_offsets!",
    "category": "method",
    "text": "Produce a new array of offsets in which near duplicate values have been removed.\n\n\n\n\n\n"
},

{
    "location": "library/#Compose.optimize_batching-Tuple{Context}",
    "page": "Library",
    "title": "Compose.optimize_batching",
    "category": "method",
    "text": "Attempt to transform a tree into an equivalent tree that can more easily be batched.\n\nWhat this does is look for patterns in which a long vector form is accompanied by a large vector property that has a relatively small number of unique values. If there are n unique values, we can split it into n contexts, each with a shorter vector form and only scalar properties.\n\n\n\n\n\n"
},

{
    "location": "library/#",
    "page": "Library",
    "title": "Library",
    "category": "page",
    "text": "Modules = [Compose]"
},

]}
