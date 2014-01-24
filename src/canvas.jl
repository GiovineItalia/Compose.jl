
# Canvas: a thing upon which other things are placed.

export Canvas, Units, Rotation, canvas, deferredcanvas,
       draw, drawpart, set_unit_box, set_box


# Canvases are containers for forms and other canvases with an associated
# coordinate transform.
abstract Canvas


# An identity element for the canvas monoid.
immutable EmptyCanvas <: Canvas end
const empty_canvas = EmptyCanvas()


# A deferred canvas is a function that produces a canvas and is evaluated when
# the graphic is drawn. This is useful when the layout of the contents of a
# canvas depends on its size in absolute coordinates, which is only known, in
# general at draw time.
#
# The function `f` must have the signature:
#   f(box::BoundingBox, unit_box::BoundingBox)
#
# Where box is the bounding box in absolute coordinates of the parent canvas,
# and unit_box is its unit box.
#
# Another way to think of this: a canvas in to an expression what a
# deferredcanvas is to a macro.
#
type DeferredCanvas <: Canvas
    f::Function
    order::Integer
    d3only::Bool
end

function deferredcanvas(f::Function)
    DeferredCanvas(f, 0, false)
end

function deferredcanvas(f::Function, order::Integer, d3only::Bool)
    DeferredCanvas(f, order, d3only)
end

# non-empty tree of canvases.
type CanvasTree <: Canvas
    box::BoundingBox
    form::Form
    property::Property
    unit_box::UnitBox
    rot::Rotation
    children::List{Canvas}

    # Z-order of this canvas relative to its siblings.
    order::Integer

    # True if this canvas should use its parent's coordinate system.
    units_inherited::Bool

    # True if children of the canvas should be clipped by its bounding box.
    clip::Bool

    # True if the canvas (and its children) should be ignored on non-d3
    # backends.
    d3only::Bool

    function CanvasTree(box::BoundingBox,
                        form::Form,
                        property::Property,
                        unit_box::UnitBox,
                        rot::Rotation,
                        children::List{Canvas},
                        order::Integer,
                        units_inherited::Bool,
                        clip::Bool,
                        d3only::Bool)

        new(box, form, property, unit_box, rot,
            children, order, units_inherited, clip, d3only)
    end

    function CanvasTree(x0=0.0w,
                        y0=0.0h,
                        width=1.0w,
                        height=1.0h;
                        unit_box=UnitBox(),
                        rotation=Rotation(),
                        units_inherited=false,
                        order=0,
                        clip=false,
                        d3only=false)
        new(BoundingBox(x0, y0, width, height),
            empty_form,
            empty_property,
            unit_box,
            rotation,
            ListNil{Canvas}(),
            order,
            units_inherited,
            clip,
            d3only)
    end

    # shallow copy constructor
    function CanvasTree(c::CanvasTree)
        new(c.box, c.form, c.property, c.unit_box,
            c.rot, c.children, c.order, c.units_inherited, c.clip, c.d3only)
    end
end


# Alias constructor functions
const canvas = CanvasTree


copy(c::CanvasTree) = CanvasTree(c)


# Return a new Canvas with unit_box substituted.
function set_unit_box(c::CanvasTree, unit_box::UnitBox)
    c = copy(c)
    c.unit_box = unit_box
    c
end


# Return a new Canvas with the box substituted
function set_box(c::CanvasTree, box::BoundingBox)
    c = copy(c)
    c.box = box
    c
end


function show(io::IO, c::CanvasTree)
    print(io, "Canvas(")
    show(io, c.children)
    print(io, ")")
end


# Similar to dump(), but briefer
function contents(io::IO, c::CanvasTree, n::Int, indent)
    lc = length(c.children)
    lf = length(c.form)
    println(indent, "Canvas with ", lc, lc == 1 ? " child" : " children",
            " and ", lf, lf == 1 ? " form" : " forms")
    if n > 0
        # Children
        i = 1
        for cc in c.children
            contents(io, cc, n - 1, string(indent, "  "))
            if i > 10
                println(io, indent, "  ...")
                break
            end
            i += 1
        end
        # Form
        contents(io, c.form, n, indent)
    end
end
contents(io::IO, c::CanvasTree, n::Int) = contents(io, c, n, "")
contents(io::IO, c::CanvasTree) = contents(io, c, 10, "")
contents(c::CanvasTree, args...) = contents(OUTPUT_STREAM, c, args...)


compose(canvas::Canvas) = canvas


# Compositions of canvases is tree joining by making b a subtree of a.
#     a      b             a___
#    / \    / \    --->   / \  \
#   X   Y  U   V         X   Y  b
#                              / \
#                             U   V
function compose(canvases::Canvas...)
    root = empty_canvas
    for canvas in canvases
        if canvas === empty_canvas
            continue
        elseif root === empty_canvas
            root = copy(canvas)
        else
            # TODO: make immutable compatible
            root.children = cons(canvas, root.children)
        end
    end
    root
end


# Composition of forms into canvases
function compose(a::CanvasTree, b::Form)
    CanvasTree(a.box, combine(a.form, b), a.property,
               a.unit_box, a.rot, a.children, a.order, a.units_inherited,
               a.clip, a.d3only)
end


function compose(a::EmptyCanvas, b::Form)
    a
end


function compose(a::CanvasTree, b::Property)
    CanvasTree(a.box, a.form, combine(a.property, b),
               a.unit_box, a.rot, a.children, a.order, a.units_inherited,
               a.clip, a.d3only)
end


function compose(a::EmptyCanvas, b::Property)
    a
end


function compose(a::DeferredCanvas, b::Union(Canvas, Form, Property))
    error("Error: Deferred canvases cannot be composed with.")
end


# Draw a realization of the graphic on a given backend.
#
# Args:
#   backend: Backend on which to draw the graphic.
#   root_canvas: Root node of the graphic.
#
# Returns:
#   nothing
#
# Effects:
#   The root_canvas will be finished after the call to draw. If you wish to draw
#   multiple canvases, use `drawpart`.
#
function draw(backend::Backend, root_canvas::Canvas)
    if isfinished(backend)
        reset(backend)
    end

    drawpart(backend, root_canvas)
    finish(backend)
end


# Draw without finishing the backend.
function drawpart(backend::Backend, root_canvas::Canvas)
    # apply default property states
    push_property(backend,
                  combine(stroke(default_stroke_color),
                          fill(default_fill_color),
                          linewidth(default_line_width),
                          font(default_font_family),
                          fontsize(default_font_size)))

    S = {(root_canvas, Transform(), UnitBox(), root_box(backend))}
    while !isempty(S)
        s = pop!(S)

        if s == :POP_PROPERTY
            pop_property(backend)
            continue
        end

        (canvas, parent_t, unit_box, parent_box) = s

        if canvas === empty_canvas || (canvas.d3only && !isa(backend, D3))
            continue
        end

        if typeof(canvas) == DeferredCanvas
            canvas = canvas.f(parent_t, unit_box, parent_box)
            if !(typeof(canvas) <: Canvas)
                error("Error: A deferred canvas function did not evaluate to a canvas.")
            end
        end

        box = absolute_units(canvas.box, parent_t, unit_box, parent_box)
        rot = absolute_units(canvas.rot, parent_t, unit_box, box)
        t = combine(rot, parent_t)

        if !canvas.units_inherited
            unit_box = canvas.unit_box
        end

        property = canvas.property
        if canvas.clip
            property = combine(property,
                            clip(Point(0w, 0h), Point(1w, 0h),
                                 Point(1w, 1h), Point(0w, 1h)))
        end

        if !is(property, empty_property)
            push!(S, :POP_PROPERTY)
            push_property(backend, absolute_units(property, t, unit_box, box))
        end

        draw(backend, t, unit_box, box, canvas.form)

        ordered_children = Array((Integer, Integer, Canvas), length(canvas.children))
        for (i, child) in enumerate(canvas.children)
            # The tuple here includes i so canvases are never compared to break
            # ties.
            ordered_children[i] = (child.order, i, child)
        end
        sort!(ordered_children)
        reverse!(ordered_children)

        for (_, _, child) in ordered_children
            push!(S, (child, t, unit_box, box))
        end
    end
    pop_property(backend)
end


# Put a form in a default canvas and draw it.
function draw(backend::Backend, form::Form)
    draw(backend, compose(canvas(), form))
end

