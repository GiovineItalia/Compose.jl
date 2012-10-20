
# Canvas: a thing upon which other things are placed.

require("measure.jl")
require("form.jl")
require("property.jl")


# A box giving the coordinate system used by a canvas.
type Units
    x0::Float64
    y0::Float64
    width::Float64
    height::Float64

    function Units(width::Number,
                   height::Number)
        new(0.0, 0.0, width, height)
    end

    function Units(x0::Number,
                   y0::Number,
                   width::Number,
                   height::Number)
        new(x0, y0, width, height)
    end
end


# Canvases are containers for forms and other canvases with an associated
# coordinate transform.
abstract Canvas


# An identity element for the canvas monoid.
type EmptyCanvas <: Canvas end
const empty_canvas = EmptyCanvas()


# non-empty tree of canvases.
type CanvasTree <: Canvas
    box::BoundingBox
    form::Form
    unit_box::BoundingBox
    rot::Rotation
    children::List{Canvas}

    function CanvasTree(opts::Union(Units, Rotation)...)
        CanvasTree(0.0w, 0.0h, 1.0w, 1.0h, opts...)
    end

    function CanvasTree(x0::MeasureOrNumber,
                        y0::MeasureOrNumber,
                        width::MeasureOrNumber,
                        height::MeasureOrNumber,
                        opts::Union(Units, Rotation)...)
        c = new(BoundingBox(x0, y0, width, height),
                empty_form,
                BoundingBox(),
                Rotation(),
                ListNil{Canvas}())

        for opt in opts
            if typeof(opt) == Rotation
                c.rot = opt
            elseif typeof(opt) == Units
                c.unit_box = opt.box
            end
        end

        c
    end

    # shallow copy constructor
    function CanvasTree(c::CanvasTree)
        new(c.box, c.form, c.unit_box, c.rot, c.children)
    end
end


# Alias constructor functions
const canvas = CanvasTree


copy(c::CanvasTree) = CanvasTree(c)


# Composition of canvases.
compose(a::EmptyCanvas, b::EmptyCanvas) = a
compose(a::CanvasTree, b::EmptyCanvas) = a
compose(a::EmptyCanvas, b::CanvasTree) = b


# Copositions of canvases is tree joining by making b a subtree of a.
#     a      b             a___
#    / \    / \    --->   / \  \
#   X   Y  U   V         X   Y  b
#                              / \
#                             U   V
#
function compose(a::CanvasTree, b::CanvasTree)
    a = copy(a)
    a.children = cons(b, a.children)
    a
end


#
function draw(backend::Backend, root_canvas::Canvas)
    S = {(root_canvas, NativeTransform(), BoundingBox(), root_box(backend))}
    while !isempty(S)
        (canvas, parent_t, unit_box, parent_box) = pop(S)
        if canvas === empty_canvas
            continue
        end

        box = native_measure(canvas.box, parent_t, unit_box, parent_box, backend)
        rot = native_measure(canvas.rot, parent_t, unit_box, box, backend)
        t = combine(rot, parent_t)

        draw(backend, t, unit_box, box, root_box)

        for child in canvas.children
            push(S, (child, t, canvas.unit_box, canvas.unit_box, box))
        end
    end

end



