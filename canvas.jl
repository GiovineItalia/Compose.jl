
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

    function Units()
        new(0.0, 0.0, 1.0, 1.0)
    end

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


# Note: Units has almost the same representation as BoundingBox, except no unit
# is associated with the numbers.
function convert(::Type{BoundingBox}, u::Units)
    BoundingBox(u.x0, u.y0, u.width, u.height)
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
    unit_box::Units
    rot::Rotation
    children::List{Canvas}

    function CanvasTree(box::BoundingBox,
                        form::Form,
                        unit_box::Units,
                        rot::Rotation,
                        children::List{Canvas})
        new(box, form, unit_box, rot, children)
    end

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
                Units(),
                Rotation(),
                ListNil{Canvas}())

        for opt in opts
            if typeof(opt) == Rotation
                c.rot = opt
            elseif typeof(opt) == Units
                c.unit_box = opt
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


function show(io, c::CanvasTree)
    print(io, "Canvas(")
    show(io, c.children)
    print(io, ")")
end


# Copositions of canvases is tree joining by making b a subtree of a.
#     a      b             a___
#    / \    / \    --->   / \  \
#   X   Y  U   V         X   Y  b
#                              / \
#                             U   V
function compose(canvases::Canvas...)
    root = empty_canvas
    for canvas in canvases
        if canvas == empty_canvas
            continue
        elseif root == empty_canvas
            root = copy(canvas)
        else
            root.children = cons(canvas, root.children)
        end
    end
    root
end


# Insertion of forms into canvases
function insert(a::CanvasTree, b::Form)
    CanvasTree(a.box, compose(a.form, b), a.unit_box, a.rot, a.children)
end


function insert(a::EmptyCanvas, b::Form)
    a
end


function insert(a::CanvasTree, b::Property)
    if a.form === empty_form
        a = copy(a)
        c = a.children = copy(a.children)
        while typeof(c) != ListNil{Canvas}
            c.head = insert(c.head, b)
            c.tail = copy(c.tail)
            c = c.tail
        end
        a
    else
        CanvasTree(a.box, insert(a.form, b), a.unit_box, a.rot, a.children)
    end
end


function insert(a::EmptyCanvas, b::Property)
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

        unit_box = convert(BoundingBox, canvas.unit_box)

        draw(backend, t, unit_box, box, canvas.form)

        for child in canvas.children
            push(S, (child, t, convert(BoundingBox, unit_box), box))
        end
    end

end



