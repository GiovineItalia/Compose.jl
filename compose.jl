
# Laws of composition

require("canvas.jl")
require("form.jl")
require("property.jl")
require("cairo.jl")


# Apples to apples

compose!(a::Property, b::Property) = (append!(a.specifics, b.specifics) ; a)
compose!(a::Form,     b::Form)     = (push(a.specifics, b) ; a)
compose!(a::Canvas,   b::Canvas)   = (push(a.children, b) ; a)


# Apples to oranges

compose!(a::Form,   b::Property) = (compose!(a.property, b) ; a)
compose!(a::Canvas, b::Property) = (compose!(a.property, b) ; a)
compose!(a::Canvas, b::Form)     = (compose!(a.form, b) ; a)


# Apples to bushels

typealias ComposeType Union(Property, Form, Canvas)
typealias ComposeExpr Union(ComposeType, Vector, Nothing)

function compose!(a::ComposeType, x::ComposeType, ys::ComposeExpr...)
    compose!(compose!(a, x), ys...)
end


function compose!(a::ComposeType, xs::Vector, ys::ComposeExpr...)
    compose!(a, compose!(xs), ys...)
end


compose_vector_memo = Dict{Uint64, ComposeType}()

function compose!(xs::Vector)
    id = object_id(xs)
    v = get(compose_vector_memo, id, nothing)
    v == nothing ?  compose_vector_memo[id] = compose!(xs...) : v
end


function compose!(xs::Vector, ys::ComposeExpr...)
    compose!(compose!(xs), ys...)
end


function compose!(a::ComposeType, b::Nothing)
    a
end


# Non-destructive compose

# TODO


# Helpful functions outside the Canvas/Form/Property/compose! formalism.

function pad!(canvas::Canvas, u::MeasureOrNumber)
    u = size_measure(u)
    compose!(Canvas(u, u, 1w - 2u, 1h - 2u), canvas)
end


function pad!(expr::ComposeExpr, u::MeasureOrNumber)
    pad!(compose!(Canvas(), expr), u)
end


# Actually drawing things


# A type packaging a canvas with the information needed to draw it.
type DrawCanvasContext
    canvas::Canvas
    t::NativeTransform
    unit_box::BoundingBox
    parent_box::NativeBoundingBox
    parent_property::Property
end


# Draw a canvas on a backend
function draw(backend::Backend, root_canvas::Canvas)
    Q = Queue()
    box = root_box(backend)
    enqueue(Q,
        DrawCanvasContext(root_canvas,
                          NativeTransform(),
                          BoundingBox(),
                          box,
                          default_property(backend)))

    while !isempty(Q)
        ctx = pop(Q)
        box = native_measure(ctx.canvas.box, ctx.t, ctx.unit_box,
                             ctx.parent_box, backend)
        rot = native_measure(ctx.canvas.rot, ctx.t, ctx.unit_box,
                             box, backend)
        t = combine(rot, ctx.t)

        property = isempty(ctx.canvas.property) ?
                ctx.parent_property : copy(ctx.parent_property)
        compose!(property, ctx.canvas.property)

        for f in ctx.canvas.form.specifics
            draw(backend, t, ctx.canvas.unit_box, box, property, f)
        end

        for child in ctx.canvas.children
            enqueue(Q, DrawCanvasContext(child, t, ctx.canvas.unit_box,
                                         box, property))
        end
    end
end


function draw(backend::Backend, expr::ComposeExpr)
    draw(backend, compose!(Canvas(), expr))
end


