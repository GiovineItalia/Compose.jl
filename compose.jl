
# Laws of composition

require("canvas.jl")
require("form.jl")
require("property.jl")
require("cairo.jl")
require("svg.jl")

typealias ComposeType Union(Property, Form, Canvas)

# Apples

compose!(a::ComposeType) = a


# Apples to apples

compose!(a::Property, b::Property) = (append!(a.specifics, b.specifics) ; a)
compose!(a::Form,     b::Form)     = (push(a.specifics, b) ; a)
compose!(a::Canvas,   b::Canvas)   = (push(a.children, b) ; a)


# Apples to oranges

compose!(a::Form,   b::Property) = (compose!(a.property, b) ; a)
compose!(a::Canvas, b::Property) = (compose!(a.property, b) ; a)
compose!(a::Canvas, b::Form)     = (compose!(a.form, b) ; a)


# Apples to bushels

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

function compose(a::ComposeType, xs::ComposeExpr...)
    a_copy = copy(a)
    for x in xs
        compose!(a_copy, compose(x))
    end
    a_copy
end


compose(a::ComposeType) = a
compose(a::Nothing) = a
compose(a::Vector) = compose(a...)

+(xs::ComposeExpr...) = compose(xs...)

# Helpful functions outside the Canvas/Form/Property/compose! formalism.

function pad!(canvas::Canvas, u::MeasureOrNumber)
    u = size_measure(u)
    compose!(Canvas(u, u, 1w - 2u, 1h - 2u), canvas)
end


function pad!(expr::ComposeExpr, u::MeasureOrNumber)
    pad!(compose!(Canvas(), expr), u)
end


# Evenly partition out a space into an n-by-m grid of canvases, which are
# returned as a matrix. (Ok, but how can I compose these all onto one matrix?)
function grid(n::Int, m::Int)
    cs = Array(Any, n, m)
    for i in 1:n
        for j in 1:m
            cs[i,j] = Canvas((i-1)/n, (j-1)/m, 1/n, 1/m)
        end
    end
    cs
end


# Actually drawing things


# A type packaging a canvas with the information needed to draw it.
type DrawCanvasContext
    canvas::Canvas
    t::NativeTransform
    unit_box::BoundingBox
    parent_box::NativeBoundingBox
end


function draw(backend::Backend, canvas::Canvas)
    draw(backend, canvas, NativeTransform(), BoundingBox(), root_box(backend))
end


function draw(backend::Backend, canvas::Canvas, parent_t::NativeTransform,
              unit_box::BoundingBox, parent_box::NativeBoundingBox)
    box = native_measure(canvas.box, parent_t, unit_box, parent_box, backend)
    rot = native_measure(canvas.rot, parent_t, unit_box, box, backend)
    t = combine(rot, parent_t)

    if !isempty(canvas.property);
        push_property(backend,
                      native_measure(canvas.property,
                                     t, unit_box, box, backend))
    end

    for f in canvas.form.specifics
        draw(backend, t, canvas.unit_box, box, f)
    end

    for child in canvas.children
        draw(backend, child, t, canvas.unit_box, box)
    end

    if !isempty(canvas.property);
        pop_property(backend)
    end
end


function draw(backend::Backend, expr::ComposeExpr)
    draw(backend, compose!(Canvas(), expr))
end


