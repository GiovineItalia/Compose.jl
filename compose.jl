
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
typealias ComposeExpr Union(ComposeType, Vector, Tuple)


function compose!(a::ComposeType, x::ComposeType, ys::ComposeExpr...)
    compose!(compose!(a, x), ys...)
end


function compose!(a::ComposeType, xs::Tuple, ys::ComposeExpr...)
    compose!(a, [x for x in xs], ys...)
end


function compose!(a::ComposeType, xs::Vector, ys::ComposeExpr...)
    compose!(a, compose!(xs...), ys...)
end


function compose!(xs::Tuple)
    compose!([x for x in xs])
end


function compose!(xs::Vector)
    compose!(xs...)
end


# Non-destructive compose

# TODO





