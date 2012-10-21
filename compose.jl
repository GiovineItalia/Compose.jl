
require("canvas.jl")
require("form.jl")
require("property.jl")
require("cairo.jl")
require("svg.jl")


# Compose operator
|(xs::Property...) = compose(xs...)
|(xs::Form...)     = compose(xs...)
|(xs::Canvas...)   = compose(xs...)


# Insert operator
<<(a::Form,   b::Property) = insert(a, b)
<<(a::Canvas, b::Form)     = insert(a, b)
<<(a::Canvas, b::Property) = insert(a, b)


# Helpful functions.

# Pad a canvas by composing it into a parent convas with margins.
function pad(c::Canvas, u::MeasureOrNumber)
    u = size_measure(u)
    compose(canvas(u, u, 1w - 2u, 1h - 2u), c)
end


# TODO:
# A function or functions to stick canvases together with various alignments.
# This will require some thought.
#

