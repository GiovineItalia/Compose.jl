
# Laws of composition

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


