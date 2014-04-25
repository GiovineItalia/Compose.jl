
require("Color")
require("Iterators")
require("JSON")
require("JuMP")


module Compose

using Color
using Iterators
using JuMP
import JSON

import Base: length, start, next, done, isempty, getindex, setindex!

export compose, context, ctx, ctxpromise, table,
       polygon, rectangle, circle, stroke, fill,
       inch, mm, cm, cx, cy, w, h, SVG, draw

abstract Backend

include("misc.jl")
include("measure.jl")
include("list.jl")

# Every graphic in Compose consists of a tree.
abstract ComposeNode

# Used to mark null child pointers
immutable NullNode <: ComposeNode end
nullnode = NullNode()

include("form.jl")
include("property.jl")
include("container.jl")
include("table.jl")

# How large to draw graphics when not explicitly drawing to a backend
default_graphic_width = 12cm
default_graphic_height = 12cm

# Default property values
default_font_family = "Helvetic,Arial,sans"
default_font_size = 11pt
default_line_width = 0.3mm
default_stroke_color = nothing
default_fill_color = color("black")


include("svg.jl")

# If available, pango and fontconfig are used to compute text extents and match
# fonts. Otherwise a simplistic pure-julia fallback is used.
try
    # Trigger an exception if unavailable.
    dlopen("libfontconfig")
    dlopen("libpangocairo-1.0")
    dlopen("libpango-1.0")

    include("fontconfig.jl")
    include("pango.jl")
catch
    include("fontfallback.jl")
end


# Compose over heterogenous lists of things
compose(x::ComposeNode) = x
compose(xs::Tuple) = compose(xs...)
compose(xs::Array) = compose(xs...)
compose(x, y, zs...) = compose(compose(compose(x), compose(y)), zs...)

# Make nothings go away.
compose(::Nothing, ::Nothing) = nothing
compose(::Nothing, x::ComposeNode) = x
compose(x::ComposeNode, ::Nothing) = x

end # module Pos


