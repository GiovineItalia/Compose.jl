
module Compose

using Base
import Base.+, Base.-, Base.*, Base./, Base.|, Base.convert,
       Base.length, Base.==, Base.<, Base.<=, Base.>=, Base.isempty, Base.insert,
       Base.start, Base.next, Base.done, Base.copy, Base.isless, Base.max,
       Base.<<, Base.>>

export |, <<, >>, pad, hstack, vstack, compose, combine

# Empty combine. This violates the rules a bit, since nothing is not the
# identity element in any of the monoids, but it's sometimes convenient if it
# behaves as such.
combine() = nothing

load("Compose/src/canvas.jl")
load("Compose/src/form.jl")
load("Compose/src/property.jl")
load("Compose/src/cairo.jl")
load("Compose/src/svg.jl")

typealias ComposeType Union(Canvas, Form, Property)

# Compose operator
<<(a::Form,   b::Property) = compose(a, b)
<<(a::Canvas, b::Form)     = compose(a, b)
<<(a::Canvas, b::Property) = compose(a, b)
<<(a::Canvas, b::Canvas)   = compose(a, b)

>>(b::Property, a::Form)   = compose(a, b)
>>(b::Form,     a::Canvas) = compose(a, b)
>>(b::Property, a::Canvas) = compose(a, b)
>>(b::Canvas,   a::Canvas) = compose(a, b)

# Combine operator
|(xs::Property...) = combine(xs...)
|(xs::Form...)     = combine(xs...)

# Compose over hetergenous lists of things.
compose(x::ComposeType) = x
compose(xs::Tuple) = compose(xs...)
compose(xs::Array) = compose(xs...)
compose(x, y, zs...) = compose(compose(compose(x), compose(y)), zs...)


# Helpful functions# Create a new canvas containing the given canvas with margins on all sides of
# size u.
function pad(c::Canvas, u::MeasureOrNumber)
    u = size_measure(u)
    compose(canvas(u, u, 1w - 2u, 1h - 2u), c)
end

# TODO: pad_top, pad_bottom, pad_left, pad_right


# Create a new canvas containing the given canvases stacked horizontally.
#
# Args:
#  x0: X-position of the new root canvas
#  y0: Y-position of the new root canvas
#  height: Height of the root canvas.
#  aligned_canvases: One or more canveses accompanied with a vertical alignment
#                    specifier, giving the vertical positioning of the canvas.
#
function hstack(x0::MeasureOrNumber, y0::MeasureOrNumber,height::MeasureOrNumber,
                aligned_canvases::(Canvas, VAlignment)...)
    width = sum([canvas.box.width for (canvas, _) in aligned_canvases])
    height = y_measure(height)

    root = canvas(x0, y0, width, height)
    x = 0cx
    for (canvas, aln) in aligned_canvases
        canvas = copy(canvas)
        canvas.box = copy(canvas.box)

        # Should we interpret vbottom to mean 0?
        canvas.box.x0 = x
        if aln == vtop
            canvas.box.y0 = 0cy
        elseif aln == vcenter
            canvas.box.y0 = (height / 2) - (canvas.box.height / 2)
        elseif aln == vbottom
            canvas.box.y0 = height - canvas.box.height
        end

        root <<= canvas
        x += canvas.box.width
    end

    root
end


# Create a new canvas containing the given canvases stacked horizontally.
#
# This is the simple version of hstack. The root canvas will be placed on 0cx,
# 0cy, and its height will be the maximum of the canvases it contains. All
# canvases will be centered vertically.
#
function hstack(canvases::Canvas...)
    height = max([canvas.box.height for canvas in canvases])
    hstack(0, 0, height, [(canvas, vcenter) for canvas in canvases]...)
end


# Create a new canvas containing the given canvases stacked vertically.
#
# Args:
#  x0: X-position of the new root canvas
#  y0: Y-position of the new root canvas
#  width: Height of the root canvas.
#  aligned_canvases: One or more canveses accompanied with a horizontal alignment
#                    specifier, giving the horizontal positioning of the canvas.
#
function vstack(x0::MeasureOrNumber, y0::MeasureOrNumber, width::MeasureOrNumber,
                aligned_canvases::(Canvas, HAlignment)...)
    width = x_measure(width)
    height = sum([canvas.box.height for (canvas, _) in aligned_canvases])

    root = canvas(x0, y0, width, height)
    y = 0cy
    for (canvas, aln) in aligned_canvases
        canvas = copy(canvas)
        canvas.box = copy(canvas.box)

        canvas.box.y0 = y
        if aln == hleft
            canvas.box.x0 = 0cx
        elseif aln == hcenter
            canvas.box.x0 = (width / 2) - (canvas.box.width / 2)
        elseif aln == hright
            canvas.box.x0 = width - canvas.box.width
        end

        root <<= canvas
        y += canvas.box.height
    end

    root
end


# Create a new canvas containing the given canvases stacked horizontally.
#
# The simple version of vstack. The root canvas will be placed on 0cx, 0cy, and
# its width will be the maximum of the canvases it contains. All canvases will
# be centered horizontally..
#
function vstack(canvases::Canvas...)
    width = max([canvas.box.width for canvas in canvases])
    vstack(0, 0, width, [(canvas, hcenter) for canvas in canvases]...)
end

end # module Compose
