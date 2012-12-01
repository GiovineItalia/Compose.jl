
module Compose

using Base
import Base.+, Base.-, Base.*, Base./, Base.|, Base.convert,
       Base.length, Base.==, Base.<=, Base.isempty, Base.insert,
       Base.start, Base.next, Base.done

export |, <<, pad, hstack, vstack

load("Compose/src/canvas.jl")
load("Compose/src/form.jl")
load("Compose/src/property.jl")
load("Compose/src/cairo.jl")
load("Compose/src/svg.jl")


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

# TODO: pad_top, pad_bottom, pad_left, pad_right


# Create a new canvas containing the given canvases stacked horizontally.
function hstack(x0::MeasureOrNumber, y0::MeasureOrNumber, height::MeasureOrNumber,
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

        root |= canvas
        x += canvas.box.width
    end

    root
end


# Create a new canvas containing the given canvases stacked vertically.
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

        root |= canvas
        y += canvas.box.height
    end

    root
end


end # module Compose
