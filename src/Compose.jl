
require("Color")
require("DataFrames")
require("Iterators")
require("JSON")
require("Mustache")

module Compose

using Color
typealias ColorOrNothing Union(ColorValue, Nothing)

using DataFrames
using Iterators
using JSON

import JSON.json

import Base.+, Base.-, Base.*, Base./, Base.|, Base.convert,
       Base.length, Base.==, Base.<, Base.<=, Base.>=, Base.isempty, Base.insert,
       Base.start, Base.next, Base.done, Base.copy, Base.isless, Base.max,
       Base.<<, Base.>>, Base.show, Base.hex, Base.writemime

export |, <<, >>, pad, pad_outer, pad_inner, hstack, vstack, gridstack, compose,
       combine, contents, decompose

import Mustache
import Iterators

using Color

# Empty combine. This violates the rules a bit, since nothing is not the
# identity element in any of the monoids, but it's sometimes convenient if it
# behaves as such.
combine() = nothing

include("util.jl")
include("list.jl")
include("measure.jl")
include("backend.jl")
include("property.jl")
include("form.jl")
include("canvas.jl")

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

# Use cairo for the PNG, PS, PDF if its installed.
try
    require("Cairo")
    include("cairo.jl")
catch
    PNG(::String, ::MeasureOrNumber, ::MeasureOrNumber) =
        error("Cairo must be installed to use the PNG backend.")
    PS(::String, ::MeasureOrNumber, ::MeasureOrNumber) =
        error("Cairo must be installed to use the PS backend.")
    PDF(::String, ::MeasureOrNumber, ::MeasureOrNumber) =
        error("Cairo must be installed to use the PDF backend.")
end

include("svg.jl")
include("d3.jl")
include("dataform.jl")



typealias Composable Union(Form, Property, Canvas)


# Compose operator
<<(a::Form,   b::Property)     = compose(a, b)
<<(a::Form,   b::DataProperty) = compose(a, b)
<<(a::Canvas, b::Form)         = compose(a, b)
<<(a::Canvas, b::Property)     = compose(a, b)
<<(a::Canvas, b::DataProperty) = compose(a, b)
<<(a::Canvas, b::Canvas)       = compose(a, b)

>>(b::Property,     a::Form)   = compose(a, b)
>>(b::DataProperty, a::Form)   = compose(a, b)
>>(b::Form,         a::Canvas) = compose(a, b)
>>(b::Property,     a::Canvas) = compose(a, b)
>>(b::DataProperty, a::Canvas) = compose(a, b)
>>(b::Canvas,       a::Canvas) = compose(a, b)

# Combine operator
|(xs::Property...) = combine(xs...)
|(xs::Form...)     = combine(xs...)

# Compose over hetergenous lists of things.
compose(x) = x
compose(xs::Tuple) = compose(xs...)
compose(xs::Array) = compose(xs...)
compose(x, y, zs...) = compose(compose(compose(x), compose(y)), zs...)

# Make nothings go away.
compose(::Nothing, ::Nothing) = nothing
compose(::Nothing, x::Composable) = x
compose(x::Composable, ::Nothing) = x


# Helpful functions# Create a new canvas containing the given canvas with margins on all sides of
# size u.
function pad_outer(c::Canvas, u::MeasureOrNumber)
    u = size_measure(u)
    root = canvas(c.box.x0, c.box.y0, c.box.width + 2u, c.box.height + 2u,
                  units_inherited=true)
    c = copy(c)
    c.box = copy(c.box)
    c.box.width = 1w -2u
    c.box.height = 1h - 2u
    c.box.x0 = u
    c.box.y0 = u

    compose(root, c)
end


function pad_inner(c::Canvas, u::MeasureOrNumber)
    compose(canvas(), (canvas(u, u, 1w - 2u, 1h - 2u), c))
end


const pad = pad_outer

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
function hstack(x0::MeasureOrNumber, y0::MeasureOrNumber, height::MeasureOrNumber,
                aligned_canvases::(Canvas, VAlignment)...)

    # To get the expected results, we scale width units, so that everything
    # fits.
    total_width_units = 0
    for (canvas, _)  in aligned_canvases
        if typeof(canvas.box.width) == SimpleMeasure{WidthUnit}
            total_width_units += canvas.box.width.value
        elseif typeof(canvas.box.width) == CompoundMeasure &&
               haskey(canvas.box.width.values, WidthUnit)
            total_width_units += canvas.box.width.values[WidthUnit]
       end
    end

    width = CompoundMeasure() + 0w

    if length(aligned_canvases) > 0
        width += sum([canvas.box.width for (canvas, _) in aligned_canvases])
    end

    if width.values[WidthUnit] > 0
        width.values[WidthUnit] /= total_width_units
    end

    height = y_measure(height)

    root = canvas(x0, y0, width, height)
    x = 0cx
    for (canvas, aln) in aligned_canvases
        canvas = copy(canvas)
        canvas.box = copy(canvas.box)

        if typeof(canvas.box.width) == SimpleMeasure{WidthUnit}
            canvas.box.width.value /= total_width_units
        elseif typeof(canvas.box.width) == CompoundMeasure &&
               haskey(canvas.box.width.values, WidthUnit)
            if canvas.box.width.value[WidthUnit] > 0.0
                canvas.box.width.values[WidthUnit] /= total_width_units
            end
        end

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


hstack() = canvas()


# Create a new canvas containing the given canvases stacked horizontally.
#
# This is the simple version of hstack. The root canvas will be placed on 0cx,
# 0cy, and its height will be the maximum of the canvases it contains. All
# canvases will be centered vertically.
#
function hstack(canvases::Canvas...; x0::MeasureOrNumber=0,
                y0::MeasureOrNumber=0, height=0)
    if height == 0
        height = max([canvas.box.height for canvas in canvases])
    end
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

    # Scale height units
    total_height_units = 0
    for (canvas, _)  in aligned_canvases
        if typeof(canvas.box.height) == SimpleMeasure{HeightUnit}
            total_height_units += canvas.box.height.value
        elseif typeof(canvas.box.height) == CompoundMeasure &&
               haskey(canvas.box.height.values, HeightUnit)
            total_height_units += canvas.box.height.values[HeightUnit]
       end
    end

    width = x_measure(width)

    height = CompoundMeasure() + 0h

    if length(aligned_canvases) > 0
        height += sum([canvas.box.height for (canvas, _) in aligned_canvases])
    end

    if height.values[HeightUnit] > 0
        height.values[HeightUnit] /= total_height_units
    end

    root = canvas(x0, y0, width, height)
    y = 0cy
    for (canvas, aln) in aligned_canvases
        canvas = copy(canvas)
        canvas.box = copy(canvas.box)

        if typeof(canvas.box.height) == SimpleMeasure{HeightUnit}
            canvas.box.height.value /= total_height_units
        elseif typeof(canvas.box.height) == CompoundMeasure &&
               haskey(canvas.box.height.values, HeightUnit)
            if canvas.box.height.values[HeightUnit] > 0.0
                canvas.box.height.values[HeightUnit] /= total_height_units
            end
        end

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


vstack() = canvas()


# Create a new canvas containing the given canvases stacked horizontally.
#
# The simple version of vstack. The root canvas will be placed on 0cx, 0cy, and
# its width will be the maximum of the canvases it contains. All canvases will
# be centered horizontally..
#
function vstack(canvases::Canvas...; x0::MeasureOrNumber=0,
                y0::MeasureOrNumber=0, width::MeasureOrNumber=0)
    if width == 0
        width = max([canvas.box.width for canvas in canvases]...)
    end
    vstack(0, 0, width, [(canvas, hcenter) for canvas in canvases]...)
end




function scale_width_height_units(u::CompoundMeasure,
                                  width_scale::Measure,
                                  height_scale::Measure)
    u = copy(u)
    if haskey(u.values, WidthUnit)
        val = u.values[WidthUnit]
        u.values[WidthUnit] = 0.0
        u += val * width_scale
    end

    if haskey(u.values, HeightUnit)
        val = u.values[HeightUnit]
        u.values[HeightUnit] = 0.0
        u += val * height_scale
    end

    u
end


function scale_width_height_units(u::SimpleMeasure,
                                  width_scale::Measure,
                                  height_scale::Measure)
    scale_width_height_units(convert(CompoundMeasure, u),
                             width_scale, height_scale)
end


# Arrange a matrix of canvases in a grid.
#
# This just works like a simultaneous hstack and vstack.
#
function gridstack(canvases::AbstractMatrix{Canvas},
                   x0::MeasureOrNumber=0, y0::MeasureOrNumber=0;
                   halign::HAlignment=hleft, valign::VAlignment=vtop)


    n, m = size(canvases)

    row_heights = fill(CompoundMeasure() + 0h, n)
    col_widths  = fill(CompoundMeasure() + 0w, m)
    for i in 1:n, j in 1:m
        row_heights[i] = max(row_heights[i], canvases[i, j].box.height)
        col_widths[j]  = max(col_widths[j], canvases[i, j].box.width)
    end
    root_width_units = convert(CompoundMeasure, sum(col_widths)).values[WidthUnit]
    root_height_units = convert(CompoundMeasure, sum(row_heights)).values[HeightUnit]

    root_abs_x_units = copy(sum(col_widths))
    root_abs_x_units.values[WidthUnit] = 0.0

    root_abs_y_units = copy(sum(row_heights))
    root_abs_y_units.values[HeightUnit] = 0.0

    root_width_unit_scale = (1.0w - root_abs_x_units) / root_width_units
    root_height_unit_scale = (1.0h - root_abs_y_units) / root_height_units

    row_positions = Array(CompoundMeasure, n+1)
    row_positions[1] = 0h
    for i in 2:n+1
        row_positions[i] = row_positions[i - 1] + row_heights[i - 1]
    end

    for i in 2:n+1
        row_positions[i] = scale_width_height_units(row_positions[i],
                                                    root_width_unit_scale,
                                                    root_height_unit_scale)
    end

    col_positions = Array(CompoundMeasure, m+1)
    col_positions[1] = 0w
    for j in 2:m+1
        col_positions[j] = col_positions[j - 1] + col_widths[j - 1]
    end

    for j in 2:m+1
        col_positions[j] = scale_width_height_units(col_positions[j],
                                                    root_width_unit_scale,
                                                    root_width_unit_scale)
    end

    root = canvas(x0, y0, 1w, 1h)

    for i in 1:n, j in 1:m
        canvas = copy(canvases[i,j])
        canvas.box = copy(canvas.box)

        # Make adjustments to the interpretation of width/height units.
        canvas.box.width = scale_width_height_units(canvas.box.width,
                                                    root_width_unit_scale,
                                                    root_height_unit_scale)

        canvas.box.height = scale_width_height_units(canvas.box.height,
                                                     root_width_unit_scale,
                                                     root_height_unit_scale)

        if halign == hleft
            canvas.box.x0 = col_positions[j]
        elseif halign == hcenter
            canvas.box.x0 =
                (col_positions[j] + col_positions[j+1] + canvas.box.width) / 2
        elseif halign == hright
            canvas.box.x0 = col_positions[j+1] - canvas.box.width
        end

        if valign == vtop
            canvas.box.y0 = row_positions[i]
        elseif valign == vcenter
            canvas.box.y0 =
                (row_positions[i] + row_positions[i+1] + canvas.box.height) / 2
        elseif valign == vright
            canvas.box.y0 = row_positions[i+1] - canvas.box.height
        end

        root <<= canvas
    end

    root
end


# Turn the tree represented by a ComposeType into a nested array (S-expression)
# representation.
#
# This operation is fully invertable with `compose`. Thas is
# decompose(compose(x)) = x should always be true if x is either a Form or a
# Canvas.
#
function decompose(p::Property)
    v = Array(Any, length(p))
    i = 1
    while !is(p, empty_property)
        v[i] = PropertySeq(p.primitive)
        i += 1
        p = p.next
    end
    v
end


decompose(f::EmptyForm) = {}

function decompose(f::FormTree)
    pv = decompose(f.property)
    cv = {decompose(child) for child in f.children}

    f = copy(f)
    f.property = empty_property
    f.children = ListNil{FormTree}()

    v = Array(Any, length(pv) + length(cv) + 1)
    v[1] = f

    i = 2
    for c in cv
        if length(c) == 1
            v[i] = c[1]
        else
            v[i] = c
        end
        i += 1
    end

    for p in pv
        v[i] = p
        i += 1
    end

    v
end


decompose(c::EmptyCanvas) = {}

function decompose(c::CanvasTree)
    pv = decompose(c.property)
    fv = decompose(c.form)
    cv = {decompose(child) for child in c.children}

    c = copy(c)
    c.property = empty_property
    c.form = empty_form
    c.children = ListNil{Canvas}()

    v = Array(Any, length(pv) + length(fv) + length(cv) + 1)
    v[1] = c

    i = 2
    for c in cv
        if length(c) == 1
            v[i] = c[1]
        else
            v[i] = c
        end
        i += 1
    end

    for f in fv
        if length(f) == 1
            v[i] = f[1]
        else
            v[i] = f
        end
        i += 1
    end

    for p in pv
        v[i] = p
        i += 1
    end

    v
end


# Emitting graphics.
#
# Calling draw on a backend without an explicit output files causes the graphic
# to be "emitted". What the means depends on the context. By default, it writes
# the file to a temporary file and opens it in the appropriate viewer.
#
# But this behavior may be changed. In particular when weaving documents, we can
# change the function used to emit graphics to instead embed the graphic in the
# document.
#
# More explicitly, this is just dynamic multiple dispatch. That is multiple
# dispath with bindings that we can change on a whim.
#

type Emitable
    mime::String
    data
end

emitters = Dict{String, Function}()


function emit(emitable::Emitable)
    if haskey(emitters, emitable.mime)
        emitter = emitters[emitable.mime]
        emitter(emitable.data)
    else
        warn("Unable to emit data of type ", string(typeof(emitable)))
    end
end


# Default emitters

function emitsvg(data::String)
    templ = readall(joinpath(Pkg.dir("Compose"), "src", "show.html"))
    htmlout_path, htmlout = mktemp()
    write(htmlout, Mustache.render(templ, {"svgdata" => data}))
    close(htmlout)
    htm_path = htmlout_path * ".html"
    mv(htmlout_path, htm_path)
    open_browser(htm_path)
end

emitters["image/svg+xml"] = emitsvg


end # module Compose

