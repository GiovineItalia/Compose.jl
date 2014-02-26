
require("Color")
require("Iterators")
require("JSON")

module Compose

using Color
typealias ColorOrNothing Union(ColorValue, Nothing)

using Iterators
using JSON

import JSON.json

import Base: +, -, *, /, convert, length, ==, <, <=, >=, isempty, start, next,
             done, copy, isless, max, min, show, hex, writemime, zero, union

export pad, pad_outer, pad_inner, hstack, vstack, gridstack, compose,
       combine, contents, decompose, text_extents,
       Measure, AbsoluteBoundingBox, BoundingBox, UnitBox,
       absolute_x_position, absolute_y_position, absolute_units,
       mm, cm, inch, pt, w, h, px, cx, cy,
       SVG, D3, JS, PNG, PS, PDF

import Iterators


# Backwards compatibility with julia-0.2 names
if !isdefined(:rad2deg)
    const rad2deg = radians2degrees
    const deg2rad = degrees2radians
end


# Empty combine. This violates the rules a bit, since nothing is not the
# identity element in any of the monoids, but it's sometimes convenient if it
# behaves as such.
combine() = nothing

abstract Backend

include("util.jl")
include("list.jl")
include("measure.jl")
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
    include("cairo_backends.jl")
catch
    global PNG
    global PS
    global PDF
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



typealias Composable Union(Form, Property, Canvas,
                           DataForm, DataProperty)


# Compose over hetergenous lists of things.
compose(x::Composable) = x
compose(xs::Tuple) = compose(xs...)
compose(xs::Array) = compose(xs...)
compose(x, y, zs...) = compose(compose(compose(x), compose(y)), zs...)

# Make nothings go away.
compose(::Nothing, ::Nothing) = nothing
compose(::Nothing, x::Composable) = x
compose(x::Composable, ::Nothing) = x


# Helpful functions# Create a new canvas containing the given canvas with margins on all sides of
# size u.
function pad_outer(c::Canvas, u)
    u = size_measure(u)
    root = canvas(c.box.x0, c.box.y0, c.box.width + 2u, c.box.height + 2u,
                  units_inherited=true)
    c = copy(c)
    c.box = BoundingBox(u, u, 1w - 2u, 1h - 2u)

    compose(root, c)
end


function pad_inner(c::Canvas, u)
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
function hstack(x0, y0, height, aligned_canvases::(Canvas, VAlignment)...)

    if isempty(aligned_canvases)
        return canvas(x0, y0, 0, height)
    end

    # To get the expected results, we scale width units, so that everything
    # fits.
    total_width_units = sum(Float64[canvas.box.width.cw
                                    for (canvas, _) in aligned_canvases])
    width = sum(Measure[canvas.box.width
                        for (canvas, _) in aligned_canvases])
    width = Measure(width,
                    cw=total_width_units > 0.0 ?
                        width.cw / total_width_units : 0.0)

    height = y_measure(height)

    root = canvas(x0, y0, width, height)
    x = Measure()
    for (canvas, aln) in aligned_canvases
        canvas = copy(canvas)
        canvas.box = copy(canvas.box)

        if canvas.box.width.cw != 0.0
            canvas.box =
                BoundingBox(canvas.box,
                    width=Measure(canvas.box.width,
                        cw=canvas.box.width.cw / total_width_units))
        end

        # Should we interpret vbottom to mean 0?
        canvas.box = BoundingBox(canvas.box, x0=x)
        if aln == vtop
            canvas.box = BoundingBox(canvas.box, y0=Measure{T}())
        elseif aln == vcenter
            canvas.box = BoundingBox(canvas.box,
                            y0=(height / 2) - (canvas.box.height / 2))
        elseif aln == vbottom
            canvas.box = BoundingBox(canvas.box,
                            y0=height - canvas.box.height)
        end

        root = compose(root, canvas)
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
        height = maximum([canvas.box.height for canvas in canvases])
    end
    hstack(x0, y0, height, [(canvas, vcenter) for canvas in canvases]...)
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
function vstack(x0, y0, width, aligned_canvases::(Canvas, HAlignment)...)

    if isempty(aligned_canvases)
        return canvas(x0, y0, width, 0)
    end

    # Scale height units
    total_height_units = sum(Float64[canvas.box.height.ch
                                     for (canvas, _) in aligned_canvases])
    height = sum(Measure[canvas.box.height
                         for (canvas, _) in aligned_canvases])
    height = Measure(height,
                     ch=total_height_units > 0.0 ?
                          height.ch / total_height_units : 0.0)

    width = x_measure(width)

    root = canvas(x0, y0, width, height)
    y = Measure()
    for (canvas, aln) in aligned_canvases
        canvas = copy(canvas)
        canvas.box = copy(canvas.box)

        if canvas.box.height.ch != 0.0
            canvas.box =
                BoundingBox(canvas.box,
                    height=Measure(canvas.box.height,
                        ch=canvas.box.height.ch / total_height_units))
        end

        canvas.box = BoundingBox(canvas.box, y0=y)
        if aln == hleft
            canvas.box = BoundingBox(canvas.box, x0=Measure{T}())
        elseif aln == hcenter
            canvas.box = BoundingBox(canvas.box,
                            x0=(width / 2) - (canvas.box.width / 2))
        elseif aln == hright
            canvas.box = BoundingBox(canvas.box,
                            x0 = width - canvas.box.width)
        end

        root = compose(root, canvas)
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
    vstack(x0, y0, width, [(canvas, hcenter) for canvas in canvases]...)
end


function scale_width_height_units(u::Measure,
                                  width_scale::Measure,
                                  height_scale::Measure)
    u = copy(u)
    if u.cw > 0.0
        val = u.cw
        u = Measure(u, cw=0.0)
        u += val * width_scale
    end

    if u.ch > 0.0
        val = u.ch
        u = Measure(u, ch=0.0)
        u += val * height_scale
    end

    u
end


# Arrange a matrix of canvases in a grid.
#
# This just works like a simultaneous hstack and vstack.
#
function gridstack(canvases::AbstractMatrix{Canvas},
                   x0=0.0, y0=0.0;
                   halign::HAlignment=hleft, valign::VAlignment=vtop)

    if isempty(canvases)
        canvas()
    end

    n, m = size(canvases)

    row_heights = Array(Measure, n)
    fill!(row_heights, Measure())

    col_widths  = Array(Measure, m)
    fill!(col_widths, Measure())

    for i in 1:n, j in 1:m
        row_heights[i] = max(row_heights[i], canvases[i, j].box.height)
        col_widths[j]  = max(col_widths[j], canvases[i, j].box.width)
    end

    root_width_units = sum(col_widths).cw
    root_height_units = sum(row_heights).ch

    root_abs_x_units = Measure(sum(col_widths), cw=0.0)
    root_abs_y_units = Measure(sum(row_heights), ch=0.0)

    root_width_unit_scale = (1.0w - root_abs_x_units) / root_width_units
    root_height_unit_scale = (1.0h - root_abs_y_units) / root_height_units

    row_positions = Array(Measure, n+1)
    row_positions[1] = 0h
    for i in 2:n+1
        row_positions[i] = row_positions[i - 1] + row_heights[i - 1]
    end

    for i in 2:n+1
        row_positions[i] = scale_width_height_units(row_positions[i],
                                                    root_width_unit_scale,
                                                    root_height_unit_scale)
    end

    col_positions = Array(Measure, m+1)
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

        # Make adjustments to the interpretation of width/height units.
        box_width = scale_width_height_units(canvas.box.width,
                                             root_width_unit_scale,
                                             root_height_unit_scale)

        box_height = scale_width_height_units(canvas.box.height,
                                              root_width_unit_scale,
                                              root_height_unit_scale)

        canvas.box = BoundingBox(canvas.box, width=box_width, height=box_height)

        if halign == hleft
            canvas.box = BoundingBox(canvas.box, x0=col_positions[j])
        elseif halign == hcenter
            canvas.box = BoundingBox(canvas.box,
                x0=(col_positions[j] + col_positions[j+1] + canvas.box.width) / 2)
        elseif halign == hright
            canvas.box = BoundingBox(canvas.box,
                x0=col_positions[j+1] - canvas.box.width)
        end

        if valign == vtop
            canvas.box = BoundingBox(canvas.box, y0=row_positions[i])
        elseif valign == vcenter
            canvas.box = BoundingBox(canvas.box,
                y0=(row_positions[i] + row_positions[i+1] + canvas.box.height) / 2)
        elseif valign == vright
            canvas.box = BoundingBox(canvas.box,
                y0=row_positions[i+1] - canvas.box.height)
        end

        root = compose(root, canvas)

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

default_graphic_width = 12cm
default_graphic_height = 12cm


# Default property values
default_font_family = "Helvetic,Arial,sans"
default_font_size = 11pt
default_line_width = 0.3mm
default_stroke_color = nothing
default_fill_color = color("black")


function set_default_graphic_size(width::MeasureOrNumber, height::MeasureOrNumber)
    global default_graphic_width
    global default_graphic_height
    default_graphic_width = width
    default_graphic_height = height
end


function writemime(io::IO, ::MIME"text/html", canvas::Canvas)
    bb = boundingbox(canvas)
    width = isabsolute(bb.width) ? bb.width : default_graphic_width
    height = isabsolute(bb.height) ? bb.height : default_graphic_height

    draw(SVG(io, width.abs*mm, height.abs*mm), canvas)
end


function writemime(io::IO, ::MIME"text/html", form::Form)
    bb = boundingbox(form)
    width = isabsolute(bb.width) ? bb.width : default_graphic_width
    height = isabsolute(bb.height) ? bb.height : default_graphic_height

    draw(SVG(io, width.abs*mm, height.abs*mm),
         compose(canvas(-bb.x0, -bb.y0), form))
end

end # module Compose

