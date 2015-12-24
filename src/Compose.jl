VERSION >= v"0.4.0-dev+6521" && __precompile__()

module Compose

using Colors
using Iterators
using DataStructures
using Compat
using Measures
import JSON

import Base: length, start, next, done, isempty, getindex, setindex!,
             display, writemime, showcompact, convert, zero, isless, max, fill, size, copy,
             min, max, abs, +, -, *, /

import Measures: resolve, w, h

export compose, compose!, Context, UnitBox, AbsoluteBoundingBox, Rotation, Mirror,
       ParentDrawContext, context, ctxpromise, table, set_units!, minwidth, minheight,
       text_extents, max_text_extents, polygon, line, rectangle, circle, path,
       ellipse, text, curve, bitmap, stroke, fill, strokedash, strokelinecap,
       strokelinejoin, linewidth, visible, fillopacity, strokeopacity, clip,
       font, fontsize, svgid, svgclass, svgattribute, jsinclude, jscall, Measure,
       inch, mm, cm, pt, px, cx, cy, w, h, hleft, hcenter, hright, vtop, vcenter,
       vbottom, SVG, SVGJS, PGF, PNG, PS, PDF, draw, pad, pad_inner, pad_outer,
       hstack, vstack, gridstack, LineCapButt, LineCapSquare, LineCapRound,
       CAIROSURFACE, introspect, set_default_graphic_size, set_default_jsmode,
       boundingbox, Patchable



function isinstalled(pkg, ge=v"0.0.0-")
    try
        # Pkg.installed might throw an error,
        # we need to account for it to be able to precompile
        ver = Pkg.installed(pkg)
        ver == nothing && try
            # Assume the version is new enough if the package is in LOAD_PATH
            ex = Expr(:import, symbol(pkg))
            @eval $ex
            return true
        catch
            return false
        end
        return ver >= ge
    catch
        return false
    end
end


abstract Backend


"""
Some backends can more efficiently draw forms by batching. If so, they
shuld define a similar method that returns true.
"""
function canbatch(::Backend)
    return false
end

# Allow users to supply strings without deprecation warnings
parse_colorant(c::Colorant) = c
parse_colorant(str::AbstractString) = parse(Colorant, str)
parse_colorant_vec(c...) = to_vec(map(parse_colorant, c)...)
@noinline to_vec(c...) = [c...]

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
include("batch.jl")
include("table.jl")
include("stack.jl")

# How large to draw graphics when not explicitly drawing to a backend
default_graphic_width = sqrt(2)*10*cm
default_graphic_height = 10cm

function set_default_graphic_size(width::MeasureOrNumber,
                                  height::MeasureOrNumber)
    global default_graphic_width
    global default_graphic_height
    default_graphic_width = x_measure(width)
    default_graphic_height = y_measure(height)
    nothing
end


default_graphic_format = :html

function set_default_graphic_format(fmt::Symbol)
    if !(fmt in [:html, :png, :svg, :pdf, :ps, :pgf])
        error("$(fmt) is not a supported plot format")
    end
    global default_graphic_format
    default_graphic_format = fmt
    nothing
end



# Default means to include javascript dependencies in the SVGJS backend.
default_jsmode = :embed

function set_default_jsmode(mode::Symbol)
    global default_jsmode
    if mode in [:none, :exclude, :embed, :linkabs, :linkrel]
        default_jsmode = mode
    else
        error("$(mode) is not a valid jsmode")
    end
    nothing
end


function default_mime()
    if default_graphic_format == :png
        "image/png"
    elseif default_graphic_format == :svg
        "image/svg+xml"
    elseif default_graphic_format == :html
        "text/html"
    elseif default_graphic_format == :ps
        "application/postscript"
    elseif default_graphic_format == :pdf
        "application/pdf"
    elseif default_graphic_format == :pgf
        "application/x-tex"
    else
        ""
    end
end

# Default property values
default_font_family = "Helvetica Neue,Helvetica,Arial,sans"
default_font_size = 11pt
default_line_width = 0.3mm
default_stroke_color = nothing
default_fill_color = colorant"black"


# Use cairo for the PNG, PS, PDF if it's installed.
macro missing_cairo_error(backend)
    msg1 = """
    Cairo and Fontconfig are necessary for the $(backend) backend. Run:
      Pkg.add("Cairo")
      Pkg.add("Fontconfig")
    """
    msg2 = if VERSION >= v"0.4.0-dev+6521"
        """
        You also have to delete $(joinpath(Base.LOAD_CACHE_PATH[1], "Compose.ji"))
        and restart your REPL session afterwards.
        """
    else
        """
        You also have to restart your REPL session afterwards.
        """
    end
    string(msg1, msg2)
end

if isinstalled("Cairo")
    include("cairo_backends.jl")
    include("immerse_backend.jl")
else
    global PNG
    global PS
    global PDF

    PNG(args...) = error(@missing_cairo_error "PNG")
    PS(args...) = error(@missing_cairo_error "PS")
    PDF(args...) = error(@missing_cairo_error "PDF")
end
include("svg.jl")
include("pgf_backend.jl")

if isinstalled("Patchwork", v"0.1.2")
    include("patchable.jl")
end

# If available, pango and fontconfig are used to compute text extents and match
# fonts. Otherwise a simplistic pure-julia fallback is used.

if isinstalled("Fontconfig")
    pango_cairo_ctx = C_NULL
    include("pango.jl")

    function __init__()
        global pango_cairo_ctx
        global pangolayout
        ccall((:g_type_init, Cairo._jl_libgobject), Void, ())
        pango_cairo_fm  = ccall((:pango_cairo_font_map_new, libpangocairo),
                                 Ptr{Void}, ())
        pango_cairo_ctx = ccall((:pango_font_map_create_context, libpango),
                                 Ptr{Void}, (Ptr{Void},), pango_cairo_fm)
        pangolayout = PangoLayout()
    end
else
    include("fontfallback.jl")
end

function writemime(io::IO, m::MIME"text/html", ctx::Context)
    draw(SVGJS(io, default_graphic_width, default_graphic_height, false,
               jsmode=default_jsmode), ctx)
end

function writemime(io::IO, m::MIME"image/svg+xml", ctx::Context)
    draw(SVG(io, default_graphic_width, default_graphic_height, false), ctx)
end

try
    getfield(Compose, :Cairo) # throws if Cairo isn't being used
    function writemime(io::IO, ::MIME"image/png", ctx::Context)
        draw(PNG(io, default_graphic_width, default_graphic_height), ctx)
    end
end


import Base.Multimedia: @try_display, xdisplayable

function display(p::Context)
    displays = Base.Multimedia.displays
    for i = length(displays):-1:1
        m = default_mime()
        if xdisplayable(displays[i], m, p)
             @try_display return display(displays[i], m, p)
        end

        if xdisplayable(displays[i], p)
            @try_display return display(displays[i], p)
        end
    end
    invoke(display,(Any,),p)
end


function pad_outer(c::Context,
                   left_padding::MeasureOrNumber,
                   right_padding::MeasureOrNumber,
                   top_padding::MeasureOrNumber,
                   bottom_padding::MeasureOrNumber)

    left_padding   = size_measure(left_padding)
    right_padding  = size_measure(right_padding)
    top_padding    = size_measure(top_padding)
    bottom_padding = size_measure(bottom_padding)

    root = context(c.box.x0[1], c.box.x0[1],
                   c.box.a[1] + left_padding + right_padding,
                   c.box.a[2] + top_padding + bottom_padding,
                   minwidth=c.minwidth,
                   minheight=c.minheight)
    c = copy(c)
    c.box = BoundingBox(left_padding, top_padding,
                        1w - left_padding - right_padding,
                        1h - top_padding - bottom_padding)
    return compose!(root, c)
end


function pad_outer(c::Context, padding::MeasureOrNumber)
    return pad_outer(c, padding, padding, padding, padding)
end


function pad_outer(cs::Vector{Context},
                   left_padding::MeasureOrNumber,
                   right_padding::MeasureOrNumber,
                   top_padding::MeasureOrNumber,
                   bottom_padding::MeasureOrNumber)
    return map(c -> pad_outer(c, left_padding, right_padding,
                              top_padding, bottom_padding), cs)
end


function pad_outer(cs::Vector{Context}, padding::MeasureOrNumber)
    return pad_outer(cs, padding, padding, padding, padding)
end


function pad_inner(c::Context,
                   left_padding::MeasureOrNumber,
                   right_padding::MeasureOrNumber,
                   top_padding::MeasureOrNumber,
                   bottom_padding::MeasureOrNumber)
    left_padding   = size_measure(left_padding)
    right_padding  = size_measure(right_padding)
    top_padding    = size_measure(top_padding)
    bottom_padding = size_measure(bottom_padding)

    root = context(c.box.x0[1], c.box.x0[2],
                   c.box.a[1], c.box.a[2],
                   minwidth=c.minwidth,
                   minheight=c.minheight)
    c = copy(c)
    c.box = BoundingBox(left_padding, top_padding,
                        1w - left_padding - right_padding,
                        1h - top_padding - bottom_padding)
    return compose!(root, c)
end


function pad_inner(c::Context, padding::MeasureOrNumber)
    return pad_inner(c, padding, padding, padding, padding)
end


function pad_inner(cs::Vector{Context}, left_padding::MeasureOrNumber,
                   right_padding::MeasureOrNumber,
                   top_padding::MeasureOrNumber,
                   bottom_padding::MeasureOrNumber)
    return map(c -> pad_inner(c, left_padding, right_padding,
                              top_padding, bottom_padding), cs)
end


function pad_inner(cs::Vector{Context}, padding::MeasureOrNumber)
    return pad_inner(cs, padding, padding, padding, padding)
end


function gridstack(cs::Matrix{Context})
    m, n = size(cs)
    t = Table(m, n, 1:m, 1:n, x_prop=ones(n), y_prop=ones(m))
    for i in 1:m, j in 1:n
        t[i, j] = [cs[i, j]]
    end
    return compose!(context(), t)
end


const pad = pad_outer

if VERSION >= v"0.4.0-dev+5512"
    include("precompile.jl")
    _precompile_()
end

end # module Compose
