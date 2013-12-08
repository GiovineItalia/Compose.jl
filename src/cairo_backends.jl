
# Cairo backend for compose
import Cairo

import Cairo.CairoContext, Cairo.CairoSurface, Cairo.CairoARGBSurface,
       Cairo.CairoEPSSurface, Cairo.CairoPDFSurface, Cairo.CairoSVGSurface

export PNG, PDF, PS

abstract ImageBackend
abstract PNGBackend <: ImageBackend

abstract VectorImageBackend <: ImageBackend
abstract SVGBackend <: VectorImageBackend
abstract PDFBackend <: VectorImageBackend
abstract PSBackend  <: VectorImageBackend

type ImagePropertyState
    stroke::ColorOrNothing
    fill::ColorOrNothing
end

type Image{B <: ImageBackend} <: Backend
    out::IO
    surface::CairoSurface
    ctx::CairoContext

    width::Float64
    height::Float64

    # Current state
    stroke::ColorOrNothing
    fill::ColorOrNothing
    opacity::Float64 # in [0,1]
    stroke_opacity::Float64 # in [0,1]
    visible::Bool

    # Keep track of property
    state_stack::Vector{ImagePropertyState}

    # Close the surface when finished
    owns_surface::Bool

    # Close the stream when finished
    close_stream::Bool

    # True when finish has been called and no more drawing should occur
    finished::Bool

    # Emit on finish
    emit_on_finish::Bool

    function Image(surface::CairoSurface, ctx::CairoContext, out::IO)
        img = new()
        img.out = out
        img.width = 0
        img.height = 0
        img.surface = surface
        img.ctx = ctx
        img.stroke = RGB(0., 0., 0.)
        img.fill   = RGB(0., 0., 0.)
        img.opacity = 1.0
        img.stroke_opacity = 1.0
        img.visible = true
        img.state_stack = Array(ImagePropertyState, 0)
        img.owns_surface = false
        img.emit_on_finish = false
        img.finished = false
        img
    end

    function Image(surface::CairoSurface, ctx::CairoContext)
        Image{B}(surface, ctx, IOBuffer())
    end

    Image(surface::CairoSurface) = Image{B}(surface, CairoContext(surface))


    function Image(out::IO,
                   width::MeasureOrNumber,
                   height::MeasureOrNumber,
                   emit_on_finish::Bool=true)

        width = size_measure(width)
        height = size_measure(height)

        if !isabsolute(width) || !isabsolute(height)
            error("Image size must be specificed in absolute units.")
        end

        width = absolute_native_units(B, width.abs)
        height = absolute_native_units(B, height.abs)

        surface = newsurface(B, out, width, height)
        img = Image{B}(surface, CairoContext(surface), out)
        img.width = width
        img.height = height
        img.owns_surface = true
        img.emit_on_finish = emit_on_finish
        img
    end

    function Image(filename::String,
                   width::MeasureOrNumber,
                   height::MeasureOrNumber)
        img = Image{B}(open(filename, "w"), width, height)
        img
    end

    function Image(width::MeasureOrNumber,
                   height::MeasureOrNumber,
                   emit_on_finish::Bool=true)
        img = Image{B}(IOBuffer(), width, height, emit_on_finish)
        img
    end
end

typealias PNG Image{PNGBackend}
typealias PDF Image{PDFBackend}
typealias PS  Image{PSBackend}


# convert compose absolute units (millimeters) to the absolute units used by the
# cairo surface (pixels for PNG, points for all others)
function absolute_native_units(::Type{PNGBackend}, u::Float64)
    assumed_ppmm * u
end


function absolute_native_units{B <: ImageBackend}(::Type{B},
                                                  u::Float64)
    72 * u / 25.4
end


function absolute_native_units{B}(img::Image{B}, u::Float64)
    absolute_native_units(B, u)
end


# Width and height of a backend in absolute units
function width(img::PNG)
    Cairo.width(img.surface) / assumed_ppmm
end


function width(img::Image)
    25.4 * Cairo.width(img.surface) / 72
end


function height(img::PNG)
    Cairo.height(img.surface) / assumed_ppmm
end


function height(img::Image)
    25.4 * Cairo.height(img.surface) / 72
end


finish{B <: ImageBackend}(::Type{B}, img::Image) = nothing

function finish(::Type{PNGBackend}, img::Image)
    Cairo.write_to_png(img.surface, img.out)
end

function finish{B <: ImageBackend}(img::Image{B})
    if img.finished
        return
    end

    if(img.owns_surface)
        Cairo.destroy(img.ctx)
    end
    finish(B, img)
    if(img.owns_surface)
        Cairo.destroy(img.surface)
    end

    img.finished = true

    if img.emit_on_finish && typeof(img.out) == IOString
        display(img)
    end

    if method_exists(flush, (typeof(img.out),))
        flush(img.out)
    end
end


function newsurface{B}(::Type{B}, out, width, height)
        local surface::CairoSurface
        if B == SVGBackend
            surface = CairoSVGSurface(out, width, height)
        elseif B == PNGBackend
            surface = CairoARGBSurface(iround(width), iround(height))
        elseif B == PDFBackend
            surface = CairoPDFSurface(out, width, height)
        elseif B == PSBackend
            surface = CairoEPSSurface(out, width, height)
        else
            error("Unkown Cairo backend.")
        end

        if Cairo.status(surface) != Cairo.STATUS_SUCCESS
            error("Unable to create cairo surface.")
        end

        surface
end


function reset{B}(img::Image{B})
    if !img.owns_surface
        error("Backend can't be reused since an external cairo surface is being used.")
    end

    try
        seekstart(img.out)
    catch
        error("Backend can't be reused, since the output stream is not seekable.")
    end

    img.surface = newsurface(B, img.out, img.width, img.height)
    img.ctx = CairoContext(img.surface)
    img.finished = false
end


function isfinished(img::Image)
    img.finished
end


function writemime(io::IO, ::MIME"image/png", img::PNG)
    write(io, takebuf_string(img.out))
end


function writemime(io::IO, ::MIME"application/pdf", img::PDF)
    write(io, takebuf_string(img.out))
end


function writemime(io::IO, ::MIME"application/postscript", img::PS)
    write(io, takebuf_string(img.out))
end


# sizes

function root_box{B}(img::Image{B})
    AbsoluteBoundingBox(0.0, 0.0, width(img), height(img))
end


# Drawing

function move_to(img::Image, point::Point)
    Cairo.move_to(
        img.ctx,
        absolute_native_units(img, point.x.abs),
        absolute_native_units(img, point.y.abs))
end


function line_to(img::Image, point::Point)
    Cairo.line_to(
        img.ctx,
        absolute_native_units(img, point.x.abs),
        absolute_native_units(img, point.y.abs))
end


function curve_to(img::Image, ctrl1::Point, ctrl2::Point, anchor::Point)
    Cairo.curve_to(
        img.ctx,
        absolute_native_units(img, ctrl1.x.abs),
        absolute_native_units(img, ctrl1.y.abs),
        absolute_native_units(img, ctrl2.x.abs),
        absolute_native_units(img, ctrl2.y.abs),
        absolute_native_units(img, anchor.x.abs),
        absolute_native_units(img, anchor.y.abs))
end


function close_path(img::Image)
    Cairo.close_path(img.ctx)
end


function arc(img::Image, x::Float64, y::Float64, radius::Float64,
                angle1::Float64, angle2::Float64)
    Cairo.arc(
        img.ctx,
        absolute_native_units(img, x),
        absolute_native_units(img, y),
        absolute_native_units(img, radius),
        absolute_native_units(img, angle1),
        absolute_native_units(img, angle2))
end


function translate(img::Image, tx::Float64, ty::Float64)
    Cairo.translate(
        img.ctx,
        absolute_native_units(img, tx),
        absolute_native_units(img, ty))
end


function scale(img::Image, sx::Float64, sy::Float64)
    Cairo.scale(img.ctx, sx, sy)
end


function rotate(img::Image, theta::Float64)
    Cairo.rotate(img.ctx, theta)
end


function fillstroke(img::Image)
    if img.fill != nothing && img.opacity > 0.0 && img.visible
        rgb = convert(RGB, img.fill)
        Cairo.set_source_rgba(img.ctx, rgb.r, rgb.g, rgb.b, img.opacity)

        if img.stroke != nothing
            Cairo.fill_preserve(img.ctx)
        else
            Cairo.fill(img.ctx)
        end
    end

    if img.stroke != nothing && img.stroke_opacity > 0.0
        rgb = convert(RGB, img.stroke)
        Cairo.set_source_rgba(img.ctx, rgb.r, rgb.g, rgb.b, img.stroke_opacity)

        Cairo.stroke(img.ctx)
    end
end


function save_state(img::Image)
    push!(img.state_stack, ImagePropertyState(img.stroke, img.fill))
    Cairo.save(img.ctx)
end


function restore_state(img::Image)
    state = pop!(img.state_stack)
    img.stroke = state.stroke
    img.fill = state.fill
    Cairo.restore(img.ctx)
end


function draw(img::Image, form::Lines)
    if isempty(form.points); return; end

    paths = make_paths(form.points)
    for path in paths
        move_to(img, path[1])
        for point in path[2:]
            line_to(img, point)
        end
        fillstroke(img)
    end
end


function draw(img::Image, form::Curve)
    move_to(img, form.anchor0)
    curve_to(img, form.ctrl0, form.ctrl1, form.anchor1)
end


function draw(img::Image, form::Polygon)
    if isempty(form.points); return; end

    move_to(img, form.points[1])
    for point in form.points[2:]
        line_to(img, point)
    end
    close_path(img)
    fillstroke(img)
end


function draw(img::Image, form::Ellipse)
    cx = form.center.x.abs
    cy = form.center.y.abs
    rx = sqrt((form.x_point.x.abs - cx)^2 +
              (form.x_point.y.abs - cy)^2)
    ry = sqrt((form.y_point.x.abs - cx)^2 +
              (form.y_point.y.abs - cy)^2)
    theta = atan2(form.x_point.y.abs - cy,
                  form.x_point.x.abs - cx)

    save_state(img)
    translate(img, cx, cy)
    rotate(img, theta)
    translate(img, -rx, -ry)
    scale(img, 2rx, 2ry)
    arc(img, 0.5, 0.5, 0.5, 0.0, 2pi)
    restore_state(img)
    fillstroke(img)
end


function get_layout_size(img::PNG)
    width, height = Cairo.get_layout_size(img.ctx)
    width / assumed_ppmm, height / assumed_ppmm
end


function get_layout_size(img::Image)
    width, height = Cairo.get_layout_size(img.ctx)
    25.4 * width / 72, 25.4 * height / 72
end


function draw(img::Image, form::Text)
    if !img.visible || img.opacity == 0.0 || img.fill === nothing
        return
    end

    pos = copy(form.pos)
    Cairo.set_text(img.ctx, form.value, true)
    width, height = get_layout_size(img)
    pos = Point(pos.x, Measure(abs=pos.y.abs - height))

    if form.halign != hleft || form.valign != vtop
        if form.halign == hcenter
            pos = Point(Measure(abs=pos.x.abs - width/2), pos.y)
        elseif form.halign == hright
            pos = Point(Measure(abs=pos.x.abs - width), pos.y)
        end

        if form.valign == vcenter
            pos = Point(pos.x, Measure(abs=pos.y.abs + height/2))
        elseif form.valign == vtop
            pos = Point(pos.x, Measure(abs=pos.y.abs + height))
        end
    end

    Cairo.set_text(img.ctx, form.value, true)
    rgb = convert(RGB, img.fill)
    Cairo.set_source_rgba(img.ctx, rgb.r, rgb.g, rgb.b, img.opacity)

    save_state(img)
    translate(img, form.t.M[1,3], form.t.M[2,3])
    rotate(img, atan2(form.t.M[2,1], form.t.M[1,1]))

    move_to(img, pos)
    Cairo.show_layout(img.ctx)
    restore_state(img)
end


# Applying properties


function push_property(img::Image, p::Property)
    save_state(img)
    while !is(p, empty_property)
        apply_property(img, p.primitive)
        p = p.next
    end
end


function pop_property(img::Image)
    restore_state(img)
end


# Nop catchall
function apply_property(img::Image, p::PropertyPrimitive)
end


function apply_property(img::Image, p::Stroke)
    img.stroke = p.value
end


function apply_property(img::Image, p::Fill)
    img.fill = p.value
end


function apply_property(img::Image, p::Opacity)
    img.opacity = p.value
end


function apply_property(img::Image, p::StrokeOpacity)
    img.stroke_opacity = p.value
end


function apply_property(img::Image, p::Visible)
    img.visible = p.value
end


function apply_property(img::Image, property::LineWidth)
    Cairo.set_line_width(
        img.ctx,
        absolute_native_units(img, property.value.abs))
end


function apply_property(img::Image, property::Font)
    Cairo.set_font_face(img.ctx, property.family)
end


function apply_property(img::Image, property::FontSize)
    font_desc = ccall((:pango_layout_get_font_description, Cairo._jl_libpango),
                      Ptr{Void}, (Ptr{Void},), img.ctx.layout)

    if font_desc == C_NULL
        family = "sans"
    else
        family = ccall((:pango_font_description_get_family, Cairo._jl_libpango),
                       Ptr{Uint8}, (Ptr{Void},), font_desc)
        family = bytestring(family)
    end

    Cairo.set_font_face(img.ctx,
        @sprintf("%s %.2f",
            family,
            absolute_native_units(img, property.value.abs)))
end


function apply_property(img::Image, property::Clip)
    if isempty(property.points); return; end
    move_to(img, property.points[1])
    for point in property.points[2:]
        line_to(img, point)
    end
    close_path(img)
    Cairo.clip(img.ctx)
end
