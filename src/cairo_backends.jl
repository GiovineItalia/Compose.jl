
# Cairo backend for compose
import Cairo

import Cairo.CairoContext, Cairo.CairoSurface, Cairo.CairoARGBSurface,
       Cairo.CairoEPSSurface, Cairo.CairoPDFSurface, Cairo.CairoSVGSurface

abstract ImageBackend
abstract PNGBackend <: ImageBackend

abstract VectorImageBackend <: ImageBackend
abstract SVGBackend <: VectorImageBackend
abstract PDFBackend <: VectorImageBackend
abstract PSBackend  <: VectorImageBackend

type ImagePropertyState
    stroke::Maybe(ColorValue)
    fill::Maybe(ColorValue)
    fill_opacity::Float64
    stroke_opacity::Float64
    stroke_dash::Array{Float64,1}
    stroke_linecap::LineCap
    stroke_linejoin::LineJoin
end

type ImagePropertyFrame
    # Vector properties in this frame.
    vector_properties::Dict{Type, Property}

    # True if this property frame has scalar properties. Scalar properties are
    # emitted as a group (<g> tag) that must be closed when the frame is popped.
    has_scalar_properties::Bool

    function ImagePropertyFrame()
        return new(Dict{Type, Property}(), false)
    end
end

type Image{B <: ImageBackend} <: Backend
    out::IO
    surface::CairoSurface
    ctx::CairoContext

    width::Float64
    height::Float64

    # Current state
    stroke::Maybe(ColorValue)
    fill::Maybe(ColorValue)
    fill_opacity::Float64 # in [0,1]
    stroke_opacity::Float64 # in [0,1]
    stroke_dash::Array{Float64,1}
    stroke_linecap::LineCap
    stroke_linejoin::LineJoin
    visible::Bool

    # Keep track of property
    state_stack::Vector{ImagePropertyState}
    property_stack::Vector{ImagePropertyFrame}
    vector_properties::Dict{Type, Union(Nothing, Property)}

    # Close the surface when finished
    owns_surface::Bool

    # Backend is responsible for opening/closing the file
    ownedfile::Bool

    # Filename when ownedfile is true
    filename::Union(String, Nothing)

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
        img.stroke = default_stroke_color
        img.fill   = default_fill_color
        img.fill_opacity = 1.0
        img.stroke_opacity = 1.0
        img.stroke_dash = []
        img.stroke_linecap = LineCapButt()
        img.stroke_linejoin = LineJoinMiter()
        img.visible = true
        img.state_stack = Array(ImagePropertyState, 0)
        img.property_stack = Array(ImagePropertyFrame, 0)
        img.vector_properties = Dict{Type, Union(Nothing, Property)}()
        img.owns_surface = false
        img.ownedfile = false
        img.filename = nothing
        img.finished = false
        img.emit_on_finish = false
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
        img.ownedfile = true
        img.filename = filename
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


function iswithjs(img::Image)
    return false
end


function iswithousjs(img::Image)
    return true
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

    if img.emit_on_finish && typeof(img.out) == IOBuffer
        display(img)
    end

    if method_exists(flush, (typeof(img.out),))
        flush(img.out)
    end

    if img.ownedfile
        close(img.out)
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

    if img.ownedfile
        img.out = open(img.filename, "w")
    else
        try
            seekstart(img.out)
        catch
            error("Backend can't be reused, since the output stream is not seekable.")
        end
    end

    img.surface = newsurface(B, img.out, img.width, img.height)
    img.ctx = CairoContext(img.surface)
    img.finished = false
end


function isfinished(img::Image)
    img.finished
end


function root_box(img::Image)
    AbsoluteBoundingBox(0.0, 0.0, width(img), height(img))
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


# Applying Properties
# -------------------

function push_property_frame(img::Image, properties::Vector{Property})
    if isempty(properties)
        return
    end

    frame = ImagePropertyFrame()
    applied_properties = Set{Type}()
    scalar_properties = Array(Property, 0)
    for property in properties
        if isscalar(property) && !(typeof(property) in applied_properties)
            push!(scalar_properties, property)
            push!(applied_properties, typeof(property))
            frame.has_scalar_properties = true
        elseif !isscalar(property)
            frame.vector_properties[typeof(property)] = property
            img.vector_properties[typeof(property)] = property
        end
    end
    push!(img.property_stack, frame)
    if isempty(scalar_properties)
        return
    end

    save_property_state(img)
    for property in scalar_properties
        apply_property(img, property.primitives[1])
    end
end


function pop_property_frame(img::Image)
    @assert !isempty(img.property_stack)
    frame = pop!(img.property_stack)

    if frame.has_scalar_properties
        restore_property_state(img)
    end

    for (propertytype, property) in frame.vector_properties
        img.vector_properties[propertytype] = nothing
        for i in length(img.property_stack):-1:1
            if haskey(img.property_stack[i].vector_properties, propertytype)
                img.vector_properties[propertytype] =
                    img.property_stack[i].vector_properties[propertytype]
            end
        end
    end
end


function save_property_state(img::Image)
    push!(img.state_stack,
        ImagePropertyState(
            img.stroke,
            img.fill,
            img.fill_opacity,
            img.stroke_opacity,
            img.stroke_dash,
            img.stroke_linecap,
            img.stroke_linejoin))
    Cairo.save(img.ctx)
end


function restore_property_state(img::Image)
    state = pop!(img.state_stack)
    img.stroke = state.stroke
    img.fill = state.fill
    img.fill_opacity = state.fill_opacity
    img.stroke_opacity = state.stroke_opacity
    img.stroke_dash = state.stroke_dash
    img.stroke_linecap = state.stroke_linecap
    img.stroke_linejoin = state.stroke_linejoin
    Cairo.restore(img.ctx)
end


function push_vector_properties(img::Image, idx::Int)
    save_property_state(img)
    for (propertytype, property) in img.vector_properties
        if property === nothing
            continue
        end
        if idx > length(property.primitives)
            error("Vector form and vector property differ in length. Can't distribute.")
        end
        apply_property(img, property.primitives[idx])
    end
end


function pop_vector_properties(img::Image)
    restore_property_state(img)
end


function apply_property(img::Image, p::StrokePrimitive)
    img.stroke = p.color
end


function apply_property(img::Image, p::FillPrimitive)
    img.fill = p.color
end


function apply_property(img::Image, p::FillOpacityPrimitive)
    img.fill_opacity = p.value
end


function apply_property(img::Image, p::StrokeOpacityPrimitive)
    img.stroke_opacity = p.value
end


function apply_property(img::Image, p::StrokeDashPrimitive)
    img.stroke_dash = map(v -> absolute_native_units(img, v.abs), p.value)
end


function apply_property(img::Image, p::StrokeLineCapPrimitive)
    img.stroke_linecap = p.value
end


function apply_property(img::Image, p::StrokeLineJoinPrimitive)
    img.stroke_linejoin = p.value
end


function apply_property(img::Image, p::VisiblePrimitive)
    img.visible = p.value
end


function apply_property(img::Image, property::LineWidthPrimitive)
    Cairo.set_line_width(
        img.ctx,
        absolute_native_units(img, property.value.abs))
end


function apply_property(img::Image, property::FontPrimitive)
    font_desc = ccall((:pango_layout_get_font_description, Cairo._jl_libpango),
                      Ptr{Void}, (Ptr{Void},), img.ctx.layout)

    if font_desc == C_NULL
        size = absolute_native_units(img, default_font_size.abs)
    else
        size = ccall((:pango_font_description_get_size, Cairo._jl_libpango),
                     Cint, (Ptr{Void},), font_desc)
    end

    Cairo.set_font_face(img.ctx,
        @sprintf("%s %0.2fpx", property.family, size / PANGO_SCALE))
end


function apply_property(img::Image, property::FontSizePrimitive)
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
        @sprintf("%s %.2fpx",
            family, absolute_native_units(img, property.value.abs)))
end


function apply_property(img::Image, property::ClipPrimitive)
    if isempty(property.points); return; end
    move_to(img, property.points[1])
    for point in property.points[2:end]
        line_to(img, point)
    end
    close_path(img)
    Cairo.clip(img.ctx)
end


# No-op SVG+JS only properties
function apply_property(img::Image, property::JSIncludePrimitive)
end

function apply_property(img::Image, property::JSCallPrimitive)
end

function apply_property(img::Image, property::SVGClassPrimitive)
end

function apply_property(img::Image, property::SVGAttributePrimitive)
end


# Cairo Wrappers
# --------------

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


function rectangle(img::Image, corner::Point, width::Measure, height::Measure)
    Cairo.rectangle(img.ctx,
                    absolute_native_units(img, corner.x.abs),
                    absolute_native_units(img, corner.y.abs),
                    absolute_native_units(img, width.abs),
                    absolute_native_units(img, height.abs))
end


function circle(img::Image, center::Point, radius::Measure)
    Cairo.circle(img.ctx,
                 absolute_native_units(img, center.x.abs),
                 absolute_native_units(img, center.y.abs),
                 absolute_native_units(img, radius.abs))
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


function rotate(img::Image, theta::Float64, x::Float64, y::Float64)
    ct = cos(theta)
    st = sin(theta)

    x′ = x - (ct * x - st * y)
    y′ = y - (st * x + ct * y)

    translate(img, x′, y′)
    rotate(img, theta)
end


# Convert native linecap/linejoin enums to the Cairo values.
cairo_linecap(::LineCapButt) = Cairo.CAIRO_LINE_CAP_BUTT
cairo_linecap(::LineCapRound) = Cairo.CAIRO_LINE_CAP_ROUND
cairo_linecap(::LineCapSquare) = Cairo.CAIRO_LINE_CAP_SQUARE
cairo_linejoin(::LineJoinMiter) = Cairo.CAIRO_LINE_JOIN_MITER
cairo_linejoin(::LineJoinBevel) = Cairo.CAIRO_LINE_JOIN_BEVEL
cairo_linejoin(::LineJoinRound) = Cairo.CAIRO_LINE_JOIN_ROUND


function fillstroke(img::Image)
    if img.fill != nothing && img.fill_opacity > 0.0 && img.visible
        rgb = convert(RGB, img.fill)
        Cairo.set_source_rgba(img.ctx, rgb.r, rgb.g, rgb.b, img.fill_opacity)

        if img.stroke != nothing
            Cairo.fill_preserve(img.ctx)
        else
            Cairo.fill(img.ctx)
        end
    end

    if img.stroke != nothing && img.stroke_opacity > 0.0
        rgb = convert(RGB, img.stroke)
        Cairo.set_source_rgba(img.ctx, rgb.r, rgb.g, rgb.b, img.stroke_opacity)
        Cairo.set_dash(img.ctx, img.stroke_dash)
        Cairo.set_line_cap(img.ctx, cairo_linecap(img.stroke_linecap))
        Cairo.set_line_join(img.ctx, cairo_linejoin(img.stroke_linejoin))

        Cairo.stroke(img.ctx)
    end

    # if the path wasn't stroked or filled, we should still clear it
    Cairo.new_path(img.ctx)
end


function save_state(img::Image)
    push!(img.state_stack,
        ImagePropertyState(
            img.stroke,
            img.fill,
            img.fill_opacity,
            img.stroke_opacity,
            img.stroke_dash,
            img.stroke_linecap,
            img.stroke_linejoin))
    Cairo.save(img.ctx)
end


function restore_state(img::Image)
    state = pop!(img.state_stack)
    img.stroke = state.stroke
    img.fill = state.fill
    img.fill_opacity = state.fill_opacity
    img.stroke_opacity = state.stroke_opacity
    img.stroke_dash = state.stroke_dash
    img.stroke_linecap = state.stroke_linecap
    img.stroke_linejoin = state.stroke_linejoin
    Cairo.restore(img.ctx)
end


# Form Drawing
# ------------


function draw(img::Image, form::Form)
    for (idx, primitive) in enumerate(form.primitives)
        push_vector_properties(img, idx)
        draw(img, primitive)
        pop_vector_properties(img)
    end
end


function draw(img::Image, prim::RectanglePrimitive)
    rectangle(img, prim.corner, prim.width, prim.height)
    fillstroke(img)
end


function draw(img::Image, prim::PolygonPrimitive)
    if isempty(prim.points); return; end

    paths = make_paths(prim.points)
    for path in paths
        move_to(img, path[1])
        for point in path[2:end]
            line_to(img, point)
        end
        close_path(img)
        fillstroke(img)
    end
end


function draw(img::Image, prim::CirclePrimitive)
    circle(img, prim.center, prim.radius)
    fillstroke(img)
end


function draw(img::Image, prim::EllipsePrimitive)
    cx = prim.center.x.abs
    cy = prim.center.y.abs
    rx = sqrt((prim.x_point.x.abs - cx)^2 +
              (prim.x_point.y.abs - cy)^2)
    ry = sqrt((prim.y_point.x.abs - cx)^2 +
              (prim.y_point.y.abs - cy)^2)
    theta = atan2(prim.x_point.y.abs - cy,
                  prim.x_point.x.abs - cx)

    if !all(isfinite([cx, cy, rx, ry, theta]))
        return
    end

    save_state(img)
    translate(img, cx, cy)
    rotate(img, theta)
    translate(img, -rx, -ry)
    scale(img, 2rx, 2ry)
    arc(img, 0.5, 0.5, 0.5, 0.0, 2pi)
    restore_state(img)
    fillstroke(img)
end


function draw(img::Image, prim::LinePrimitive)
    if length(prim.points) <= 1; return; end

    paths = make_paths(prim.points)
    for path in paths
        move_to(img, path[1])
        for point in path[2:end]
            line_to(img, point)
        end
        fillstroke(img)
    end
end


function get_layout_size(img::PNG)
    width, height = Cairo.get_layout_size(img.ctx)
    width / assumed_ppmm, height / assumed_ppmm
end


function get_layout_size(img::Image)
    width, height = Cairo.get_layout_size(img.ctx)
    25.4 * width / 72, 25.4 * height / 72
end


function draw(img::Image, prim::TextPrimitive)
    if !img.visible || ((img.fill_opacity == 0.0 || img.fill === nothing) &&
            (img.stroke_opacity == 0.0 || img.stroke === nothing))
        return
    end

    pos = copy(prim.position)
    Cairo.set_text(img.ctx, prim.value, true)
    width, height = get_layout_size(img)
    pos = Point(pos.x, Measure(abs=pos.y.abs - height))

    if prim.halign != hleft || prim.valign != vbottom
        if prim.halign == hcenter
            pos = Point(Measure(abs=pos.x.abs - width/2), pos.y)
        elseif prim.halign == hright
            pos = Point(Measure(abs=pos.x.abs - width), pos.y)
        end

        if prim.valign == vcenter
            pos = Point(pos.x, Measure(abs=pos.y.abs + height/2))
        elseif prim.valign == vtop
            pos = Point(pos.x, Measure(abs=pos.y.abs + height))
        end
    end

    rgb = convert(RGB, img.fill)
    Cairo.set_source_rgba(img.ctx, rgb.r, rgb.g, rgb.b, img.fill_opacity)

    if prim.rot.theta != 0.0
        save_state(img)
        rotate(img, prim.rot.theta,
               prim.rot.offset.x.abs, prim.rot.offset.y.abs)
    end

    move_to(img, pos)
    Cairo.show_layout(img.ctx)

    if prim.rot.theta != 0.0
        restore_state(img)
    end
end

function draw(img::Image, prim::CurvePrimitive)
    move_to(img, prim.anchor0)
    curve_to(img, prim.ctrl0, prim.ctrl1, prim.anchor1)
end

function draw(img::Image, prim::BitmapPrimitive)
    error("Embedding bitmaps in Cairo backends (i.e. PNG, PDF, PS) is not supported.")
end


