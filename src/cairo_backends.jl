using Compat

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
abstract CairoBackend <: VectorImageBackend

type ImagePropertyState
    stroke::RGBA{Float64}
    fill::RGBA{Float64}
    stroke_dash::Array{Float64,1}
    stroke_linecap::LineCap
    stroke_linejoin::LineJoin
    visible::Bool
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
    stroke::RGBA{Float64}
    fill::RGBA{Float64}
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

    # Points (or pixels for PNG) per mm
    ppmm::Float64

    # For use with the t/T and s/S commands in SVG-style paths
    last_ctrl1_point::Union(Point, Nothing)
    last_ctrl2_point::Union(Point, Nothing)

    function Image(surface::CairoSurface, ctx::CairoContext, out::IO)
        img = new()
        img.out = out
        img.width = 0
        img.height = 0
        img.surface = surface
        img.ctx = ctx
        img.stroke = default_stroke_color == nothing ?
                        RGBA{Float64}(0, 0, 0, 0) : convert(RGBA{Float64}, default_stroke_color)
        img.fill   = default_fill_color == nothing ?
                        RGBA{Float64}(0, 0, 0, 0) : convert(RGBA{Float64}, default_fill_color)
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
        img.ppmm = 72 / 25.4
        img.last_ctrl1_point = nothing
        img.last_ctrl2_point = nothing
        img
    end

    function Image(surface::CairoSurface, ctx::CairoContext)
        Image{B}(surface, ctx, IOBuffer())
    end

    Image(surface::CairoSurface) = Image{B}(surface, CairoContext(surface))


    function Image(out::IO,
                   width::MeasureOrNumber,
                   height::MeasureOrNumber,
                   emit_on_finish::Bool=true;
                   dpi = (B == PNGBackend ? 96 : 72))

        width = size_measure(width)
        height = size_measure(height)

        if !isabsolute(width) || !isabsolute(height)
            error("Image size must be specificed in absolute units.")
        end

        ppmm = dpi / 25.4
        width = width.abs * ppmm
        height = height.abs * ppmm

        surface = newsurface(B, out, width, height)
        img = Image{B}(surface, CairoContext(surface), out)
        img.width = width
        img.height = height
        img.owns_surface = true
        img.emit_on_finish = emit_on_finish
        img.ppmm = ppmm
        img
    end

    function Image(filename::String,
                   width::MeasureOrNumber,
                   height::MeasureOrNumber;
                   dpi = (B == PNGBackend ? 96 : 72))
        img = Image{B}(open(filename, "w"), width, height, dpi = dpi)
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

    function Image(c::CairoSurface)
        img = Image{B}(c,CairoContext(c))
        img
    end
end

typealias PNG Image{PNGBackend}
typealias PDF Image{PDFBackend}
typealias PS  Image{PSBackend}
typealias CAIROSURFACE  Image{CairoBackend}


# convert compose absolute units (millimeters) to the absolute units used by the
# cairo surface (pixels for PNG, points for all others)

function absolute_native_units{B}(img::Image{B}, u::Float64)
    img.ppmm * u
end

function width(img::Image)
    Cairo.width(img.surface) / img.ppmm
end

function height(img::Image)
    Cairo.height(img.surface) / img.ppmm
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
            surface = CairoARGBSurface(round(Integer, width), round(Integer, height))
        elseif B == PDFBackend
            surface = CairoPDFSurface(out, width, height)
        elseif B == PSBackend
            surface = CairoEPSSurface(out, width, height)
        elseif B == CairoBackend
            surface = out
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
            img.stroke_dash,
            img.stroke_linecap,
            img.stroke_linejoin,
            img.visible))
    Cairo.save(img.ctx)
end


function restore_property_state(img::Image)
    state = pop!(img.state_stack)
    img.stroke = state.stroke
    img.fill = state.fill
    img.stroke_dash = state.stroke_dash
    img.stroke_linecap = state.stroke_linecap
    img.stroke_linejoin = state.stroke_linejoin
    img.visible = state.visible
    Cairo.restore(img.ctx)
end



# Return true if the vector properties need to be pushed and popped, rather
# than simply applied.
function vector_properties_require_push_pop(img::Image)
    for (propertytype, property) in img.vector_properties
        propertytype
        if in(propertytype, [Property{FontPrimitive},
                             Property{FontSizePrimitive},
                             Property{ClipPrimitive}])
            return true
        end
    end
    return false
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
    img.fill = RGBA{Float64}(img.fill.c, p.value)
end


function apply_property(img::Image, p::StrokeOpacityPrimitive)
    img.stroke = RGBA{Float64}(img.stroke.c, p.value)
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

function apply_property(img::Image, property::SVGIDPrimitive)
end

function apply_property(img::Image, property::SVGClassPrimitive)
end

function apply_property(img::Image, property::SVGAttributePrimitive)
end


# Cairo Wrappers
# --------------

function current_point(img::Image)
    x = Array(Float64, 1)
    y = Array(Float64, 1)
    ccall((:cairo_get_current_point, Cairo._jl_libcairo), Void,
          (Ptr{Void}, Ptr{Float64}, Ptr{Float64}), img.ctx.ptr, x, y)
    return Point(Measure(abs=x[1] / img.ppmm),
                 Measure(abs=y[1] / img.ppmm))
end


function move_to(img::Image, point::Point)
    Cairo.move_to(
        img.ctx,
        absolute_native_units(img, point.x.abs),
        absolute_native_units(img, point.y.abs))
end


function rel_move_to(img::Image, point::Point)
    Cairo.rel_move_to(
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


function rel_line_to(img::Image, point::Point)
    Cairo.rel_line_to(
        img.ctx,
        absolute_native_units(img, point.x.abs),
        absolute_native_units(img, point.y.abs))
end


function curve_to(img::Image, ctrl1::Point, ctrl2::Point, to::Point)
    Cairo.curve_to(
        img.ctx,
        absolute_native_units(img, ctrl1.x.abs),
        absolute_native_units(img, ctrl1.y.abs),
        absolute_native_units(img, ctrl2.x.abs),
        absolute_native_units(img, ctrl2.y.abs),
        absolute_native_units(img, to.x.abs),
        absolute_native_units(img, to.y.abs),)
end


function rel_curve_to(img::Image, ctrl1::Point, ctrl2::Point, to::Point)
    Cairo.rel_curve_to(
        img.ctx,
        absolute_native_units(img, ctrl1.x.abs),
        absolute_native_units(img, ctrl1.y.abs),
        absolute_native_units(img, ctrl2.x.abs),
        absolute_native_units(img, ctrl2.y.abs),
        absolute_native_units(img, to.x.abs),
        absolute_native_units(img, to.y.abs),)
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
        angle1, angle2)
end


function arc_negative(img::Image, x::Float64, y::Float64, radius::Float64,
                      angle1::Float64, angle2::Float64)
    Cairo.arc_negative(
        img.ctx,
        absolute_native_units(img, x),
        absolute_native_units(img, y),
        absolute_native_units(img, radius),
        angle1, angle2)
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


function fillstroke(img::Image, strokeonly::Bool=false)
    if !img.visible
        return
    end

    if img.fill.alpha > 0.0 && !strokeonly
        Cairo.set_source_rgba(img.ctx, img.fill.c.r, img.fill.c.g, img.fill.c.b,
                              img.fill.alpha)

        if img.stroke.alpha > 0.0
            Cairo.fill_preserve(img.ctx)
        else
            Cairo.fill(img.ctx)
        end
    end

    if img.stroke.alpha > 0.0
        Cairo.set_source_rgba(img.ctx, img.stroke.c.r, img.stroke.c.g,
                                     img.stroke.c.b, img.stroke.alpha)
        Cairo.set_dash(img.ctx, img.stroke_dash)
        Cairo.set_line_cap(img.ctx, cairo_linecap(img.stroke_linecap))
        Cairo.set_line_join(img.ctx, cairo_linejoin(img.stroke_linejoin))

        Cairo.stroke(img.ctx)
    end

    # if the path wasn't stroked or filled, we should still clear it
    Cairo.new_path(img.ctx)
end



# Form Drawing
# ------------


function draw(img::Image, form::Form)
    if vector_properties_require_push_pop(img)
        for (idx, primitive) in enumerate(form.primitives)
            push_vector_properties(img, idx)
            draw(img, primitive)
            pop_vector_properties(img)
        end
    else
        for (idx, primitive) in enumerate(form.primitives)
            for (propertytype, property) in img.vector_properties
                if property === nothing
                    continue
                end
                if idx > length(property.primitives)
                    error("Vector form and vector property differ in length. Can't distribute.")
                end
                apply_property(img, property.primitives[idx])
            end
            draw(img, primitive)
        end
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

function draw(img::Image, prim::Compose.ComplexPolygonPrimitive)
    if isempty(prim.rings); return; end

    for ring in prim.rings
        move_to(img, ring[1])
        for point in ring[2:end]
            line_to(img, point)
        end
        close_path(img)
    end
    fillstroke(img)
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

    save_property_state(img)
    translate(img, cx, cy)
    rotate(img, theta)
    translate(img, -rx, -ry)
    scale(img, 2rx, 2ry)
    arc(img, 0.5, 0.5, 0.5, 0.0, 2pi)
    restore_property_state(img)
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
        fillstroke(img, true)
    end
end


function get_layout_size(img::Image)
    width, height = Cairo.get_layout_size(img.ctx)
    width / img.ppmm, height / img.ppmm
end


function draw(img::Image, prim::TextPrimitive)
    if !img.visible || (img.fill.alpha == 0.0 && img.stroke.alpha == 0.0)
        return
    end

    pos = copy(prim.position)
    Cairo.set_text(img.ctx, prim.value, true)
    width, height = get_layout_size(img)
    pos = Point(pos.x, Measure(pos.y.abs - height))

    if prim.halign != hleft || prim.valign != vbottom
        if prim.halign == hcenter
            pos = Point(Measure(pos.x.abs - width/2), pos.y)
        elseif prim.halign == hright
            pos = Point(Measure(pos.x.abs - width), pos.y)
        end

        if prim.valign == vcenter
            pos = Point(pos.x, Measure(pos.y.abs + height/2))
        elseif prim.valign == vtop
            pos = Point(pos.x, Measure(pos.y.abs + height))
        end
    end

    Cairo.set_source_rgba(img.ctx, img.fill.c.r, img.fill.c.g, img.fill.c.b,
                          img.fill.alpha)

    if prim.rot.theta != 0.0
        save_property_state(img)
        rotate(img, prim.rot.theta,
               prim.rot.offset.x.abs, prim.rot.offset.y.abs)
    end

    move_to(img, pos)
    Cairo.show_layout(img.ctx)

    if prim.rot.theta != 0.0
        restore_property_state(img)
    end
end

function draw(img::Image, prim::CurvePrimitive)
    move_to(img, prim.anchor0)
    curve_to(img, prim.ctrl0, prim.ctrl1, prim.anchor1)
    fillstroke(img, true)
end

function draw(img::Image, prim::BitmapPrimitive)
    error("Embedding bitmaps in Cairo backends (i.e. PNG, PDF, PS) is not supported.")
end

function draw(img::Image, prim::ImageMatrixPrimitive)

    rectangle(img, prim.corner, prim.width, prim.height)
    s = Cairo.CairoRGBSurface(prim.data);
    p = Cairo.CairoPattern(s)
    w = absolute_native_units(img,prim.width.abs)
    h = absolute_native_units(img,prim.height.abs)
    m = Cairo.CairoMatrix(s.width/w,0,0,s.height/h,0,0)
    Cairo.set_matrix(p,m)
    Cairo.pattern_set_filter(p,Cairo.FILTER_NEAREST)        
    Cairo.set_source(img.ctx,p)
    Cairo.fill(img.ctx)

end

function draw(img::Image, prim::PathPrimitive)
    for op in prim.ops
        draw_path_op(img, op)
    end
    fillstroke(img)
end


function draw_path_op(img::Image, op::MoveAbsPathOp)
    move_to(img, op.to)
end


function draw_path_op(img::Image, op::MoveRelPathOp)
    rel_move_to(img, op.to)
end


function draw_path_op(img::Image, op::ClosePathOp)
    close_path(img)
end


function draw_path_op(img::Image, op::LineAbsPathOp)
    line_to(img, op.to)
end


function draw_path_op(img::Image, op::LineRelPathOp)
    rel_line_to(img, op.to)
end


function draw_path_op(img::Image, op::HorLineAbsPathOp)
    pos = current_point(img)
    line_to(img, Point(op.x, pos.y))
end


function draw_path_op(img::Image, op::HorLineRelPathOp)
    rel_line_to(img, Point(op.Δx, 0.0))
end


function draw_path_op(img::Image, op::VertLineAbsPathOp)
    pos = current_point(img)
    line_to(img, Point(pos.x, op.y))
end


function draw_path_op(img::Image, op::VertLineRelPathOp)
    rel_line_to(img, Point(0.0, op.Δy))
end


function draw_path_op(img::Image, op::CubicCurveAbsPathOp)
    curve_to(img, op.ctrl1, op.ctrl2, op.to)
    img.last_ctrl2_point = op.ctrl2
end


function draw_path_op(img::Image, op::CubicCurveRelPathOp)
    xy = current_point(img)
    rel_curve_to(img, op.ctrl1, op.ctrl2, op.to)
    img.last_ctrl2_point =
        Point(Measure(abs=op.ctrl2.x.abs + xy.x.abs),
              Measure(abs=op.ctrl2.y.abs + xy.y.abs))
end


function draw_path_op(img::Image, op::CubicCurveShortAbsPathOp)
    xy = current_point(img)
    x1, y1 = xy.x.abs, xy.y.abs
    x2, y2 = op.to.x.abs, op.to.y.abs

    ctrl1 = img.last_ctrl2_point
    if ctrl1 === nothing
        ctrl1 = xy
    else
        ctrl1 = Point(Measure(abs=2*x1 - ctrl1.x.abs),
                      Measure(abs=2*y1 - ctrl1.y.abs))
    end
    cx, cy = ctrl1.x.abs, ctrl1.y.abs

    curve_to(img, ctrl1, op.ctrl2, op.to)
    img.last_ctrl2_point = op.ctrl2
end


function draw_path_op(img::Image, op::CubicCurveShortRelPathOp)
    xy = current_point(img)
    x1, y1 = xy.x.abs, xy.y.abs
    x2, y2 = op.to.x.abs, op.to.y.abs

    ctrl1 = img.last_ctrl2_point
    if ctrl1 === nothing
        ctrl1 = xy
    else
        ctrl1 = Point(Measure(abs=(2*x1 - ctrl1.x.abs) - x1),
                      Measure(abs=(2*y1 - ctrl1.y.abs) - y1))
    end
    cx, cy = ctrl1.x.abs, ctrl1.y.abs

    rel_curve_to(img, ctrl1, op.ctrl2, op.to)
    img.last_ctrl2_point =
        Point(Measure(abs=op.ctrl2.x.abs + xy.x.abs),
              Measure(abs=op.ctrl2.y.abs + xy.y.abs))
end


function draw_path_op(img::Image, op::QuadCurveAbsPathOp)
    xy = current_point(img)
    x1, y1 = xy.x.abs, xy.y.abs
    x2, y2 = op.to.x.abs, op.to.y.abs
    cx, cy = op.ctrl1.x.abs, op.ctrl1.y.abs
    curve_to(img,
             Point(Measure(abs=(x1 + 2*cx)/3),
                   Measure(abs=(y1 + 2*cy)/3)),
             Point(Measure(abs=(x2 + 2*cx)/3),
                   Measure(abs=(y2 + 2*cy)/3)),
             op.to)
    img.last_ctrl1_point = op.ctrl1
end


function draw_path_op(img::Image, op::QuadCurveRelPathOp)
    xy = current_point(img)
    x1, y1 = xy.x.abs, xy.y.abs
    x2, y2 = op.to.x.abs, op.to.y.abs
    cx, cy = op.ctrl1.x.abs, op.ctrl1.y.abs
    rel_curve_to(img,
             Point(Measure(abs=(x1 + 2*cx)/3),
                   Measure(abs=(y1 + 2*cy)/3)),
             Point(Measure(abs=(x2 + 2*cx)/3),
                   Measure(abs=(y2 + 2*cy)/3)),
             op.to)
    img.last_ctrl1_point =
        Point(Measure(abs=op.ctrl1.x.abs + xy.x.abs),
              Measure(abs=op.ctrl1.y.abs + xy.y.abs))
end


function draw_path_op(img::Image, op::QuadCurveShortAbsPathOp)
    xy = current_point(img)
    x1, y1 = xy.x.abs, xy.y.abs
    x2, y2 = op.to.x.abs, op.to.y.abs

    ctrl1 = img.last_ctrl1_point
    if ctrl1 === nothing
        ctrl1 = xy
    else
        ctrl1 = Point(Measure(abs=2*x1 - ctrl1.x.abs),
                      Measure(abs=2*y1 - ctrl1.y.abs))
    end
    cx, cy = ctrl1.x.abs, ctrl1.y.abs

    curve_to(img,
             Point(Measure(abs=(x1 + 2*cx)/3),
                   Measure(abs=(y1 + 2*cy)/3)),
             Point(Measure(abs=(x2 + 2*cx)/3),
                   Measure(abs=(y2 + 2*cy)/3)),
             Point(Measure(abs=x2), Measure(abs=y2)))
    img.last_ctrl1_point = ctrl1
end


function draw_path_op(img::Image, op::QuadCurveShortRelPathOp)
    xy = current_point(img)
    x1, y1 = xy.x.abs, xy.y.abs
    x2, y2 = x1 + op.to.x.abs, y1 + op.to.y.abs

    ctrl1 = img.last_ctrl1_point
    if ctrl1 === nothing
        ctrl1 = xy
    else
        ctrl1 = Point(Measure(abs=(2*x1 - ctrl1.x.abs) - x1),
                      Measure(abs=(2*y1 - ctrl1.y.abs) - y1))
    end
    cx, cy = ctrl1.x.abs, ctrl1.y.abs

    rel_curve_to(img,
                 Point(Measure(abs=(x1 + 2*cx)/3),
                       Measure(abs=(y1 + 2*cy)/3)),
                 Point(Measure(abs=(x2 + 2*cx)/3),
                       Measure(abs=(y2 + 2*cy)/3)),
                 Point(Measure(abs=x2), Measure(abs=y2)))
    img.last_ctrl1_point =
        Point(Measure(abs=op.ctrl1.x.abs + x1),
              Measure(abs=op.ctrl1.y.abs + y1))
end


function draw_path_op(img::Image, op::ArcAbsPathOp)
    xy = current_point(img)
    x1, y1 = xy.x.abs, xy.y.abs
    x2, y2 = op.to.x.abs, op.to.y.abs
    rx, ry = op.rx.abs, op.ry.abs
    φ = deg2rad(op.rotation)
    draw_endpoint_arc(img, rx, ry, φ, op.largearc, op.sweep, x1, y1, x2, y2)
end


function draw_path_op(img::Image, op::ArcRelPathOp)
    xy = current_point(img)
    x1, y1 = xy.x.abs, xy.y.abs
    x2, y2 = x1 + op.to.x.abs, y1 + op.to.y.abs
    rx, ry = op.rx.abs, op.ry.abs
    φ = deg2rad(op.rotation)
    draw_endpoint_arc(img, rx, ry, φ, op.largearc, op.sweep, x1, y1, x2, y2)
end


# Draw an SVG style elliptical arc
function draw_endpoint_arc(img::Image, rx::Float64, ry::Float64, φ::Float64,
                           largearc::Bool, sweep::Bool,
                           x1::Float64, y1::Float64,
                           x2::Float64, y2::Float64)
    function uvangle(ux, uy, vx, vy)
        t = (ux * vx + uy * vy) / (sqrt(ux^2 + uy^2) * sqrt(vx^2 + vy^2))
        t = max(min(t, 1.0), -1.0)
        return (ux * vy - uy * vx < 0.0 ? -1 : 1.0) * acos(t)
    end

    # From: http://www.w3.org/TR/SVG/implnote.html#ArcConversionEndpointToCenter
    xm, ym = (x1 - x2)/2, (y1 - y2)/2
    x1p =  cos(φ) * xm + sin(φ) * ym
    y1p = -sin(φ) * xm + cos(φ) * ym

    u = (rx^2 * ry^2 - rx^2 * y1p^2 - ry^2 * x1p^2) / (rx^2 * y1p^2 + ry^2 * x1p^2)
    u = u >= 0.0 ? sqrt(u) : 0.0

    cxp =  u * (rx * y1p) / ry
    cyp = -u * (ry * x1p) / rx
    if sweep == largearc
        cxp = -cxp
        cyp = -cyp
    end
    cx = (x1 + x2)/2 + cos(φ) * cxp - sin(φ) * cyp
    cy = (y1 + y2)/2 + sin(φ) * cxp + cos(φ) * cyp

    θ1 = uvangle(1.0, 0.0, (x1p - cxp) / rx, (y1p - cyp) / ry)
    Δθ = uvangle((x1p - cxp) / rx, (y1p - cyp) / ry,
                 (-x1p - cxp) / rx, (-y1p - cyp) / ry) % (2.0*π)
    if Δθ > 0.0 && !sweep
        Δθ -= 2*π
    elseif Δθ < 0.0 && sweep
        Δθ += 2*π
    end

    Cairo.save(img.ctx)
    Cairo.translate(img.ctx,
                    absolute_native_units(img, cx),
                    absolute_native_units(img, cy))
    Cairo.rotate(img.ctx, φ)
    Cairo.scale(img.ctx, rx, ry)
    if sweep
        arc(img, 0.0, 0.0, 1.0, θ1, θ1 + Δθ)
    else
        arc_negative(img, 0.0, 0.0, 1.0, θ1, θ1 + Δθ)
    end
    Cairo.restore(img.ctx)
end


