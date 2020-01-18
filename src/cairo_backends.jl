import .Cairo: CairoContext, CairoSurface, CairoARGBSurface, CairoEPSSurface,
CairoPDFSurface, CairoSVGSurface, CairoImageSurface

abstract type ImageBackend end
abstract type PNGBackend <: ImageBackend end

abstract type VectorImageBackend <: ImageBackend end
abstract type SVGBackend <: VectorImageBackend end
abstract type PDFBackend <: VectorImageBackend end
abstract type PSBackend  <: VectorImageBackend end
abstract type CairoBackend <: VectorImageBackend end

mutable struct ImagePropertyState
    stroke::RGBA{Float64}
    fill::RGBA{Float64}
    stroke_dash::Array{Float64,1}
    stroke_linecap::LineCap
    stroke_linejoin::LineJoin
    visible::Bool
    linewidth::AbsoluteLength
    fontsize::AbsoluteLength
    font::AbstractString
    clip::Union{ClipPrimitive, Nothing}
    arrow::Bool
end

mutable struct ImagePropertyFrame
    # Vector properties in this frame.
    vector_properties::Dict{Type, Property}

    # True if this property frame has scalar properties. Scalar properties are
    # emitted as a group (<g> tag) that must be closed when the frame is popped.
    has_scalar_properties::Bool
end
ImagePropertyFrame() = ImagePropertyFrame(Dict{Type, Property}(), false)

mutable struct Image{B<:ImageBackend} <: Backend
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
    linewidth::AbsoluteLength
    fontsize::AbsoluteLength
    font::AbstractString
    clip::Union{ClipPrimitive, Nothing}
    arrow::Bool

    # Keep track of property
    state_stack::Vector{ImagePropertyState}
    property_stack::Vector{ImagePropertyFrame}
    vector_properties::Dict{Type, Union{Property, Nothing}}

    # Close the surface when finished
    owns_surface::Bool

    # Backend is responsible for opening/closing the file
    ownedfile::Bool

    # Filename when ownedfile is true
    filename::Union{AbstractString, Nothing}

    # True when finish has been called and no more drawing should occur
    finished::Bool

    # Emit on finish
    emit_on_finish::Bool

    # Points (or pixels for PNG) per mm
    ppmm::Float64

    # For use with the t/T and s/S commands in SVG-style paths
    last_ctrl1_point::Union{AbsoluteVec2, Nothing}
    last_ctrl2_point::Union{AbsoluteVec2, Nothing}
end

function Image{B}(surface::CairoSurface,
               ctx::CairoContext,
               out::IO;

               width = 0,
               height = 0,
               stroke = default_stroke_color == nothing ?
                   RGBA{Float64}(0, 0, 0, 0) : convert(RGBA{Float64}, default_stroke_color),
               fill = default_fill_color == nothing ?
                   RGBA{Float64}(0, 0, 0, 0) : convert(RGBA{Float64}, default_fill_color),
               stroke_dash = [],
               stroke_linecap = LineCapButt(),
               stroke_linejoin = LineJoinMiter(),
               visible = true,
               linewidth = default_line_width,
               fontsize = default_font_size,
               font = default_font_family,
               clip = nothing,
               arrow = false,
               state_stack = Array{ImagePropertyState}(undef, 0),
               property_stack = Array{ImagePropertyFrame}(undef, 0),
               vector_properties = Dict{Type, Union{Property, Nothing}}(),
               owns_surface = false,
               ownedfile = false,
               filename = nothing,
               finished = false,
               emit_on_finish = false,
               ppmm = 72 / 25.4,
               last_ctrl1_point = nothing,
               last_ctrl2_point = nothing) where B<:ImageBackend

    Image{B}(out,
          surface,
          ctx,
          width,
          height,
          stroke,
          fill,
          stroke_dash,
          stroke_linecap,
          stroke_linejoin,
          visible,
          linewidth,
          fontsize,
          font,
          clip,
          arrow,
          state_stack,
          property_stack,
          vector_properties,
          owns_surface,
          ownedfile,
          filename,
          finished,
          emit_on_finish,
          ppmm,
          last_ctrl1_point,
          last_ctrl2_point)
end

Image{B}(surface::CairoSurface, ctx::CairoContext) where {B<:ImageBackend} =
        Image{B}(surface, ctx, IOBuffer())
Image{B}(surface::CairoSurface) where {B<:ImageBackend} =
        Image{B}(surface, CairoContext(surface))

function Image{B}(out::IO,
            width::MeasureOrNumber=default_graphic_width,
            height::MeasureOrNumber=default_graphic_height,
            emit_on_finish::Bool=true;
            dpi = (B==PNGBackend ? 96 : 72),
            kwargs...) where B<:ImageBackend
    width = size_measure(width)
    height = size_measure(height)

    (!isa(width, AbsoluteLength) || !isa(height, AbsoluteLength)) &&
                error("Image size must be specificed in absolute units.")

    ppmm = dpi / 25.4
    width = width.value * ppmm
    height = height.value * ppmm

    surface = newsurface(B, out, width, height)
    Image{B}(surface, CairoContext(surface), out;
                width=width, height=height,
                owns_surface=true, emit_on_finish=emit_on_finish, ppmm=ppmm,
                kwargs...)
end

Image{B}(filename::AbstractString,
            width::MeasureOrNumber=default_graphic_width,
            height::MeasureOrNumber=default_graphic_height;
            dpi = (B==PNGBackend ? 96 : 72)) where {B<:ImageBackend} =
        Image{B}(open(filename, "w"), width, height, dpi=dpi; ownedfile=true, filename=filename)

Image{B}(width::MeasureOrNumber=default_graphic_width,
            height::MeasureOrNumber=default_graphic_height,
            emit_on_finish::Bool=true;
            dpi = (B==PNGBackend ? 96 : 72)) where {B<:ImageBackend} =
        Image{B}(IOBuffer(), width, height, emit_on_finish, dpi=dpi)



docfunc(func,abbr) = """
    $func([output::Union{AbstractString, IO}], width=√200cm, height=10cm; dpi=$(func==:PNG ? 96 : 72)) -> Backend

Create a $abbr backend. The output is normally passed to [`draw`](@ref).
Specify a filename using a string as the first argument.  Depends on `Cairo.jl`.

# Examples
```
using Cairo
c = compose(context(), circle())
draw($(func)("myplot.$(lowercase(String(func)))", 10cm, 5cm, dpi=250), c)
```
"""


for (func,abbr) in [(:PNG,"Portable Network Graphics"),
                    (:PDF,"Portable Document Format"),
                    (:PS,"Postscript")]
    backend = Symbol(func, "Backend")
    docstr = docfunc(func,abbr)
    @eval $func(args...; kwargs...) = Image{$backend}(args...; kwargs...)
    @eval @doc $docstr $func

end



const CAIROSURFACE = Image{CairoBackend}

function (img::Image)(x)
    draw(img, x)
end

function canbatch(img::Image)
    for vp in values(img.vector_properties)
        (vp === nothing) || return false
    end
    return true
end

# convert compose absolute units (millimeters) to the absolute units used by the
# cairo surface (pixels for PNG, points for all others)

absolute_native_units(img::Image, u::Float64) = img.ppmm * u

surface(img::Image) = img.surface

Measures.width(img::Image) = (Cairo.width(img.surface) / img.ppmm) * mm
Measures.height(img::Image) = (Cairo.height(img.surface) / img.ppmm) * mm

iswithjs(img::Image) = false
iswithousjs(img::Image) = true

finish(::Type{B}, img::Image) where {B<:ImageBackend} = nothing
finish(::Type{PNGBackend}, img::Image) = Cairo.write_to_png(img.surface, img.out)

function finish(img::Image{B}) where B<:ImageBackend
    img.finished && return

    img.owns_surface && Cairo.destroy(img.ctx)
    finish(B, img)
    img.owns_surface && Cairo.destroy(img.surface)

    img.finished = true
    img.emit_on_finish && typeof(img.out) == IOBuffer && display(img)
    hasmethod(flush, (typeof(img.out),)) && flush(img.out)
    img.ownedfile && close(img.out)
end

function newsurface(::Type{B}, out, width, height) where B
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

    Cairo.status(surface) == Cairo.STATUS_SUCCESS || error("Unable to create cairo surface.")

    surface
end

function reset(img::Image{B}) where B
    img.owns_surface ||
            error("Backend can't be reused since an external cairo surface is being used.")

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

isfinished(img::Image) = img.finished

root_box(img::Image) = BoundingBox(width(img), height(img))

show(io::IO, ::MIME"image/png", img::Image{PNGBackend}) = write(io, String(take!(img.out)))
show(io::IO, ::MIME"application/pdf", img::Image{PDFBackend}) = write(io, String(take!(img.out)))
show(io::IO, ::MIME"application/postscript", img::Image{PSBackend}) = write(io, String(take!(img.out)))


# Applying Properties
# -------------------

function push_property_frame(img::Image, properties::Vector{Property})
    isempty(properties) && return

    frame = ImagePropertyFrame()
    isp = isscalar.(properties)
    scalar_properties = Dict{Type, Property}(typeof(x)=>x for x in properties[isp])
    vector_properties = Dict{Type, Property}(typeof(x)=>x for x in properties[.!isp])

    kt = [Property{FillOpacityPrimitive}, Property{FillPrimitive}]
    if haskey(scalar_properties, kt[1]) && haskey(vector_properties, kt[2])
        alpha = scalar_properties[kt[1]].primitives[1].value
        vector_properties[kt[1]] = fillopacity(fill(alpha, length(vector_properties[kt[2]].primitives)))
        pop!(scalar_properties, kt[1])
    end

    frame.has_scalar_properties = !isempty(scalar_properties)
    img.vector_properties = vector_properties
    frame.vector_properties = vector_properties
    push!(img.property_stack, frame)
    isempty(scalar_properties) && return

    save_property_state(img)
    for (_, property) in scalar_properties
        apply_property(img, property.primitives[1])
    end
    haskey(scalar_properties, kt[1]) && apply_property(img, scalar_properties[kt[1]].primitives[1])
end


function pop_property_frame(img::Image)
    @assert !isempty(img.property_stack)
    frame = pop!(img.property_stack)

    frame.has_scalar_properties && restore_property_state(img)

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
            img.visible,
            img.linewidth,
            img.fontsize,
            img.font,
            img.clip,
            img.arrow))
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
    img.linewidth = state.linewidth
    img.fontsize = state.fontsize
    img.font = state.font
    img.clip = state.clip
    img.arrow = state.arrow
    Cairo.restore(img.ctx)
end

# Return true if the vector properties need to be pushed and popped, rather
# than simply applied.
function vector_properties_require_push_pop(img::Image)
    for (propertytype, property) in img.vector_properties
        in(propertytype, [Property{FontPrimitive},
                          Property{FontSizePrimitive},
                          Property{ClipPrimitive}]) && return true
    end
    return false
end

function push_vector_properties(img::Image, idx::Int)
    save_property_state(img)
    for (propertytype, property) in img.vector_properties
        (property === nothing) && continue
        primitives = property.primitives
        idx > length(primitives) &&
                error("Vector form and vector property differ in length. Can't distribute.")
        apply_property(img, primitives[idx])
    end
end

pop_vector_properties(img::Image) = restore_property_state(img)
apply_property(img::Image, p::StrokePrimitive) =
        img.stroke = p.color
apply_property(img::Image, p::FillPrimitive) =
        img.fill = p.color
apply_property(img::Image, p::FillOpacityPrimitive) =
        img.fill = RGBA{Float64}(color(img.fill), p.value)
apply_property(img::Image, p::StrokeOpacityPrimitive) =
        img.stroke = RGBA{Float64}(color(img.stroke), p.value)
apply_property(img::Image, p::StrokeDashPrimitive) =
        img.stroke_dash = map(v -> absolute_native_units(img, v.value), p.value)
apply_property(img::Image, p::StrokeLineCapPrimitive) =
        img.stroke_linecap = p.value
apply_property(img::Image, p::StrokeLineJoinPrimitive) =
        img.stroke_linejoin = p.value
apply_property(img::Image, p::VisiblePrimitive) =
        img.visible = p.value

function apply_property(img::Image, property::LineWidthPrimitive)
    img.linewidth = property.value
    Cairo.set_line_width(img.ctx,
        absolute_native_units(img, property.value.value))
end

function apply_property(img::Image, property::FontPrimitive)
    img.font = property.family

    font_desc = ccall((:pango_layout_get_font_description, Cairo.libpango),
                      Ptr{Cvoid}, (Ptr{Cvoid},), img.ctx.layout)

    if font_desc == C_NULL
        size = absolute_native_units(img, default_font_size.value)
    else
        size = ccall((:pango_font_description_get_size, Cairo.libpango),
                     Cint, (Ptr{Cvoid},), font_desc)
    end

    Cairo.set_font_face(img.ctx,
        @sprintf("%s %0.2fpx", property.family, size / PANGO_SCALE))
end

function apply_property(img::Image, property::FontSizePrimitive)
    img.fontsize = property.value

    font_desc = ccall((:pango_layout_get_font_description, Cairo.libpango),
                      Ptr{Cvoid}, (Ptr{Cvoid},), img.ctx.layout)

    if font_desc == C_NULL
        family = "sans"
    else
        family = ccall((:pango_font_description_get_family, Cairo.libpango),
                       Ptr{UInt8}, (Ptr{Cvoid},), font_desc)
        family = unsafe_string(family)
    end

    Cairo.set_font_face(img.ctx,
        @sprintf("%s %.2fpx",
            family, absolute_native_units(img, property.value.value)))
end

function apply_property(img::Image, property::ClipPrimitive)
    if isempty(property.points); return; end
    move_to(img, property.points[1])
    for point in property.points[2:end]
        line_to(img, point)
    end
    close_path(img)
    Cairo.clip(img.ctx)
    img.clip = property
end

# No-op SVG+JS only properties
apply_property(img::Image, property::JSIncludePrimitive) = nothing
apply_property(img::Image, property::JSCallPrimitive) = nothing
apply_property(img::Image, property::SVGIDPrimitive) = nothing
apply_property(img::Image, property::SVGClassPrimitive) = nothing
apply_property(img::Image, property::SVGAttributePrimitive) = nothing


# Cairo Wrappers
# --------------

function current_point(img::Image)
    x = Array{Float64}(undef, 1)
    y = Array{Float64}(undef, 1)
    ccall((:cairo_get_current_point, Cairo.libcairo), Cvoid,
          (Ptr{Cvoid}, Ptr{Float64}, Ptr{Float64}), img.ctx.ptr, x, y)
    return ((x[1] / img.ppmm)*mm, (x[2] / img.ppmm)*mm)
end

move_to(img::Image, point::AbsoluteVec2) = Cairo.move_to(img.ctx,
        absolute_native_units(img, point[1].value),
        absolute_native_units(img, point[2].value))

rel_move_to(img::Image, point::AbsoluteVec2) = Cairo.rel_move_to(img.ctx,
        absolute_native_units(img, point[1].value),
        absolute_native_units(img, point[2].value))

line_to(img::Image, point::AbsoluteVec2) = Cairo.line_to(img.ctx,
        absolute_native_units(img, point[1].value),
        absolute_native_units(img, point[2].value))

rel_line_to(img::Image, point::AbsoluteVec2) = Cairo.rel_line_to(img.ctx,
        absolute_native_units(img, point[1].value),
        absolute_native_units(img, point[2].value))

rel_curve_to(img::Image, ctrl1::AbsoluteVec2, ctrl2::AbsoluteVec2, to::AbsoluteVec2) =
        Cairo.rel_curve_to(img.ctx,
            absolute_native_units(img, ctrl1[1].value),
            absolute_native_units(img, ctrl1[2].value),
            absolute_native_units(img, ctrl2[1].value),
            absolute_native_units(img, ctrl2[2].value),
            absolute_native_units(img, to[1].value),
            absolute_native_units(img, to[2].value),)

rectangle(img::Image, corner::AbsoluteVec2, width::AbsoluteLength, height::AbsoluteLength) =
        Cairo.rectangle(img.ctx,
            absolute_native_units(img, corner[1].value),
            absolute_native_units(img, corner[2].value),
            absolute_native_units(img, width.value),
            absolute_native_units(img, height.value))

circle(img::Image, center::AbsoluteVec2, radius::AbsoluteLength) =
        Cairo.circle(img.ctx,
            absolute_native_units(img, center[1].value),
            absolute_native_units(img, center[2].value),
            absolute_native_units(img, radius.value))

curve_to(img::Image, ctrl1::AbsoluteVec2, ctrl2::AbsoluteVec2, anchor::AbsoluteVec2) =
        Cairo.curve_to(img.ctx,
            absolute_native_units(img, ctrl1[1].value),
            absolute_native_units(img, ctrl1[2].value),
            absolute_native_units(img, ctrl2[1].value),
            absolute_native_units(img, ctrl2[2].value),
            absolute_native_units(img, anchor[1].value),
            absolute_native_units(img, anchor[2].value))

close_path(img::Image) = Cairo.close_path(img.ctx)
new_sub_path(img::Image) = Cairo.new_sub_path(img.ctx)

arc(img::Image, x::Float64, y::Float64, radius::Float64, angle1::Float64, angle2::Float64) =
        Cairo.arc(img.ctx,
            absolute_native_units(img, x),
            absolute_native_units(img, y),
            absolute_native_units(img, radius),
            angle1, angle2)

arc_negative(img::Image, x::Float64, y::Float64, radius::Float64,
                      angle1::Float64, angle2::Float64) =
        Cairo.arc_negative(img.ctx,
            absolute_native_units(img, x),
            absolute_native_units(img, y),
            absolute_native_units(img, radius),
            angle1, angle2)

translate(img::Image, tx::Float64, ty::Float64) = Cairo.translate(img.ctx,
        absolute_native_units(img, tx),
        absolute_native_units(img, ty))

scale(img::Image, sx::Float64, sy::Float64) = Cairo.scale(img.ctx, sx, sy)
rotate(img::Image, theta::Float64) = Cairo.rotate(img.ctx, theta)

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
    img.visible || return

    if img.fill.alpha > 0.0 && !strokeonly
        Cairo.set_source_rgba(img.ctx, img.fill.r, img.fill.g, img.fill.b, img.fill.alpha)

        if img.stroke.alpha > 0.0
            Cairo.fill_preserve(img.ctx)
        else
            Cairo.fill(img.ctx)
        end
    end

    if img.stroke.alpha > 0.0
        Cairo.set_source_rgba(img.ctx, img.stroke.r, img.stroke.g, img.stroke.b, img.stroke.alpha)
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
                (property === nothing) && continue
                primitives = property.primitives
                idx > length(primitives) &&
                        error("Vector form and vector property differ in length. Can't distribute.")
                apply_property(img, primitives[idx])
            end
            kt = [Property{FillOpacityPrimitive}]
            haskey(img.vector_properties, kt[1]) && apply_property(img, img.vector_properties[kt[1]].primitives[idx])
            draw(img, primitive)
        end
    end
end

function draw(img::Image, prim::RectanglePrimitive)
    rectangle(img, prim.corner, prim.width, prim.height)
    fillstroke(img)
end

function draw(img::Image, prim::PolygonPrimitive)
    length(prim.points) <= 1 && return
    prev_ok = false
    for (i,p) in enumerate(prim.points)
        ok = isfinite(p[1].value) && isfinite(p[2].value)
        if ok && prev_ok
            line_to(img, p)
        elseif !ok && prev_ok
            close_path(img)
            fillstroke(img)
        else
            move_to(img, p)
        end
        prev_ok = ok
    end
    close_path(img)
    fillstroke(img)
end

function draw(img::Image, prim::Compose.ComplexPolygonPrimitive)
    isempty(prim.rings) && return

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
    new_sub_path(img)
    circle(img, prim.center, prim.radius)
    fillstroke(img)
end

function draw(img::Image, prim::EllipsePrimitive)
    new_sub_path(img)
    cx = prim.center[1].value
    cy = prim.center[2].value
    rx = sqrt((prim.x_point[1].value - cx)^2 +
              (prim.x_point[2].value - cy)^2)
    ry = sqrt((prim.y_point[1].value - cx)^2 +
              (prim.y_point[2].value - cy)^2)
    theta = atan(prim.x_point[2].value - cy,
                 prim.x_point[1].value - cx)

    all(isfinite,[cx, cy, rx, ry, theta]) || return

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
    length(prim.points) <= 1 && return
    prev_ok = false
    for (i,p) in enumerate(prim.points)
        ok = isfinite(p[1].value) && isfinite(p[2].value)
        if ok && prev_ok
            line_to(img, p)
        else
            move_to(img, p)
        end
        prev_ok = ok
    end
    img.arrow && arrow(img, prim.points)
    fillstroke(img, true)
end

function get_layout_size(img::Image)
    width, height = Cairo.get_layout_size(img.ctx)
    return ((width / img.ppmm) * mm, (height / img.ppmm) * mm)
end

show(io::IO, ::MIME"image/png", ctx::Context) =
    draw(PNG(io, default_graphic_width, default_graphic_height), ctx)

function draw(img::Image, prim::TextPrimitive)
    (!img.visible || (img.fill.alpha == 0.0 && img.stroke.alpha == 0.0)) && return

    Cairo.set_text(img.ctx, prim.value, true)
    width, height = get_layout_size(img)
    pos = (prim.position[1]+prim.offset[1], prim.position[2]+prim.offset[2] - height)

    if prim.halign != hleft || prim.valign != vbottom
        if prim.halign == hcenter
            pos = (pos[1] - width/2, pos[2])
        elseif prim.halign == hright
            pos = (pos[1] - width, pos[2])
        end

        if prim.valign == vcenter
            pos = (pos[1], pos[2] + height/2)
        elseif prim.valign == vtop
            pos = (pos[1], pos[2] + height)
        end
    end

    Cairo.set_source_rgba(img.ctx, img.fill.r, img.fill.g, img.fill.b, img.fill.alpha)

    if prim.rot.theta != 0.0
        save_property_state(img)
        rotate(img, prim.rot.theta, prim.rot.offset[1].value, prim.rot.offset[2].value)
    end

    move_to(img, pos)
    Cairo.show_layout(img.ctx)

    prim.rot.theta == 0.0 || restore_property_state(img)
end

function draw(img::Image, prim::CurvePrimitive)
    move_to(img, prim.anchor0)
    curve_to(img, prim.ctrl0, prim.ctrl1, prim.anchor1)
    fillstroke(img, true)
end

draw(img::Image, prim::BitmapPrimitive) =
        error("Embedding bitmaps in Cairo backends (i.e. PNG, PDF, PS) is not supported.")



function draw(img::Image, batch::FormBatch)
    bounds = boundingbox(batch.primitive, img.linewidth, img.font, img.fontsize)
    width = bounds.a[1].value * img.ppmm
    height = bounds.a[2].value * img.ppmm

    xt = -bounds.x0[1].value * img.ppmm
    yt = -bounds.x0[2].value * img.ppmm

    Cairo.save(img.ctx)
    Cairo.reset_clip(img.ctx)
    Cairo.translate(img.ctx, xt, yt)
    Cairo.push_group(img.ctx)
    draw(img, batch.primitive)
    pattern = Cairo.pop_group(img.ctx)
    surface = Cairo.pattern_get_surface(pattern)
    Cairo.restore(img.ctx)

    # reapply the clipping region we just reset
    if img.clip !== nothing
        apply_property(img, img.clip)
    end

    Cairo.set_antialias(img.ctx, Cairo.ANTIALIAS_NONE)
    for offset in batch.offsets
        x = offset[1].value * img.ppmm - xt
        y = offset[2].value * img.ppmm - yt
        Cairo.set_source_surface(img.ctx, surface, x, y)
        Cairo.rectangle(img.ctx, x, y, width, height)
        Cairo.fill(img.ctx)
    end
    Cairo.set_antialias(img.ctx, Cairo.ANTIALIAS_DEFAULT)
end


function draw(img::Compose.Image, prim::ArcPrimitive)
    new_sub_path(img)
    xc = prim.center[1]
    yc = prim.center[2]
    prim.sector && move_to(img, (xc, yc))
    arc(img, xc.value, yc.value, prim.radius.value, prim.angle1, prim.angle2)
    prim.sector && line_to(img, (xc, yc))
    img.arrow && arrow(img, prim)
    fillstroke(img)
end


# Arrows

apply_property(img::Image, p::ArrowPrimitive) = 
        img.arrow = p.value


function arrow(img::Image, points::Vector{<:Vec})
    xmax, ymax = points[end]
    dxy = points[end] .- points[end-1]
    dx, dy = dxy[1].value,  dxy[2].value 
    vl, θ = 0.225*hypot(dy,dx), atan(dy, dx) 
    arrowhead(img, (xmax,ymax), vl, θ)
end

function arrow(img::Image, prim::ArcPrimitive)
    xy = prim.center .+ prim.radius.*(cos(prim.angle2), sin(prim.angle2))
    θ = prim.angle2 + 0.45π
   arrowhead(img, xy, 0.5*prim.radius.value, θ)
end


function arrowhead(img::Image, point::Vec, vl::Float64, θ::Float64)
    xmax, ymax = point
    ϕ  =  pi/15
    xr = -vl*[cos(θ+ϕ), cos(θ-ϕ)].*1mm
    yr = -vl*[sin(θ+ϕ), sin(θ-ϕ)].*1mm
    move_to(img, (xmax+xr[1], ymax+yr[1]))
    line_to(img, (xmax, ymax))
    line_to(img, (xmax+xr[2], ymax+yr[2]))
end


# Bezigon



function draw(img::Image, prim::BezierPolygonPrimitive)
    move_to(img, prim.anchor)
    currentpoint = prim.anchor
    for side in prim.sides
        if length(side)==1
            line_to(img, side[1])
        elseif length(side)==2
            cp1 = controlpnt(currentpoint, side[1])
            cp2 = controlpnt(side[2], side[1])
            curve_to(img, cp1, cp2, side[2])
        elseif length(side)==3
            curve_to(img, side[1], side[2], side[3])
        end
        currentpoint = side[end]
    end
    close_path(img)
    fillstroke(img)
end




