
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

# Native unit
type ImageMeasure{T <: ImageBackend} <: NativeMeasure
    value::Float64
end

copy{T}(u::ImageMeasure{T}) = ImageMeasure{T}(u.value)

convert(::Type{Float64}, u::ImageMeasure) = u.value
function convert{T}(::Type{ImageMeasure{T}}, u::Number)
    ImageMeasure{T}(convert(Float64, u))
end

function *{T}(u::Number, v::ImageMeasure{T})
    ImageMeasure{T}(convert(Float64, u) * v.value)
end

function /{T}(u::ImageMeasure{T}, v::Number)
    ImageMeasure{T}(u.value / convert(Float64, v))
end

function +{T}(u::ImageMeasure{T}, v::ImageMeasure{T})
    ImageMeasure{T}(u.value + v.value)
end

function -{T}(u::ImageMeasure{T}, v::ImageMeasure{T})
    ImageMeasure{T}(u.value - v.value)
end

type ImagePropertyState
    stroke::ColorOrNothing
    fill::ColorOrNothing
end

type Image{B <: ImageBackend} <: Backend
    out::IO
    surface::CairoSurface
    ctx::CairoContext

    # Current state
    stroke::ColorOrNothing
    fill::ColorOrNothing
    opacity::Float64 # in [0,1]
    visible::Bool

    # Keep track of property
    state_stack::Vector{ImagePropertyState}

    # Close the surface when finished
    owns_surface::Bool

    # Close the stream when finished
    close_stream::Bool

    # Emit on finish
    emit_on_finish::Bool

    function Image(surface::CairoSurface, ctx::CairoContext, out::IO)
        img = new()
        img.out = out
        img.surface = surface
        img.ctx = ctx
        img.stroke = RGB(0., 0., 0.)
        img.fill   = RGB(0., 0., 0.)
        img.opacity = 1.0
        img.visible = true
        img.state_stack = Array(ImagePropertyState, 0)
        img.owns_surface = false
        img.emit_on_finish = false
        img
    end

    function Image(surface::CairoSurface, ctx::CairoContext)
        Image{B}(surface, ctx, IOString())
    end

    Image(surface::CairoSurface) = Image{B}(surface, CairoContext(surface))

    function Image(out::IO,
                   width::MeasureOrNumber,
                   height::MeasureOrNumber,
                   emit_on_finish::Bool=true)

        w = native_measure(width, B).value
        h = native_measure(height, B).value

        local surface::CairoSurface
        if B == SVGBackend
            surface = CairoSVGSurface(out, w, h)
        elseif B == PNGBackend
            surface = CairoARGBSurface(round(w), round(h))
        elseif B == PDFBackend
            surface = CairoPDFSurface(out, w, h)
        elseif B == PSBackend
            surface = CairoEPSSurface(out, w, h)
        else
            error("Unkown Cairo backend.")
        end

        if Cairo.status(surface) != Cairo.STATUS_SUCCESS
            error("Unable to create cairo surface.")
        end

        img = Image{B}(surface, CairoContext(surface), out)
        img.owns_surface = true
        img.close_stream = false
        img.emit_on_finish = emit_on_finish
        img
    end

    function Image(filename::String,
                   width::MeasureOrNumber,
                   height::MeasureOrNumber)
        img = Image{B}(open(filename, "w"), width, height)
        img.close_stream = true
        img
    end
end


width{B}(img::Image{B}) = ImageMeasure{B}(Cairo.width(img.surface))
height{B}(img::Image{B}) = ImageMeasure{B}(Cairo.height(img.surface))

finish{B <: ImageBackend}(::Type{B}, img::Image) = nothing

function finish(::Type{PNGBackend}, img::Image)
    Cairo.write_to_png(img.surface, img.out)
end

function finish{B <: ImageBackend}(img::Image{B})
    if(img.owns_surface)
        Cairo.destroy(img.ctx)
    end
    finish(B, img)
    if(img.owns_surface)
        Cairo.destroy(img.surface)
    end

    if img.emit_on_finish && typeof(img.out) == IOString
        seek(img.out, 0)
        if B == PNGBackend
            mime = "image/png"
        elseif B == SVGBackend
            mime = "image/svg+xml"
        elseif B == "PSBackend"
            mime = "application/postscript"
        elseif B == "PDFBackend"
            mime = "application/pdf"
        end
        emit(Emitable(mime, readall(img.f)))
        close(img.out)
    end
end


typealias PNG Image{PNGBackend}
typealias PDF Image{PDFBackend}
typealias PS  Image{PSBackend}


# sizes

function root_box{B}(img::Image{B})
    NativeBoundingBox(
        ImageMeasure{B}(0.),
        ImageMeasure{B}(0.),
        width(img),
        height(img))
end


native_zero{T}(backend::Image{T}) = ImageMeasure{T}(0.0)


# PNG conversion to native units (i.e., pixels)

function native_measure{K <: ImageBackend}(u::Number, ::Type{K})
    ImageMeasure{K}(convert(Float64, u))
end


function native_measure{K <: ImageBackend}(u::SimpleMeasure{PixelUnit},
                                           ::Type{K})
    ImageMeasure{K}(u.value)
end


function native_measure{K <: ImageBackend}(u::SimpleMeasure{MillimeterUnit},
                                           ::Type{K})
    native_measure(convert(SimpleMeasure{PixelUnit}, u), K)
end


function native_measure{K <: ImageBackend}(u::Number, backend::Image{K})
    native_measure(u, K)
end


function native_measure{K <: ImageBackend}(u::SimpleMeasure{PixelUnit},
                        backend::Image{K})
    native_measure(u, K)
end


function native_measure{K <: ImageBackend}(u::SimpleMeasure{MillimeterUnit},
                        backend::Image{K})
    native_measure(u, K)
end


# SVG/PDF/PS conversion to native units (i.e., pts)

function native_measure{K <: VectorImageBackend}(u::Number, ::Type{K})
    ImageMeasure{K}(convert(Float64, u))
end


function native_measure{K <: VectorImageBackend}(u::SimpleMeasure{PixelUnit},
                                                 ::Type{K})
    native_measure(convert(SimpleMeasure{MillimeterUnit}, u), K)
end


function native_measure{K <: VectorImageBackend}(u::SimpleMeasure{MillimeterUnit},
                                                 ::Type{K})
    ImageMeasure{K}(u / pt)
end



function native_measure{K <: VectorImageBackend}(
        u::Number,
        backend::Image{K})
    native_measure(u, K)
end


function native_measure{K <: VectorImageBackend}(
        u::SimpleMeasure{PixelUnit},
        backend::Image{K})
    native_measure(u, K)
end


function native_measure{K <: VectorImageBackend}(
        u::SimpleMeasure{MillimeterUnit},
        backend::Image{K})
    native_measure(u, K)
end


# Drawing

move_to(img::Image, point::Point) = Cairo.move_to(img.ctx, point.x.value, point.y.value)
line_to(img::Image, point::Point) = Cairo.line_to(img.ctx, point.x.value, point.y.value)

# TODO: What is the equivalent Cairo call?
curve_to(img::Image, ctrl0::Point, ctrl1::Point, anchor1::Point) =
    curve_to(img.ctx, ctrl0.x.value, ctrl0.y.value, ctrl1.x.value, ctrl1.y.value,
            anchor1.x.value, anchor1.y.value)

close_path(img::Image) = Cairo.close_path(img.ctx)

function arc(img::Image, x::Float64, y::Float64, radius::Float64,
             angle1::Float64, angle2::Float64)
    Cairo.arc(img.ctx,x,y,radius,angle1,angle2)
end

translate(img::Image, tx::Float64, ty::Float64) = Cairo.translate(img.ctx, tx, ty)
scale(img::Image, sx::Float64, sy::Float64) = Cairo.scale(img.ctx, sx, sy)
rotate(img::Image, theta::Float64) = Cairo.rotate(img.ctx, theta)


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

    if img.stroke != nothing
        rgb = convert(RGB, img.stroke)
        Cairo.set_source_rgb(img.ctx, rgb.r, rgb.g, rgb.b)

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
    cx = form.center.x.value
    cy = form.center.y.value
    rx = sqrt((form.x_point.x.value - cx)^2 +
              (form.x_point.y.value - cy)^2)
    ry = sqrt((form.y_point.x.value - cx)^2 +
              (form.y_point.y.value - cy)^2)
    theta = atan2(form.x_point.y.value - cy,
                  form.x_point.x.value - cx)

    save_state(img)
    translate(img, cx, cy)
    rotate(img, theta)
    translate(img, -rx, -ry)
    scale(img, 2rx, 2ry)
    arc(img, 0.5, 0.5, 0.5, 0.0, 2pi)
    restore_state(img)
    fillstroke(img)
end


function draw(img::Image, form::Text)
    if !img.visible || img.opacity == 0.0 || img.fill === nothing
        return
    end

    pos = copy(form.pos)

    if form.halign != hleft || form.valign != vtop
        extents = Array(Float64, 6)
        Cairo.text_extents(img.ctx, bytestring(form.value), extents)

        width, height = extents[3], extents[4]

        if form.halign == hcenter
            pos.x.value -= width / 2
        elseif form.halign == hright
            pos.x.value -= width
        end

        if form.valign == vcenter
            pos.y.value += height / 2
        elseif form.valign == vtop
            pos.y.value += height
        end
    end

    rgb = convert(RGB, img.fill)
    Cairo.set_source_rgba(img.ctx, rgb.r, rgb.g, rgb.b, img.opacity)

    save_state(img)
    Cairo.translate(img.ctx, form.t.M[1,3], form.t.M[2,3])
    rotate(img, atan2(form.t.M[2,1], form.t.M[1,1]))
    move_to(img, pos)
    Cairo.show_text(img.ctx, form.value)
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


function apply_property(img::Image, p::Visible)
    img.visible = p.value
end


function apply_property(img::Image, property::LineWidth)
    Cairo.set_line_width(img.ctx, property.value.value)
end


function apply_property(img::Image, property::Font)
    Cairo.select_font_face(img.ctx, property.family,
                           Cairo.FONT_SLANT_NORMAL,
                           Cairo.FONT_WEIGHT_NORMAL)
end


function apply_property(img::Image, property::FontSize)
    Cairo.set_font_size(img.ctx, property.value.value)
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


