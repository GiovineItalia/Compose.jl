
# Cairo backend for compose
importall Cairo

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
    filename::String
    surface::CairoSurface
    ctx::CairoContext
    stroke::ColorOrNothing
    fill::ColorOrNothing
    state_stack::Vector{ImagePropertyState}


    function Image(surface::CairoSurface,filename::String)
        img = new()
        img.filename = filename
        img.surface = surface
        img.ctx = CairoContext(surface)
    end

    function Image(filename::String,
                   width::MeasureOrNumber,
                   height::MeasureOrNumber)
        img = new()

        img.filename = bytestring(abs_path(filename))

        w  = native_measure(width,  img).value
        h = native_measure(height, img).value

        # Try opening the file for writing immediately so we can fail early if
        # it doesn't exist.
        f = try
            open(filename, "w")
            if(B==PNGBackend)
        catch
            error(@printf("Can't write to %s.", filename))
        end

        if B == SVGBackend
            img.surface = SVGSurface(f,w,h)
        elseif B == PNGBackend
            img.surface = CairoARGBSurface(round(w), round(h))
        elseif B == PDFBackend
            img.surface = CairoPDFSurface(f,w,h)
        elseif B == PSBackend
            img.surface = CairoEPSSurface(f,w,h)
        else
            error("Unkown Cairo backend.")
        end

        if status(img.surface) != CAIRO_STATUS_SUCCESS
            error("Unable to create cairo surface.")
        end

        img.ctx = CairoContext(img.surface)


        img.stroke = RGB(0., 0., 0.)
        img.fill   = RGB(0., 0., 0.)
        img.state_stack = Array(ImagePropertyState, 0)

        img
    end
end

Image(args...) = Image{ImageBackend}(args...)

finsh{B<:ImageBackend}(::Type{B},img::Image) = nothing
finsh(::Type{PNGBackend},img::Image) = write_to_png(img.surf, img.filename)

function finish{B}(img::Image{B})
    destroy(img.ctx)
    finish(B,img)
    destroy(img.surface)
end


typealias PNG Image{PNGBackend}
typealias PDF Image{PDFBackend}
typealias PS  Image{PSBackend}


# sizes

function root_box{B}(img::Image{B})
    NativeBoundingBox(
        ImageMeasure{B}(0.),
        ImageMeasure{B}(0.),
        img.width,
        img.height)
end


native_zero{T}(backend::Image{T}) = ImageMeasure{T}(0.0)


# PNG conversion to native units (i.e., pixels)

function native_measure(u::Number,
                        backend::Image{PNGBackend})
    ImageMeasure{PNGBackend}(convert(Float64, u))
end


function native_measure(u::SimpleMeasure{PixelUnit},
                        backend::Image{PNGBackend})
    ImageMeasure{PNGBackend}(u.value)
end


function native_measure(u::SimpleMeasure{MillimeterUnit},
                        backend::Image{PNGBackend})

    native_measure(convert(SimpleMeasure{PixelUnit}, u), backend)
end


# SVG/PDF/PS conversion to native units (i.e., pts)

function native_measure{K <: VectorImageBackend}(
        u::Number,
        backend::Image{K})
    ImageMeasure{K}(convert(Float64, u))
end


function native_measure{K <: VectorImageBackend}(
        u::SimpleMeasure{PixelUnit},
        backend::Image{K})
    native_measure(convert(SimpleMeasure{MillimeterUnit}, u), backend)
end


function native_measure{K <: VectorImageBackend}(
        u::SimpleMeasure{MillimeterUnit},
        backend::Image{K})
    ImageMeasure{K}(u / pt)
end


# Drawing

move_to(img::Image, point::Point) = move_to(img.ctx, point.x.value, point.y.value)
line_to(img::Image, point::Point) = line_to(img.ctx, point.x.value, point.y.value)
curve_to(img::Image, ctrl0::Point, ctrl1::Point, anchor1::Point) =
    curve_to(img.ctx, ctrl0.x.value, ctrl0.y.value, ctrl1.x.value, ctrl1.y.value,
            anchor1.x.value, anchor1.y.value)

close_path(img::Image) = close_path(img.context)

arc(img::Image, x::Float64, y::Float64, radius::Float64, angle1::Float64, 
    angle2::Float64) = arc(img.context,x,y,radius,angle1,angle2)

translate(img::Image, tx::Float64, ty::Float64) = translate(img.ctx,dx,dy)
scale(img::Image, sx::Float64, sy::Float64) = scale(img.ctx,sx,sy)
rotate(img::Image, theta::Float64) = rotate(img.ctx,theta)


function fillstroke(img::Image)
    if img.fill != nothing
        rgb = convert(RGB, img.fill)
        set_source_rgb(img.ctx, rgb.r, rgb.g, rgb.b)

        if img.stroke != nothing
            fill_preserve(img.ctx)
        else
            fill(img.ctx)
        end
    end

    if img.stroke != nothing
        rgb = convert(RGB, img.stroke)
        set_source_rgb(img.ctx, rgb.r, rgb.g, rgb.b)

        stroke(img.ctx)
    end
end


function save_state(img::Image)
    push!(img.state_stack, ImagePropertyState(img.stroke, img.fill))
    save(img.ctx)
end


function restore_state(img::Image)
    state = pop!(img.state_stack)
    img.stroke = state.stroke
    img.fill = state.fill
    restore(img.ctx)
end


function draw(img::Image, form::Lines)
    if isempty(form.points); return; end

    move_to(img, form.points[1])
    for point in form.points[2:]
        line_to(img, point)
    end
    fillstroke(img)
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
    pos = copy(form.pos)

    if form.halign != hleft || form.valign != vtop
        extents = Array(Float64, 6)
        text_extents(img.ctx, bytestring(form.value), extents)

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

    move_to(img, pos)
    show_text(img,form.value)
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


function apply_property(img::Image, p::Stroke)
    img.stroke = p.value
end


function apply_property(img::Image, p::Fill)
    img.fill = p.value
end


apply_property(img::Image, property::LineWidth) = set_line_width(img.ctx, property.value.value)
apply_property(img::Image, property::Font) = select_font_face(img.ctx,property.family,CAIRO_FONT_SLANT_NORMAL,CAIRO_FONT_WEIGHT_NORMAL)
apply_property(img::Image, property::FontSize) = set_font_size(img.ctx, property.value.value)

