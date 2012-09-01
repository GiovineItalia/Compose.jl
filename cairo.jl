
# Cairo backend for compose

require("backend.jl")
require("measure.jl")
require("color.jl")
require("form.jl")

const libcairo = dlopen("libcairo")

typealias cairo_format_t Int32
const CAIRO_FORMAT_INVALID   = int32(-1)
const CAIRO_FORMAT_ARGB32    = int32(0)
const CAIRO_FORMAT_RGB24     = int32(1)
const CAIRO_FORMAT_A8        = int32(2)
const CAIRO_FORMAT_A1        = int32(3)
const CAIRO_FORMAT_RGB16_565 = int32(4)
const CAIRO_FORMAT_RGB30     = int32(5)

typealias cairo_status_t Int32
const CAIRO_STATUS_SUCCESS                   = int32(0)
const CAIRO_STATUS_NO_MEMORY                 = int32(1)
const CAIRO_STATUS_INVALID_RESTORE           = int32(2)
const CAIRO_STATUS_INVALID_POP_GROUP         = int32(3)
const CAIRO_STATUS_NO_CURRENT_POINT          = int32(4)
const CAIRO_STATUS_INVALID_MATRIX            = int32(5)
const CAIRO_STATUS_INVALID_STATUS            = int32(6)
const CAIRO_STATUS_NULL_POINTER              = int32(7)
const CAIRO_STATUS_INVALID_STRING            = int32(8)
const CAIRO_STATUS_INVALID_PATH_DATA         = int32(9)
const CAIRO_STATUS_READ_ERROR                = int32(10)
const CAIRO_STATUS_WRITE_ERROR               = int32(11)
const CAIRO_STATUS_SURFACE_FINISHED          = int32(12)
const CAIRO_STATUS_SURFACE_TYPE_MISMATCH     = int32(13)
const CAIRO_STATUS_PATTERN_TYPE_MISMATCH     = int32(14)
const CAIRO_STATUS_INVALID_CONTENT           = int32(15)
const CAIRO_STATUS_INVALID_FORMAT            = int32(16)
const CAIRO_STATUS_INVALID_VISUAL            = int32(17)
const CAIRO_STATUS_FILE_NOT_FOUND            = int32(17)
const CAIRO_STATUS_INVALID_DASH              = int32(18)
const CAIRO_STATUS_INVALID_DSC_COMMENT       = int32(19)
const CAIRO_STATUS_INVALID_INDEX             = int32(20)
const CAIRO_STATUS_CLIP_NOT_REPRESENTABLE    = int32(21)
const CAIRO_STATUS_TEMP_FILE_ERROR           = int32(22)
const CAIRO_STATUS_INVALID_STRIDE            = int32(23)
const CAIRO_STATUS_FONT_TYPE_MISMATCH        = int32(24)
const CAIRO_STATUS_USER_FONT_IMMUTABLE       = int32(25)
const CAIRO_STATUS_USER_FONT_ERROR           = int32(26)
const CAIRO_STATUS_NEGATIVE_COUNT            = int32(27)
const CAIRO_STATUS_INVALID_CLUSTERS          = int32(28)
const CAIRO_STATUS_INVALID_SLANT             = int32(29)
const CAIRO_STATUS_INVALID_WEIGHT            = int32(30)
const CAIRO_STATUS_INVALID_SIZE              = int32(31)
const CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED = int32(32)
const CAIRO_STATUS_DEVICE_TYPE_MISMATCH      = int32(33)
const CAIRO_STATUS_DEVICE_ERROR              = int32(34)
const CAIRO_STATUS_INVALID_MESH_CONSTRUCTION = int32(35)
const CAIRO_STATUS_DEVICE_FINISHED           = int32(36)
const CAIRO_STATUS_LAST_STATUS               = int32(37)


abstract ImageBackend
abstract PNGBackend <: ImageBackend

abstract VectorImageBackend <: ImageBackend
abstract SVGBackend <: VectorImageBackend
abstract PDFBackend <: VectorImageBackend
abstract PSBackend  <: VectorImageBackend

# Native unit
type ImageMeasure{T <: ImageBackend} <: NativeMeasure
    value::Float
end

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
    width::ImageMeasure{B}
    height::ImageMeasure{B}
    surf::Ptr{Void}
    ctx::Ptr{Void}
    stroke::ColorOrNothing
    fill::ColorOrNothing
    state_stack::Vector{ImagePropertyState}

    function Image(filename::String,
                   width::MeasureOrNumber,
                   height::MeasureOrNumber)
        img = new()

        img.filename = bytestring(abs_path(filename))

        img.width  = native_measure(width,  img)
        img.height = native_measure(height, img)

        # Try opening the file for writing immediately so we can fail early if
        # it doesn't exist.
        try
            f = open(filename, "w")
            close(f)
        catch
            error(@printf("Can't write to %s.", filename))
        end

        if B == SVGBackend
            img.surf = ccall(dlsym(libcairo, :cairo_svg_surface_create),
                             Ptr{Void}, (Ptr{Uint8}, Float64, Float64),
                             img.filename, img.width.value, img.height.value)
        elseif B == PNGBackend
            img.surf = ccall(dlsym(libcairo, :cairo_image_surface_create),
                             Ptr{Void}, (Int32, Int32, Int32),
                             CAIRO_FORMAT_ARGB32,
                             convert(Int32, round(img.width.value)),
                             convert(Int32, round(img.height.value)))
        elseif B == PDFBackend
            img.surf = ccall(dlsym(libcairo, :cairo_pdf_surface_create),
                             Ptr{Void}, (Ptr{Uint8}, Float64, Float64),
                             img.filename, img.width.value, img.height.value)
        elseif B == PSBackend
            img.surf = ccall(dlsym(libcairo, :cairo_ps_surface_create),
                             Ptr{Void}, (Ptr{Uint8}, Float64, Float64),
                             img.filename, img.width.value, img.height.value)
        else
            error("Unkown Cairo backend.")
        end

        status = ccall(dlsym(libcairo, :cairo_surface_status),
                       Int32, (Ptr{Void},), img.surf)

        if status != CAIRO_STATUS_SUCCESS
            error("Unable to create cairo surface.")
        end

        img.ctx = ccall(dlsym(libcairo, :cairo_create),
                        Ptr{Void}, (Ptr{Void},), img.surf)


        img.stroke = RGB(0., 0., 0.)
        img.fill   = RGB(0., 0., 0.)
        img.state_stack = Array(ImagePropertyState, 0)

        img
    end
end


function finish{B}(img::Image{B})
    ccall(dlsym(libcairo, :cairo_destroy),
          Void, (Ptr{Void},), img.ctx)

    if B == PNGBackend
        ccall(dlsym(libcairo, :cairo_surface_write_to_png),
              Int32, (Ptr{Void}, Ptr{Uint8}),
              img.surf, img.filename)
    end

    ccall(dlsym(libcairo, :cairo_surface_destroy),
          Void, (Ptr{Void},), img.surf)
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

function move_to(img::Image, point::Point)
    ccall(dlsym(libcairo, :cairo_move_to), Void, (Ptr{Void}, Float64, Float64),
          img.ctx, point.x.value, point.y.value)
end


function line_to(img::Image, point::Point)
    ccall(dlsym(libcairo, :cairo_line_to), Void, (Ptr{Void}, Float64, Float64),
          img.ctx, point.x.value, point.y.value)
end


function close_path(img::Image)
    ccall(dlsym(libcairo, :cairo_close_path), Void, (Ptr{Void},), img.ctx)
end


function fillstroke(img::Image)
    if img.fill != nothing
        rgb = convert(RGB, img.fill)
        ccall(dlsym(libcairo, :cairo_set_source_rgb), Void,
              (Ptr{Void}, Float64, Float64, Float64),
              img.ctx, rgb.r, rgb.g, rgb.b)

        if img.stroke != nothing
            ccall(dlsym(libcairo, :cairo_fill_preserve),
                  Void, (Ptr{Void},), img.ctx)
        else
            ccall(dlsym(libcairo, :cairo_fill),
                  Void, (Ptr{Void},), img.ctx)
        end
    end

    if img.stroke != nothing
        rgb = convert(RGB, img.stroke)
        ccall(dlsym(libcairo, :cairo_set_source_rgb), Void,
              (Ptr{Void}, Float64, Float64, Float64),
              img.ctx, rgb.r, rgb.g, rgb.b)

        ccall(dlsym(libcairo, :cairo_stroke), Void, (Ptr{Void},), img.ctx)
    end
end


function save_state(img::Image)
    push(img.state_stack, ImagePropertyState(img.stroke, img.fill))
    ccall(dlsym(libcairo, :cairo_save), Void, (Ptr{Void},), img.ctx)
end


function restore_state(img::Image)
    state = pop(img.state_stack)
    img.stroke = state.stroke
    img.fill = state.fill
    ccall(dlsym(libcairo, :cairo_restore), Void, (Ptr{Void},), img.ctx)
end


function arc(img::Image, x::Float64, y::Float64,
             radius::Float64, angle1::Float64, angle2::Float64)
    ccall(dlsym(libcairo, :cairo_arc), Void,
          (Ptr{Void}, Float64, Float64, Float64, Float64, Float64),
          img.ctx, x, y, radius, angle1, angle2)
end


function translate(img::Image, tx::Float64, ty::Float64)
    ccall(dlsym(libcairo, :cairo_translate), Void,
          (Ptr{Void}, Float64, Float64),
          img.ctx, tx, ty)
end


function scale(img::Image, sx::Float64, sy::Float64)
    ccall(dlsym(libcairo, :cairo_scale), Void,
          (Ptr{Void}, Float64, Float64),
          img.ctx, sx, sy)
end


function rotate(img::Image, theta::Float64)
    ccall(dlsym(libcairo, :cairo_rotate), Void,
          (Ptr{Void}, Float64), img.ctx, theta)
end


function draw(img::Image, form::LinesForm)
    if isempty(form.points); return; end

    move_to(img, form.points[1])
    for point in form.points[2:]
        line_to(img, point)
    end
    fillstroke(img)
end


function draw(img::Image, form::PolygonForm)
    if isempty(form.points); return; end

    move_to(img, form.points[1])
    for point in form.points[2:]
        line_to(img, point)
    end
    close_path(img)
    fillstroke(img)
end


function draw(img::Image, form::EllipseForm)
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


# Applying properties


function push_property(img::Image, p::Property)
    save_state(img)
    for specific in p.specifics
        apply_property(img, specific)
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


function apply_property(img::Image, property::LineWidth)
    ccall(dlsym(libcairo, :cairo_set_line_width),
          Void, (Ptr{Void}, Float64),
          img.ctx, property.value.value)
end

