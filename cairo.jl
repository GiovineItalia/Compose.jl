
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


type Image{B <: ImageBackend} <: Backend
    filename::String
    width::ImageMeasure{B}
    height::ImageMeasure{B}
    surf::Ptr{Void}
    ctx::Ptr{Void}
    stroke::ColorOrNothing
    fill::ColorOrNothing

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
typealias SVG Image{SVGBackend}
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


function default_property(img::Image)
    Property(PropertyType[
        StrokeBare(RGB(0.,0.,0.)),
        FillBare(RGB(0.,0.,0.)),
        LineWidthBare(0.5mm)])
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

function draw(img::Image, op::MoveTo)
    ccall(dlsym(libcairo, :cairo_move_to), Void, (Ptr{Void}, Float64, Float64),
          img.ctx, op.point.x.value, op.point.y.value)
end


function draw(img::Image, op::LineTo)
    ccall(dlsym(libcairo, :cairo_line_to), Void, (Ptr{Void}, Float64, Float64),
          img.ctx, op.point.x.value, op.point.y.value)
end


function draw(img::Image, op::ClosePath)
    ccall(dlsym(libcairo, :cairo_close_path), Void, (Ptr{Void},), img.ctx)
end


function draw(img::Image, op::FillStroke)
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


# Applying properties

function draw(img::Image, property::Fill)
    img.fill = property.value
end


function draw(img::Image, property::Stroke)
    img.stroke = property.value
end


function draw(img::Image, property::LineWidth)
    ccall(dlsym(libcairo, :cairo_set_line_width),
          Void, (Ptr{Void}, Float64),
          img.ctx, property.value.value)
end

