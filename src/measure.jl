
export mm, cm, inch, furlong, pt, w, h, px, cx, cy

# A unit gives some semantic meaning to a bare number.
abstract Unit
show(io, ::Type{Unit}) = print(io, "??")

# Units defined in terms of the screen.
abstract PixelUnit <: Unit
show(io, ::Type{PixelUnit}) = print(io, "px")

# Units of absolute length.
abstract MillimeterUnit <: Unit
show(io, ::Type{MillimeterUnit}) = print(io, "mm")

# Units proportional to the width/height of the containing canvas.
abstract WidthUnit <: Unit
show(io, ::Type{WidthUnit}) = print(io, "width")

abstract HeightUnit <: Unit
show(io, ::Type{HeightUnit}) = print(io, "height")

# Canvas specific units defined in terms of width/height.
abstract CanvasXUnit <: Unit
abstract CanvasYUnit <: Unit

# A measure is a number annotated with a type.
abstract Measure


# A measure used by a backend
abstract NativeMeasure <: Measure


# A single number with a single associated unit.
type SimpleMeasure{U <: Unit} <: Measure
    value::Float64
end


function copy{U}(a::SimpleMeasure{U})
    SimpleMeasure{U}(a.value)
end


function show{U}(io, a::SimpleMeasure{U})
    print(io, a.value, U)
end


function +{U}(a::SimpleMeasure{U}, b::SimpleMeasure{U})
    SimpleMeasure{U}(a.value + b.value)
end


function -{U}(a::SimpleMeasure{U}, b::SimpleMeasure{U})
    SimpleMeasure{U}(a.value - b.value)
end


function *{U}(a::Number, b::SimpleMeasure{U})
    SimpleMeasure{U}(convert(Float64, a) * b.value)
end


*{U}(a::SimpleMeasure{U}, b::Number) = b * a

-{U}(a::SimpleMeasure{U}) = SimpleMeasure{U}(-a.value)


function /{U}(a::SimpleMeasure{U}, b::SimpleMeasure{U})
    a.value / b.value
end


function /{U}(a::SimpleMeasure{U}, b::Number)
    SimpleMeasure{U}(a.value / convert(Float64, b))
end


=={U}(a::SimpleMeasure{U}, b::SimpleMeasure{U}) = a.value == b.value
<{U}(a::SimpleMeasure{U}, b::SimpleMeasure{U}) = a.value < b.value
<={U}(a::SimpleMeasure{U}, b::SimpleMeasure{U}) = a.value <= b.value
>={U}(a::SimpleMeasure{U}, b::SimpleMeasure{U}) = a.value >= b.value


# A sum of one or more measures of different units.
type CompoundMeasure <: Measure
    values::Dict{Type, Float64}

    function CompoundMeasure()
        new(Dict{Type, Float64}())
    end

    function CompoundMeasure(values::Dict{Type, Float64})
        new(values)
    end
end


function copy(a::CompoundMeasure)
    CompoundMeasure(copy(a.values))
end

function show(io, a::CompoundMeasure)
    print(io, join([sprint(print, v, k) for (k, v) in a.values], " + "))
end


function +{U, V}(a::SimpleMeasure{U}, b::SimpleMeasure{V})
    c = CompoundMeasure()
    c.values[U] = a.value
    c.values[V] = b.value
    c
end


function -{U, V}(a::SimpleMeasure{U}, b::SimpleMeasure{V})
    c = CompoundMeasure()
    c.values[U] =  a.value
    c.values[V] = -b.value
    c
end


function +{U}(a::CompoundMeasure, b::SimpleMeasure{U})
    c = CompoundMeasure(copy(a.values))
    if has(c.values, U)
        c.values[U] += b.value
    else
        c.values[U] = b.value
    end
    c
end


+{U}(a::SimpleMeasure{U}, b::CompoundMeasure) = b + a


function -{U}(a::CompoundMeasure, b::SimpleMeasure{U})
    c = CompoundMeasure(copy(a.values))
    if has(c.values, U)
        c.values[U] -= b.value
    else
        c.values[U] = -b.value
    end
    c
end


function -{U}(a::SimpleMeasure{U}, b::CompoundMeasure)
    c = CompoundMeasure()
    c.values[U] = a.value
    for (V, value) in b.values
        if has(c.values, V)
            c.values[V] -= value
        else
            c.values[V] = -value
        end
    end
    c
end


function +(a::CompoundMeasure, b::CompoundMeasure)
    c = CompoundMeasure(copy(a.values))
    for (k, v) in b.values
        if has(c, k)
            c.values[k] += v
        else
            c.values[k] = v
        end
    end
    c
end


function *(a::Number, b::CompoundMeasure)
    a = convert(Float64, a)
    c = CompoundMeasure(copy(b.values))
    for (k,v) in c.values
        c.values[k] *= a
    end
    c
end


*(a::CompoundMeasure, b::Number) = b * a


function /(a::CompoundMeasure, b::Number)
    b = convert(Float64, b)
    c = CompoundMeasure(copy(a.values))
    for (k,v) in c.values
        c.values[k] /= b
    end
    c
end


# Rather than explicitly intialize instances of Measure, one should use these
# constants to form coordinates by multiplication. E.g., "2inch", "35px", and
# so on.
const mm      = SimpleMeasure{MillimeterUnit}(1.0)
const cm      = SimpleMeasure{MillimeterUnit}(10.0)
const inch    = SimpleMeasure{MillimeterUnit}(25.4)
const furlong = 7920inch
const pt      = inch/72
const w       = SimpleMeasure{WidthUnit}(1.0)
const h       = SimpleMeasure{HeightUnit}(1.0)
const px      = SimpleMeasure{PixelUnit}(1.0)
const cx      = SimpleMeasure{CanvasXUnit}(1.0)
const cy      = SimpleMeasure{CanvasYUnit}(1.0)


# Conversion between pixels and millimeters. This is tricky business, since we
# have no way of knowing the pixel density yet want to do something other than
# display an error message. Thus we are going to choose a reasonable default of
# 4.5 pixels per mm, which is in typical laptop screen territory.
const assumed_ppmm = 4.5

function convert(::Type{SimpleMeasure{PixelUnit}},
                 u::SimpleMeasure{MillimeterUnit})
    SimpleMeasure{PixelUnit}(u.value * assumed_ppmm)
end


function convert(::Type{SimpleMeasure{MillimeterUnit}},
                 u::SimpleMeasure{PixelUnit})
    SimpleMeasure{PixelUnit}(u.value / assumed_ppmm)
end


typealias MeasureOrNumber Union(Measure, Number)


# Points
type Point
    x::Measure
    y::Measure

    function Point(x::MeasureOrNumber, y::MeasureOrNumber)
        new(x_measure(x), y_measure(y))
    end

    function Point(point::Point)
        new(copy(point.x), copy(point.y))
    end
end


copy(point::Point) = Point(point)


# Support specifying points as tuples.
typealias XYTuple NTuple{2, MeasureOrNumber}


function Point(xy::XYTuple)
    Point(x_measure(xy[1]),
          y_measure(xy[2]))
end


function convert(::Type{Point}, xy::XYTuple)
    Point(xy)
end


# A point in broadest terms.
typealias XYTupleOrPoint Union(XYTuple, Point)


# Rotation about a point.
type Rotation
    theta::Float64
    offset::Point

    function Rotation()
        new(0.0, Point(0., 0.))
    end

    function Rotation(theta::Number)
        Rotation(theta, 0.0, 0.0)
    end

    function Rotation(theta::Number, offset::XYTupleOrPoint)
        new(convert(Float64, theta), convert(Point, offset))
    end

    function Rotation(theta::Number,
        offset_x::MeasureOrNumber,
        offset_y::MeasureOrNumber)
        new(convert(Float64, theta), Point(offset_x, offset_y))
    end

    # copy constructor
    function Rotation(rot::Rotation)
        new(copy(rot.theta),
            copy(rot.offset))
    end
end


copy(rot::Rotation) = Rotation(rot)


type NativeTransform
    M::Matrix{Float64}

    function NativeTransform()
        new([1.0 0.0 0.0
        0.0 1.0 0.0
        0.0 0.0 1.0])
    end

    function NativeTransform(M::Matrix{Float64})
        new(M)
    end
end


const identity_native_transform = NativeTransform()


function isidentity(a::NativeTransform)
    a.M == identity_native_transform.M
end


function combine(a::NativeTransform, b::NativeTransform)
    NativeTransform(a.M * b.M)
end


# A rectangle defining a coordinate system
type BoundingBox
    x0::Measure
    y0::Measure
    width::Measure
    height::Measure

    function BoundingBox(x0::MeasureOrNumber,
        y0::MeasureOrNumber,
        width::MeasureOrNumber,
        height::MeasureOrNumber)
        new(x_measure(x0),
        y_measure(y0),
        x_measure(width),
        y_measure(height))
    end

    # copy-constructor
    function BoundingBox(box::BoundingBox)
        new(copy(box.x0), copy(box.y0),
            copy(box.width), copy(box.height))
    end
end


BoundingBox() = BoundingBox(0, 0, 1, 1)


copy(box::BoundingBox) = BoundingBox(box)


type NativeBoundingBox{M <: NativeMeasure}
    x0::M
    y0::M
    width::M
    height::M
end


# Context-dependent interpretation of bare numbers.

x_measure(u::Measure) = u
x_measure(u::Number) = SimpleMeasure{CanvasXUnit}(convert(Float64, u))

y_measure(u::Measure) = u
y_measure(u::Number) = SimpleMeasure{CanvasYUnit}(convert(Float64, u))

# TODO: defaulting to pixels is not reasonable. Maybe pt?
size_measure(u::Measure) = u
size_measure(u::Number) = SimpleMeasure{PixelUnit}(convert(Float64, u))


# Estimation of text extents using pango.

const libpango = dlopen("libpangoft2-1.0")

const PANGO_SCALE = 1024.0

type PangoLayout
    fm::Ptr{Void}
    ctx::Ptr{Void}
    layout::Ptr{Void}

    function PangoLayout()
        fm = ccall(dlsym(libpango, :pango_ft2_font_map_new),
                   Ptr{Void}, ())

        ctx = ccall(dlsym(libpango, :pango_font_map_create_context),
                    Ptr{Void}, (Ptr{Void},), fm)

        layout = ccall(dlsym(libpango, :pango_layout_new),
                       Ptr{Void}, (Ptr{Void},), ctx)

        new(fm, ctx, layout)

        # TODO: finalizer?
    end
end


function pango_set_font(pangolayout::PangoLayout, family::String, pts::Number)
    desc_str = @sprintf("%s %f", family, pts)
    desc = ccall(dlsym(libpango, :pango_font_description_from_string),
                 Ptr{Void}, (Ptr{Uint8},), bytestring(desc_str))

    ccall(dlsym(libpango, :pango_layout_set_font_description),
          Void, (Ptr{Void}, Ptr{Void}), pangolayout.layout, desc)

    ccall(dlsym(libpango, :pango_font_description_free),
          Void, (Ptr{Void},), desc)
end


function pango_text_extents(pangolayout::PangoLayout, text::String)
    ccall(dlsym(libpango, :pango_layout_set_text),
          Void, (Ptr{Void}, Ptr{Uint8}, Int32),
          pangolayout.layout, bytestring(text), length(text))

    extents = Array(Int32, 4)
    ccall(dlsym(libpango, :pango_layout_get_extents),
          Void, (Ptr{Void}, Ptr{Int32}, Ptr{Int32}),
          pangolayout.layout, extents, C_NULL)

    extents

    width, height = (extents[3] / PANGO_SCALE)pt, (extents[4] / PANGO_SCALE)pt
end


function text_extents(font_family::String, pts::Float64, texts::String...)
    layout = PangoLayout()
    pango_set_font(layout, font_family, pts)
    max_width  = 0mm
    max_height = 0mm
    for text in texts
        (width, height) = pango_text_extents(layout, text)
        max_width  = max_width.value  < width.value  ? width  : max_width
        max_height = max_height.value < height.value ? height : max_height
    end
    (max_width, max_height)
end


function text_extents(font_family::String, size::SimpleMeasure{MillimeterUnit},
                      texts::String...)
    text_extents(font_family, size/pt, texts...)
end


