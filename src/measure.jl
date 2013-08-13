
# This file contains code pertaining to "measures", which are some unit of
# length. Compose supports specifying measures in absolute terms (e.g., 15mm),
# but also in terms of the width/height of the parent canvas (e.g, 0.5w, 0.25h),
# or in "canvas coordinates" which is an arbitrary coordinate system given to
# the canvas (e.g, 10cx, -5cy).

export Measure, BoundingBox, text_extents,
       mm, cm, inch, furlong, pt, w, h, px, cx, cy

# A unit gives some semantic meaning to a bare number.
abstract Unit
show(io::IO, ::Type{Unit}) = print(io, "??")

# Units defined in terms of the screen.
abstract PixelUnit <: Unit
show(io::IO, ::Type{PixelUnit}) = print(io, "px")

# Units of absolute length.
abstract MillimeterUnit <: Unit
show(io::IO, ::Type{MillimeterUnit}) = print(io, "mm")

# Units proportional to the width/height of the containing canvas.
abstract WidthUnit <: Unit
show(io::IO, ::Type{WidthUnit}) = print(io, "width")

abstract HeightUnit <: Unit
show(io::IO, ::Type{HeightUnit}) = print(io, "height")

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


function show{U}(io::IO, a::SimpleMeasure{U})
    print(io, a.value, U)
end


function json(a::SimpleMeasure)
    repr(string(a))
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


# Unit of a simple measure.
measure_unit{U}(a::SimpleMeasure{U}) = U


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

function show(io::IO, a::CompoundMeasure)
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
        if has(c.values, k)
            c.values[k] += v
        else
            c.values[k] = v
        end
    end
    c
end


function -(a::CompoundMeasure, b::CompoundMeasure)
    c = CompoundMeasure(copy(a.values))
    for (k, v) in b.values
        if has(c.values, k)
            c.values[k] -= v
        else
            c.values[k] = -v
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
    SimpleMeasure{MillimeterUnit}(u.value / assumed_ppmm)
end


# Compute a measure at least as large as any of the given measures.
#
# Arbitrary comparisons between measures are not necessarily well defined since
# the scale of relative units are not known until the graphic is drawn. Yet we
# sometimes need to to produce a measure that is at least as long as several
# others. This max function finds such an upper bound, but it differs from the
# normal max function in that the upper bound may not be tight.
#
# TODO: Possibly a new type should be implemented to handle these sorts of
# situations. Essentially a lazy maximum measure that gets evaluated once
# relative units are resolved.
#
function max(measures::Measure...)
    maximums = Dict{Type, Float64}()
    for measure in measures
        if isa(measure, CompoundMeasure)
            for (t, value) in measure.values
                if has(maximums, t)
                    maximums[t] = max(maximums[t], value)
                else
                    maximums[t] = value
                end
            end
        else
            t = measure_unit(measure)
            if has(maximums, t)
                maximums[t] = max(maximums[t], measure.value)
            else
                maximums[t] = measure.value
            end
        end
    end

    if length(maximums) == 1
        for (t, value) in maximums
            return SimpleMeasure{t}(value)
        end
    else
        CompoundMeasure(maximums)
    end
end

max(measures::Vector{Measure}) = max(measures...)


# For user-facing functions, there is often an obvious default unit.
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


