


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


# A measure is a number annotated with a type.
abstract Measure


# A measure used by a backend
abstract NativeMeasure <: Measure


# A single number with a single associated unit.
type SimpleMeasure{U <: Unit} <: Measure
    value::Float64
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


function /{U}(a::SimpleMeasure{U}, b::SimpleMeasure{U})
    a.value / b.value
end


function /{U}(a::SimpleMeasure{U}, b::Number)
    SimpleMeasure{U}(a.value / convert(Float64, b))
end




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


function show(io, a::CompoundMeasure)
    print(io, join([sprint(print, v, k) for (k, v) in a.values], " + "))
end


function +{U, V}(a::SimpleMeasure{U}, b::SimpleMeasure{V})
    c = CompoundMeasure()
    c.values[U] = a.value
    c.values[V] = b.value
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



# Points
type Point
    x::Measure
    y::Measure

    function Point(x::Measure, y::Measure)
        new(x, y)
    end
end


# Support specifying points as tuples.
typealias MeasureOrNumber Union(Measure, Number)
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
end

function BoundingBox()
    BoundingBox(0, 0, 1, 1)
end



type NativeBoundingBox{M <: NativeMeasure}
    x0::M
    y0::M
    width::M
    height::M
end


# Context-dependent interpretation of bare numbers.

x_measure(u::Measure) = u
x_measure(u::Number) = SimpleMeasure{WidthUnit}(convert(Float64, u))

y_measure(u::Measure) = u
y_measure(u::Number) = SimpleMeasure{HeightUnit}(convert(Float64, u))

size_measure(u::Measure) = u
size_measure(u::Number) = SimpleMeasure{PixelUnit}(convert(Float64, u))


