

# Measures without Canvas Units
# -----------------------------

# Rather than introducing a new type to represent measures that
# have no canvas unit dimension, we just parameterize Measure
# over a special type that promotes appropriately.
immutable MeasureNil
end

const measure_nil = MeasureNil()

zero(::Type{MeasureNil}) = measure_nil
zero(::MeasureNil) = measure_nil

# ambiguity warning
max(::Function, b::MeasureNil) = measure_nil
min(::Function, b::MeasureNil) = measure_nil

max(a::MeasureNil, b::MeasureNil) = measure_nil
max(a::MeasureNil, b)             = b
max(a, b::MeasureNil)             = a

min(a::MeasureNil, b::MeasureNil) = measure_nil
min(a::MeasureNil, b)             = measure_nil
min(a, b::MeasureNil)             = measure_nil

+(a::MeasureNil, b::MeasureNil) = measure_nil
+(a::MeasureNil, b)             = b
+(a, b::MeasureNil)             = a

-(a::MeasureNil, b::MeasureNil) = measure_nil
-(a::MeasureNil, b)             = -b
-(a, b::MeasureNil)             =  a
-(a::MeasureNil)                = measure_nil

*(a::MeasureNil, b::MeasureNil) = error("Two measure_nil objects multiplied")
*(a::MeasureNil, b)             = measure_nil
*(a, b::MeasureNil)             = measure_nil

/(a::MeasureNil, b::MeasureNil) = error("Division by a measure_nil")
/(a, b::MeasureNil)             = error("Division by a measure_nil")
/(a::MeasureNil, b)             = measure_nil


# All measures in Compose are specified as the sum
immutable Measure{S, T}
    abs::Float64 # absolute measurement in millimeters
    cx::S        # canvas x-units
    cy::T        # canvas y-units
    cw::Float64  # proportion of canvas width
    ch::Float64  # proportion of canvas height

    function Measure(abs::Number, cx::S, cy::T, cw::Number, ch::Number)
        new(float64(abs), cx, cy, float64(cw), float64(ch))
    end

    function Measure(; abs=0.0, cx::S=zero(T), cy::T=zero(T), cw=0.0, ch=0.0)
        new(float64(abs), cx, cy, float64(cw), float64(ch))
    end
end


function -{S,T}(a::Measure{S, T})
    return Measure{S, T}(-a.abs, -a.cx, -a.cy, -a.cw, -a.ch)
end


function zero(::Type{Measure})
    return Measure()
end


function zero(::Measure)
    return Measure()
end


function Measure(abs, cx, cy, cw, ch)
    Measure{typeof(cx), typeof(cy)}(abs, cx, cy, cw, ch)
end


function Measure(; abs=0.0, cx=measure_nil, cy=measure_nil, cw=0.0, ch=0.0)
    Measure{typeof(cx), typeof(cy)}(abs, cx, cy, cw, ch)
end


# copy wiile substituting
function Measure{S, T}(u::Measure{S, T};
                    abs=nothing, cx=nothing, cy=nothing,
                    cw=nothing, ch=nothing)
    Measure(abs === nothing ? u.abs : abs,
            cx  === nothing ? u.cx  : cx,
            cy  === nothing ? u.cy  : cy,
            cw  === nothing ? u.cw  : cw,
            ch  === nothing ? u.ch  : ch)
end


typealias MeasureOrNumber Union(Measure, Number)


function zero{S, T}(::Type{Measure{S, T}})
    Measure{S, T}()
end


function isabsolute{S, T}(u::Measure{S, T})
    iszero(u.cx) && iszero(u.cy) && u.cw == 0.0 && u.ch == 0.0
end


function copy{S, T}(a::Measure{S, T})
    Measure{S, T}(abs=a.abs, cx=a.cx, cy=a.cy, cw=a.cw, ch=a.ch)
end


function show{S, T}(io::IO, a::Measure{S, T})
    first = true
    xz = zero(S)
    yz = zero(T)

    if a.abs != 0.0
        print(io, a.abs, "mm")
        first = false
    end

    if a.cx != xz
        if !first
            print(io, " + ")
        end
        print(io, a.cx, "cx")
        first = false
    end

    if a.cy != yz
        if !first
            print(io, " + ")
        end
        print(io, a.cy, "cy")
        first = false
    end

    if a.cw != 0.0
        if !first
            print(io, " + ")
        end
        print(io, a.cw, "w")
        first = false
    end

    if a.ch != 0.0
        if !first
            print(io, " + ")
        end
        print(io, a.ch, "h")
        first = false
    end

    if first
        print(io, "Measure()")
    end
end



# Measure Arithmatic
# ------------------

function +(a::Measure, b::Measure)
    Measure(a.abs + b.abs,
            a.cx  + b.cx,
            a.cy  + b.cy,
            a.cw  + b.cw,
            a.ch  + b.ch)
end


function -(a::Measure, b::Measure)
    Measure(a.abs - b.abs,
            a.cx  - b.cx,
            a.cy  - b.cy,
            a.cw  - b.cw,
            a.ch  - b.ch)
end


function *(a::Measure, b::Measure)
    error("A Measure can not be multiplied by another Measure.")
end


function /{T, S}(a::Measure{T}, b::Measure{S})
    factor = nothing

    for unit in (:abs, :cx, :cy, :cw, :ch)
        vala = getfield(a, unit)
        valb = getfield(b, unit)

        if vala == zero(typeof(vala)) &&
           valb == zero(typeof(valb))
            continue
        end

        if factor === nothing
            factor = vala / valb
        else
            if vala / valb != factor
                error("Measure {a} is not divisible by measure {b}")
            end
        end
    end

    if factor === nothing
        error("An empty measure can not be divided by another empty measure.")
    else
        factor
    end
end


# Return a measure that is at least as large as anything else
function max(measures::Measure...)
    # current maximums
    abs = 0.0
    cx = measure_nil
    cy = measure_nil
    cw = 0.0
    ch = 0.0

    for measure in measures
        abs = max(abs, measure.abs)
        cx  = max(cx, measure.cx)
        cy  = max(cy, measure.cy)
        cw  = max(cw, measure.cw)
        ch  = max(ch, measure.ch)
    end

    Measure(abs, cx, cy, cw, ch)
end


# Return a measure that at least as small as everything else
function min(measures::Measure...)
    # current maximums
    abs = Inf
    cx = Inf
    cy = Inf
    cw = Inf
    ch = Inf

    for measure in measures
        abs = min(abs, measure.abs)
        cx  = min(cx, measure.cx)
        cy  = min(cy, measure.cy)
        cw  = min(cw, measure.cw)
        ch  = min(ch, measure.ch)
    end

    Measure(abs, cx, cy, cw, ch)
end


# Versus plain numbers

function *(a::Measure, b::Number)
    Measure(a.abs*b, a.cx*b, a.cy*b, a.cw*b, a.ch*b)
end

function *(a::Number, b::Measure)
    b * a
end


function /{T}(a::Measure{T}, b::Number)
    Measure(a.abs/b, a.cx/b, a.cy/b, a.cw/b, a.ch/b)
end



# Measure Constants
# -----------------

# Measure instances are usually constructed by multiplying by one of these
# constants. E.g. "10mm"

const mm   = Measure(abs=1.0)
const cm   = Measure(abs=10.0)
const inch = Measure(abs=25.4)
const pt   = inch/72.0
const w    = Measure(cw=1.0)
const h    = Measure(ch=1.0)
const cx   = Measure(cx=1.0)
const cy   = Measure(cy=1.0)

# Pixels are not typically used in Compose in preference of absolute
# measurements or measurements relative to parent canvases. So for the
# 'px' constant, we just punt and give something do something vagually
# reasonable.

const assumed_ppmm = 3.78 # equivalent to 96 DPI
const px = Measure(abs=1.0/assumed_ppmm)


# Interpretation of bare numbers
# ------------------------------

function x_measure(a::Measure)
    a
end

function x_measure(a)
    Measure(cx=a)
end


function y_measure(a::Measure)
    a
end

function y_measure(a)
    Measure(cy=a)
end


function size_measure(a::Measure)
    a
end

function size_measure(a)
    Measure(abs=a)
end


# Higher-order measures
# ---------------------

immutable Point
    x::Measure
    y::Measure

    function Point()
        new(Measure(), Measure())
    end

    function Point(x, y)
        new(x_measure(x), y_measure(y))
    end

    function Point(point::Point)
        new(point.x, point.y)
    end

    function Point(xy::NTuple{2})
        Point(x_measure(xy[1]), y_measure(xy[2]))
    end
end


function copy(point::Point)
    Point(point)
end


function isabsolute(point::Point)
    isabsolute(point.x) && isabsolute(point.y)
end


typealias XYTupleOrPoint Union(NTuple{2}, Point)


function convert(::Type{Point}, xy::NTuple{2})
    Point(xy[1], xy[2])
end


# Bounding Boxes
# --------------

# Measure based bounding box used to specify the extent and positioning
# of canvases.
immutable BoundingBox
    x0::Measure
    y0::Measure
    width::Measure
    height::Measure

    function BoundingBox(x0, y0, width, height)
        new(x_measure(x0), y_measure(y0),
            x_measure(width), y_measure(height))
    end

    function BoundingBox(box::BoundingBox)
        new(box.x0, box.y0, box.width, box.height)
    end

    function BoundingBox(box::BoundingBox;
                         x0=nothing,
                         y0=nothing,
                         width=nothing,
                         height=nothing)
        BoundingBox(x0     === nothing ? box.x0     : x0,
                    y0     === nothing ? box.y0     : y0,
                    width  === nothing ? box.width  : width,
                    height === nothing ? box.height : height)
    end


    function BoundingBox()
        new(0cx, 0cy, 1cx, 1cy)
    end
end


function copy(box::BoundingBox)
    BoundingBox(box)
end


# Compute the union of two bounding boxes.
#
# In other words, given two bounding boxes, return a new bounding box that
# contains both.
function union(a::BoundingBox, b::BoundingBox)
    x0 = min(a.x0, b.x0)
    y0 = min(a.y0, b.y0)
    x1 = max(a.x0 + a.width, b.x0 + b.width)
    y1 = max(a.y0 + a.height, b.y0 + b.height)
    return BoundingBox(x0, y0, x1 - x0, y1 - y0)
end


# Absolute measure bounding box. This is used to keep track of
# coordinates when traversing canvas trees to perform drawing.
immutable AbsoluteBoundingBox
    x0::Float64
    y0::Float64
    width::Float64
    height::Float64

    function AbsoluteBoundingBox(x0::Number, y0::Number, width::Number, height::Number)
        new(x0, y0, width, height)
    end

    function AbsoluteBoundingBox()
        new(0.0, 0.0, 1.0, 1.0)
    end
end




# The same type-signature is used for a box used to assign
# a custom coordinate system to a canvas.
# TODO: There should not be one fixed type for every parameter
immutable UnitBox{S, T, U, V}
    x0::S
    y0::T
    width::U
    height::V

    function UnitBox(x0::S, y0::T, width::U, height::V)
        new(x0, y0, width, height)
    end
end


function UnitBox{S,T}(width::S, height::T)
    x0 = zero(S)
    y0 = zero(T)

    UnitBox{S, T, S, T}(x0, y0, width, height)
end


function UnitBox(x0, y0, width, height)
    x0, width  = promote(x0, width)
    y0, height = promote(y0, height)
    UnitBox{typeof(x0), typeof(y0), typeof(width), typeof(height)}(
        x0, y0, width, height)
end


function UnitBox()
    UnitBox{Float64, Float64, Float64, Float64}(0.0, 0.0, 1.0, 1.0)
end


# Canvas Transforms
# -----------------


# Transform matrix in absolute coordinates
immutable Transform
    M::Matrix{Float64}

    function Transform()
        new([1.0 0.0 0.0
             0.0 1.0 0.0
             0.0 0.0 1.0])
    end

    function Transform(M::Matrix{Float64})
        new(M)
    end
end


const identity_transform = Transform()


function isidentity(a::Transform)
    a.M == identity_transform.M
end


function combine(a::Transform, b::Transform)
    Transform(a.M * b.M)
end


# Rotation about a point.
type Rotation
    theta::Float64
    offset::Point

    function Rotation()
        new(0.0, Point())
    end

    function Rotation(theta::Number)
        Rotation(theta, 0.0, 0.0)
    end

    function Rotation(theta::Number, offset::XYTupleOrPoint)
        new(convert(Float64, theta), convert(Point, offset))
    end

    function Rotation(theta::Number, offset_x, offset_y)
        new(convert(Float64, theta), Point(offset_x, offset_y))
    end

    # copy constructor
    function Rotation(rot::Rotation)
        new(copy(rot.theta),
            copy(rot.offset))
    end
end


copy(rot::Rotation) = Rotation(rot)


# Conversion to absolute units
# ----------------------------



# Covert a Measure to a Float64
function absolute_units(u::Measure,
                        t::Transform,
                        unit_box::UnitBox,
                        parent_box::AbsoluteBoundingBox)
    u.abs +
      (u.cx / unit_box.width) * parent_box.width +
      (u.cy / unit_box.height) * parent_box.height +
      u.cw * parent_box.width +
      u.ch * parent_box.height
end



# Convert a Rotation to a Transform
function absolute_units(rot::Rotation,
                        t::Transform,
                        unit_box::UnitBox,
                        parent_box::AbsoluteBoundingBox)

    off = absolute_units(rot.offset, t, unit_box, parent_box)
    ct = cos(rot.theta)
    st = sin(rot.theta)
    x0 = off.x - (ct * off.x - st * off.y)
    y0 = off.y - (st * off.x + ct * off.y)

    Transform([ct  -st  x0.abs
               st   ct  y0.abs
               0.0 0.0  1.0])
end


function absolute_position_cx(::MeasureNil, unit_box::UnitBox,
                              parent_box::AbsoluteBoundingBox)
    0.0
end


function absolute_position_cx(cx, unit_box::UnitBox,
                              parent_box::AbsoluteBoundingBox)
    float64(((cx - unit_box.x0) / unit_box.width)) * parent_box.width
end


function absolute_position_cy(::MeasureNil, unit_box::UnitBox,
                              parent_box::AbsoluteBoundingBox)
    0.0
end


function absolute_position_cy(cy, unit_box::UnitBox,
                              parent_box::AbsoluteBoundingBox)
    float64(((cy - unit_box.y0) / unit_box.height)) * parent_box.height
end


function absolute_x_position(u::Measure,
                             t::Transform,
                             unit_box::UnitBox,
                             parent_box::AbsoluteBoundingBox)
    parent_box.x0 +
      u.abs +
      absolute_position_cx(u.cx, unit_box, parent_box) +
      absolute_position_cy(u.cy, unit_box, parent_box) +
      u.cw * parent_box.width +
      u.ch * parent_box.height
end


function absolute_y_position(u::Measure,
                             t::Transform,
                             unit_box::UnitBox,
                             parent_box::AbsoluteBoundingBox)
    parent_box.y0 +
      u.abs +
      absolute_position_cx(u.cx, unit_box, parent_box) +
      absolute_position_cy(u.cy, unit_box, parent_box) +
      u.cw * parent_box.width +
      u.ch * parent_box.height
end


# Convert a BoundingBox to a AbsoluteBoundingBox
function absolute_units(box::BoundingBox,
                        t::Transform,
                        unit_box::UnitBox,
                        parent_box::AbsoluteBoundingBox)
    AbsoluteBoundingBox(
        absolute_x_position(box.x0, t, unit_box, parent_box),
        absolute_y_position(box.y0, t, unit_box, parent_box),
        absolute_units(box.width, t, unit_box, parent_box),
        absolute_units(box.height, t, unit_box, parent_box))
end


# Convert a Point to a Point in absolute units
function absolute_units(point::Point,
                        t::Transform,
                        unit_box::UnitBox,
                        parent_box::AbsoluteBoundingBox)
    x = absolute_x_position(point.x, t, unit_box, parent_box)
    y = absolute_y_position(point.y, t, unit_box, parent_box)
    xyt = t.M * [x, y, 1.0]

    Point(Measure(abs=xyt[1]), Measure(abs=xyt[2]))
end


