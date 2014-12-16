

# Measures without Canvas Units
# -----------------------------

# Rather than introducing a new type to represent measures that
# have no canvas unit dimension, we just parameterize Measure
# over a special type that promotes appropriately.
immutable MeasureNil
end

const measure_nil = MeasureNil()


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

    function Measure(; abs=0.0, cx::S=zero(S), cy::T=zero(T), cw=0.0, ch=0.0)
        new(float64(abs), cx, cy, float64(cw), float64(ch))
    end
end


typealias MeasureOrNumber Union(Measure, Number)


# MeasureNil semantics
zero(::Type{MeasureNil}) = measure_nil
zero(::MeasureNil) = measure_nil

iszero{T}(a::T) = a == zero(T)

# ambiguity warning
max(::Function, b::MeasureNil) = measure_nil
min(::Function, b::MeasureNil) = measure_nil

max(a::MeasureNil, b::MeasureNil) = measure_nil
max(a::MeasureNil, b)             = b
max(a, b::MeasureNil)             = a

min(a::MeasureNil, b::MeasureNil) = measure_nil
min(a::MeasureNil, b)             = measure_nil
min(a, b::MeasureNil)             = measure_nil

isless(a::MeasureNil, b::MeasureNil) = false


function -{S,T}(a::Measure{S, T})
    return Measure{S, T}(-a.abs, -a.cx, -a.cy, -a.cw, -a.ch)
end


function zero(::Type{Measure})
    return Measure()
end


function zero(::Measure)
    return Measure()
end


function Measure(abs, cx=measure_nil, cy=measure_nil, cw=0.0, ch=0.0)
    Measure{typeof(cx), typeof(cy)}(abs, cx, cy, cw, ch)
end


function Measure(; abs=0.0, cx=measure_nil, cy=measure_nil, cw=0.0, ch=0.0)
    Measure{typeof(cx), typeof(cy)}(abs, cx, cy, cw, ch)
end


const zero_measure = Measure()


# copy with substitutions
function Measure{S, T}(u::Measure{S, T};
                       abs=nothing, cx=nothing, cy=nothing,
                       cw=nothing, ch=nothing)
    return Measure(abs === nothing ? u.abs : abs,
                   cx  === nothing ? u.cx  : cx,
                   cy  === nothing ? u.cy  : cy,
                   cw  === nothing ? u.cw  : cw,
                   ch  === nothing ? u.ch  : ch)
end


function zero{S, T}(::Type{Measure{S, T}})
    Measure{S, T}()
end


function isless{T, S}(a::Measure{T, S}, b::Measure{T, S})
    return (a.abs < b.abs ||
            a.cx  < b.cx ||
            a.cy  < b.cy ||
            a.cw  < b.cw ||
            a.ch  < b.ch) &&
           a.abs <= b.abs &&
           a.cx  <= b.cx &&
           a.cy  <= b.cy &&
           a.cw  <= b.cw &&
           a.ch  <= b.ch
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


# Avoid ambiguity errors
add_measure_part(a::MeasureNil, b::MeasureNil) = measure_nil
add_measure_part(a::MeasureNil, b)             = b
add_measure_part(a, b::MeasureNil)             = a
add_measure_part(a, b)                         = a + b

sub_measure_part(a::MeasureNil, b::MeasureNil) = measure_nil
sub_measure_part(a::MeasureNil, b)             = b
sub_measure_part(a, b::MeasureNil)             = a
sub_measure_part(a, b)                         = a - b

*(a::MeasureNil, b::MeasureNil) = error("Two measure_nil objects multiplied")
*(a::MeasureNil, b)             = measure_nil
*(a, b::MeasureNil)             = measure_nil

/(a::MeasureNil, b::MeasureNil) = error("Division by a measure_nil")
/(a, b::MeasureNil)             = error("Division by a measure_nil")
/(a::MeasureNil, b)             = measure_nil


function +(a::Measure, b::Measure)
    Measure(a.abs + b.abs,
            add_measure_part(a.cx, b.cx),
            add_measure_part(a.cy, b.cy),
            a.cw  + b.cw,
            a.ch  + b.ch)
end


function -(a::Measure, b::Measure)
    Measure(a.abs - b.abs,
            sub_measure_part(a.cx, b.cx),
            sub_measure_part(a.cy, b.cy),
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

        if valb == zero(typeof(valb))
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
    Measure(0.0, a)
end


function y_measure(a::Measure)
    a
end

function y_measure(a)
    Measure(0.0, measure_nil, a)
end


function size_measure(a::Measure)
    a
end

function size_measure(a)
    Measure(a)
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
#
# Unfortunately this is in general uncomputable without knowing the absolute
# size of the parent canvas which may be passed in via the last two parameters.
# If not passed, this throws an error if they would have been required.
#
function union(a::BoundingBox, b::BoundingBox, units=nothing, parent_abs_width=nothing, parent_abs_height=nothing)
    (a.width == Measure() || a.height == Measure()) && return b
    (b.width == Measure() || b.height == Measure()) && return a
    x0 = min(a.x0, b.x0)
    y0 = min(a.y0, b.y0)
    x1 = max(a.x0 + a.width, b.x0 + b.width)
    y1 = max(a.y0 + a.height, b.y0 + b.height)
    # Check whether we had any problematic computations
    for m in (x0,y0,x1,y1)
        # Pure absolute or pure relative points are fine. When they are mixed,
        # there are problems
        if !isabsolute(m) && m.abs != 0.0
            if units == nothing || parent_abs_width == nothing || parent_abs_height == nothing
                error("""Bounding boxes are uncomputable without knowledge of the
                         absolute dimensions of the top canvase due to mixing of relative
                         and absolute coordinates. Either pass the dimension as a parameter
                         or restrict the context to one kind of coordinates.""")
            end
            parent_box = AbsoluteBoundingBox(0.0,0.0,parent_abs_width,parent_abs_height)
            abb = union(absolute_units(a,IdentityTransform(),units,parent_box),
                        absolute_units(b,IdentityTransform(),units,parent_box))
            return BoundingBox(Measure(;abs = abb.x0), Measure(;abs = abb.x0),
                               Measure(;abs = abb.width), Measure(;abs = abb.height))
        end
    end
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
        return new(x0, y0, width, height)
    end

    function AbsoluteBoundingBox()
        return new(0.0, 0.0, 1.0, 1.0)
    end
end

function union(a::AbsoluteBoundingBox, b::AbsoluteBoundingBox)
    (a.width == 0.0 || a.height == 0.0) && return b
    (b.width == 0.0 || b.height == 0.0) && return a
    x0 = min(a.x0, b.x0)
    y0 = min(a.y0, b.y0)
    x1 = max(a.x0 + a.width, b.x0 + b.width)
    y1 = max(a.y0 + a.height, b.y0 + b.height)
    return AbsoluteBoundingBox(x0, y0, x1 - x0, y1 - y0)
end


# The same type-signature is used for a box used to assign
# a custom coordinate system to a canvas.
# TODO: There should not be one fixed type for every parameter
immutable UnitBox{S, T, U, V}
    x0::S
    y0::T
    width::U
    height::V

    leftpad::Measure
    rightpad::Measure
    toppad::Measure
    bottompad::Measure

    function UnitBox(x0::S, y0::T, width::U, height::V;
                     leftpad::Measure=Measure(),
                     rightpad::Measure=Measure(),
                     toppad::Measure=Measure(),
                     bottompad::Measure=Measure())
        return new(x0, y0, width, height, leftpad, rightpad, toppad, bottompad)
    end
end


function UnitBox{S,T}(width::S, height::T;
                      leftpad::Measure=Measure(),
                      rightpad::Measure=Measure(),
                      toppad::Measure=Measure(),
                      bottompad::Measure=Measure())
    x0 = zero(S)
    y0 = zero(T)

    return UnitBox{S, T, S, T}(x0, y0, width, height,
                               leftpad=leftpad, rightpad=rightpad,
                               toppad=toppad, bottompad=bottompad)
end


function UnitBox(x0, y0, width, height;
                 leftpad::Measure=Measure(),
                 rightpad::Measure=Measure(),
                 toppad::Measure=Measure(),
                 bottompad::Measure=Measure())
    x0, width  = promote(x0, width)
    y0, height = promote(y0, height)
    return UnitBox{typeof(x0), typeof(y0), typeof(width), typeof(height)}(
                   x0, y0, width, height,
                   leftpad=leftpad, rightpad=rightpad,
                   toppad=toppad, bottompad=bottompad)
end


function UnitBox()
    return UnitBox{Float64, Float64, Float64, Float64}(0.0, 0.0, 1.0, 1.0)
end


# copy with substitution
function UnitBox(units::UnitBox;
                 x0=nothing, y0=nothing, width=nothing, height=nothing,
                 leftpad=nothing, rightpad=nothing, toppad=nothing,
                 bottompad=nothing)
    return UnitBox(x0 === nothing ? units.x0 : x0,
                   y0 === nothing ? units.y0 : y0,
                   width === nothing ? units.width : width,
                   height === nothing ? units.height : height,
                   leftpad   = leftpad   === nothing ? units.leftpad : leftpad,
                   rightpad  = rightpad  === nothing ? units.rightpad : rightpad,
                   toppad    = toppad    === nothing ? units.toppad : toppad,
                   bottompad = bottompad === nothing ? units.bottompad : bottompad)
end


function NilUnitBox()
    return UnitBox{Nothing, Nothing, Nothing, Nothing}(
                nothing, nothing, nothing, nothing)
end




const nil_unit_box = NilUnitBox()


function isabsolute(units::UnitBox)
    return units.leftpad == zero_measure && units.rightpad == zero_measure &&
           units.toppad == zero_measure && units.bottompad == zero_measure
end



# Canvas Transforms
# -----------------


# Transform matrix in absolute coordinates

abstract Transform


immutable IdentityTransform <: Transform
end

immutable MatrixTransform <: Transform
    M::Matrix{Float64}

    function Transform()
        new([1.0 0.0 0.0
             0.0 1.0 0.0
             0.0 0.0 1.0])
    end

    function MatrixTransform(M::Matrix{Float64})
        new(M)
    end
end

function combine(a::IdentityTransform, b::IdentityTransform)
    return a
end


function combine(a::IdentityTransform, b::MatrixTransform)
    return b
end


function combine(a::MatrixTransform, b::IdentityTransform)
    return a
end


function combine(a::MatrixTransform, b::MatrixTransform)
    MatrixTransform(a.M * b.M)
end


# Rotation about a point.
type Rotation
    theta::Float64
    offset::Point

    function Rotation()
        new(0.0, Point(0.5w, 0.5h))
    end

    function Rotation(theta::Number)
        Rotation(theta, 0.5w, 0.5h)
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


function convert(::Type{Transform}, rot::Rotation)
    if rot.theta == 0.0
        return IdentityTransform()
    else
        ct = cos(rot.theta)
        st = sin(rot.theta)
        x0 = rot.offset.x - (ct * rot.offset.x - st * rot.offset.y)
        y0 = rot.offset.y - (st * rot.offset.x + ct * rot.offset.y)
        return MatrixTransform([ct  -st  x0.abs
                                st   ct  y0.abs
                                0.0 0.0  1.0])
    end
end

# Mirror about a point at a given angle
type Mirror
    theta::Float64
    point::Point

    function Mirror()
        new(0.0, Point(0.5w, 0.5h))
    end

    function Mirror(theta::Number)
        Mirror(theta, 0.5w, 0.5h)
    end

    function Mirror(theta::Number, offset::XYTupleOrPoint)
        new(convert(Float64, theta), convert(Point, offset))
    end

    function Mirror(theta::Number, offset_x, offset_y)
        new(convert(Float64, theta), Point(offset_x, offset_y))
    end

    # copy constructor
    function Mirror(mir::Mirror)
        new(copy(mir.theta),
            copy(mir.offset))
    end
end

function convert(::Type{Transform}, mir::Mirror)
    n = [cos(mir.theta), sin(mir.theta)]
    x0 = mir.point.x
    y0 = mir.point.y

    offset = (2I - 2n*n') * [x0.abs, y0.abs]
    scale  = (2n*n' - I)
    M = vcat(hcat(scale, offset), [0 0 1])

    MatrixTransform(M)
end


copy(mir::Mirror) = Mirror(mir)


# Conversion to absolute units
# ----------------------------



# Covert a Measure to a Float64
function absolute_units(u::Measure,
                        t::Transform,
                        unit_box::UnitBox,
                        parent_box::AbsoluteBoundingBox)
    add_measure_part(u.abs,
      add_measure_part(
        add_measure_part((u.cx / unit_box.width) * parent_box.width,
                         (u.cy / unit_box.height) * parent_box.height),
        u.cw * parent_box.width +
        u.ch * parent_box.height))
end


function absolute_units(units::UnitBox, t::Transform, parent_units::UnitBox,
                        box::AbsoluteBoundingBox)
    if isabsolute(units)
        return units
    else
        leftpad   = absolute_units(units.leftpad, t, parent_units, box)
        rightpad  = absolute_units(units.rightpad, t, parent_units, box)
        toppad    = absolute_units(units.toppad, t, parent_units, box)
        bottompad = absolute_units(units.bottompad, t, parent_units, box)

        # just give up trying to pad the units if it's impossible
        if leftpad + rightpad >= box.width ||
           toppad + bottompad >= box.height
            return UnitBox(units.x0, units.y0, units.width, units.height)
       end

        width = units.width * (box.width / (box.width - leftpad - rightpad))
        height = units.height * (box.height / (box.height - toppad - bottompad))
        x0 = units.x0 - width * (leftpad / box.width)
        y0 = units.y0 - height * (toppad / box.height)

        return UnitBox(x0, y0, width, height)
    end
end


# Convert a Rotation to a Transform
function absolute_units(rot::Rotation,
                        t::MatrixTransform,
                        unit_box::UnitBox,
                        parent_box::AbsoluteBoundingBox)

    absrot = Rotation(rot.theta,
                      absolute_units(rot.offset, t, unit_box, parent_box))

    rott = convert(Transform, absrot)
    if isa(rott, IdentityTransform)
        theta = 0.0
    else
        theta = atan2(rott.M[2,1], rott.M[1,1])
    end

    return Rotation(theta, absrot.offset)
end

function absolute_units(rot::Rotation,
                        t::IdentityTransform,
                        unit_box::UnitBox,
                        parent_box::AbsoluteBoundingBox)

    return Rotation(rot.theta, absolute_units(rot.offset, t, unit_box, parent_box))
end


function absolute_units(mir::Mirror,
                        t::MatrixTransform,
                        unit_box::UnitBox,
                        parent_box::AbsoluteBoundingBox)

    theta = atan2(t.M[2,1], t.M[1,1])
    Mirror(mir.theta + theta,
            absolute_units(mir.point, t, unit_box, parent_box))
end

function absolute_units(mir::Mirror,
                        t::IdentityTransform,
                        unit_box::UnitBox,
                        parent_box::AbsoluteBoundingBox)

    Mirror(mir.theta,
           absolute_units(mir.point, t, unit_box, parent_box))
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
                        t::MatrixTransform,
                        unit_box::UnitBox,
                        parent_box::AbsoluteBoundingBox)
    x = absolute_x_position(point.x, t, unit_box, parent_box)
    y = absolute_y_position(point.y, t, unit_box, parent_box)
    xyt = t.M * [x, y, 1.0]
    return Point(Measure(xyt[1]), Measure(xyt[2]))
end


function absolute_units(point::Point,
                        t::IdentityTransform,
                        unit_box::UnitBox,
                        parent_box::AbsoluteBoundingBox)
    x = absolute_x_position(point.x, t, unit_box, parent_box)
    y = absolute_y_position(point.y, t, unit_box, parent_box)
    return Point(Measure(x), Measure(y))
end

