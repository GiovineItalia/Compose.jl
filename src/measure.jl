

# All measures in Compose are specified as the sum
immutable Measure{T <: Number}
    abs::Float64 # absolute measurement in millimeters
    cx::T        # canvas x-units
    cy::T        # canvas y-units
    cw::Float64  # proportion of canvas width
    ch::Float64  # proportion of canvas height

    function Measure(abs::Number, cx::T, cy::T, cw::Number, ch::Number)
        new(abs, cx, cy, cw, ch)
    end

    function Measure(; abs::T=0.0, cx::T=zero(T), cy::T=zero(T),
                     cw::T=0.0, ch::T=0.0)
        new(abs, cx, cy, cw, ch)
    end
end


function Measure(abs, cx, cy, cw, ch)
    cx, cy = promote(cx, cy)
    Measure{typeof(cx)}(abs, cx, cy, cw, ch)
end


function Measure(; abs=0.0, cx=0.0, cy=0.0, cw=0.0, ch=0.0)
    cx, cy = promote(cx, cy)
    Measure{typeof(cx)}(abs, cx, cy, cw, ch)
end


# copy wiile substituting
function Measure{T}(u::Measure{T};
                    abs=nothing, cx=nothing, cy=nothing,
                    cw=nothing, ch=nothing)
    Measure(abs === nothing ? u.abs : abs,
            cx  === nothing ? u.cx  : cx,
            cy  === nothing ? u.cy  : cy,
            cw  === nothing ? u.cw  : cw,
            cy  === nothing ? u.ch  : ch)
end


typealias MeasureOrNumber Union(Measure, Number)


function zero{T}(::Type{Measure{T}})
    Measure{T}()
end


function isabsolute{T}(u::Measure{T})
    iszero(u.cx) && iszero(u.cy) && u.cw == 0.0 && u.ch == 0.0
end


function copy{T}(a::Measure{T})
    Measure(abs=a.abs, cx=a.cx, cy=a.cy, cw=a.cw, ch=a.ch)
end


function show{T}(io::IO, a::Measure{T})
    first = true
    z = zero(T)
    if a.abs != z
        print(io, a.abs, "mm")
        first = false
    end

    if a.cx != z
        if !first
            print(io, " + ")
        end
        print(io, a.cx, "cx")
        first = false
    end

    if a.cy != z
        if !first
            print(io, " + ")
        end
        print(io, a.cy, "cy")
        first = false
    end

    if a.cw != z
        if !first
            print(io, " + ")
        end
        print(io, a.cw, "w")
        first = false
    end

    if a.ch != z
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

function +{T, S}(a::Measure{T}, b::Measure{S})
    Measure(a.abs + b.abs,

            a.cx == zero(T) ? b.cx :
            b.cx == zero(S) ? a.cx : a.cx + b.cx,

            a.cy == zero(T) ? b.cy :
            b.cy == zero(S) ? a.cy : a.cy + b.cy,

            a.cw + b.cw,
            a.ch + b.ch)
end


function -{T, S}(a::Measure{T}, b::Measure{S})
    Measure(a.abs - b.abs,

            a.cx == zero(T) ? -b.cx :
            b.cx == zero(S) ?  a.cx : a.cx - b.cx,

            a.cy == zero(T) ? -b.cy :
            b.cy == zero(S) ?  a.cy : a.cy - b.cy,

            a.cw - b.cw,
            a.ch - b.ch)
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
    cx = 0.0
    cy = 0.0
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


# Versus plain numbers

function *(a::Measure, b)
    Measure(a.abs*b, a.cx*b, a.cy*b, a.cw*b, a.ch*b)
end

function *(a, b::Measure)
    b * a
end


function /{T}(a::Measure{T}, b)
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

const assumed_ppmm = 4.5
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


# Absolute measure bounding box. This is used to keep track of
# coordinates when traversing canvas trees to perform drawing.
immutable AbsoluteBoundingBox
    x0::Float64
    y0::Float64
    width::Float64
    height::Float64
end


# The same type-signature is used for a box used to assign
# a custom coordinate system to a canvas.
immutable UnitBox{T}
    x0::T
    y0::T
    width::T
    height::T

    function UnitBox(x0::T, y0::T, width::T, height::T)
        new(x0, y0, width, height)
    end

    function UnitBox(width, height)
        x0, y0, width, height = promote(0.0, 0.0, width, height)
        new(x0, y0, width, height)
    end

    function UnitBox(x0, y0, width, height)
        x0, y0, width, height = promote(x0, y0, width, height)
        new(x0, y0, width, height)
    end
end


function UnitBox()
    UnitBox{Float64}(0.0, 0.0, 1.0, 1.0)
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
    a.M == identity_native_transform.M
end


function combine(a::Transform, b::Transform)
    Transform(a.M * b.M)
end


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
      float64(((u.cx - unit_box.x0) / unit_box.width)) * parent_box.width +
      float64(((u.cy - unit_box.y0) / unit_box.height)) * parent_box.height +
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


# Convert a BoundingBox to a AbsoluteBoundingBox
function absolute_units(box::BoundingBox,
                        t::Transform,
                        unit_box::UnitBox,
                        parent_box::AbsoluteBoundingBox)
    AbsoluteBoundingBox(
        parent_box.x0 + absolute_units(box.x0, t, unit_box, parent_box),
        parent_box.y0 + absolute_units(box.y0, t, unit_box, parent_box),
        absolute_units(box.width, t, unit_box, parent_box),
        absolute_units(box.width, t, unit_box, parent_box))
end


# Convert a Point to a Point in absolute units
function absolute_units(point::Point,
                        t::Transform,
                        unit_box::UnitBox,
                        parent_box::AbsoluteBoundingBox)
    x = parent_box.x0 + absolute_units(point.x, t, unit_box, parent_box)
    y = parent_box.y0 + absolute_units(point.y, t, unit_box, parent_box)
    xyt = t.M * [x, y, 1.0]

    Point(Measure(abs=xyt[1]), Measure(abs=xyt[2]))
end
