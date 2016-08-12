using Measures: Add, Min, Max, Div, Mul, Neg

# Measure Constants
# -----------------

const cx = Length{:cx}
const cy = Length{:cy}

*{T}(a::T, b::Type{cx}) = x_measure(a)
*{T}(a::T, b::Type{cy}) = y_measure(a)

# Pixels are not typically used in Compose in preference of absolute
# measurements or measurements relative to parent canvases. So for the
# 'px' constant, we just punt and give something do something vaguely
# reasonable.

const assumed_ppmm = 3.78 # equivalent to 96 DPI
const px = mm/assumed_ppmm

typealias XYTupleOrVec Union{NTuple{2}, Vec}
typealias MeasureOrNumber Union{Measure, Number}


# Scaling w and h components
# --------------------------

# Compute the length of the given type.
function sum_component{T <: Length}(::Type{T}, l)
    return 0.0
end


function sum_component{T <: Length}(::Type{T}, l::T)
    return l.value
end


function sum_component{T <: Length}(::Type{T}, l::Add)
    return sum_component(T, l.a) + sum_component(T, l.b)
end


# Scale a length component by some factor.
function scale_component{T <: Length}(::Type{T}, scale, l)
    return l
end


function scale_component{T <: Length}(::Type{T}, scale, l::T)
    return T(scale * l.value)
end


function scale_component{T <: Length}(::Type{T}, scale, l::Add)
    return scale_component(T, scale, l.a) + scale_component(T, scale, l.b)
end


# Interpretation of bare numbers
# ------------------------------

x_measure(a::Measure) = a
x_measure{T}(a::T) = Length{:cx, T}(a)

y_measure(a::Measure) = a
y_measure{T}(a::T) = Length{:cy, T}(a)

x_measure{T<:Measure}(a::Vector{T}) = a
x_measure(a::Vector) = Measure[x_measure(x) for x in a]

y_measure{T<:Measure}(a::Vector{T}) = a
y_measure(a::Vector) = Measure[y_measure(y) for y in a]

size_measure(a::Measure) = a
size_measure(a) = a * mm


# Higher-order measures
# ---------------------


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
            parent_box = AbsoluteBox(0.0,0.0,parent_abs_width,parent_abs_height)
            abb = union(absolute_units(a,IdentityTransform(),units,parent_box),
                        absolute_units(b,IdentityTransform(),units,parent_box))
            return BoundingBox(Measure(;abs = abb.x0), Measure(;abs = abb.x0),
                               Measure(;abs = abb.width), Measure(;abs = abb.height))
        end
    end
    return BoundingBox(x0, y0, x1 - x0, y1 - y0)
end


function union(a::AbsoluteBox, b::AbsoluteBox)
    (a.width == 0.0 || a.height == 0.0) && return b
    (b.width == 0.0 || b.height == 0.0) && return a
    x0 = min(a.x0, b.x0)
    y0 = min(a.y0, b.y0)
    x1 = max(a.x0 + a.width, b.x0 + b.width)
    y1 = max(a.y0 + a.height, b.y0 + b.height)
    return AbsoluteBox(x0, y0, x1 - x0, y1 - y0)
end


# The same type-signature is used for a box used to assign
# a custom coordinate system to a canvas.

immutable UnitBox{S, T, U, V}
    x0::S
    y0::T
    width::U
    height::V

    leftpad::AbsoluteLength
    rightpad::AbsoluteLength
    toppad::AbsoluteLength
    bottompad::AbsoluteLength

    function UnitBox(x0::S, y0::T, width::U, height::V;
                     leftpad=0mm, rightpad=0mm, toppad=0mm, bottompad=0mm)
        return new(x0, y0, width, height, leftpad, rightpad, toppad, bottompad)
    end
end


function UnitBox{S,T}(width::S, height::T;
                      leftpad=0mm, rightpad=0mm, toppad=0mm, bottompad=0mm)
    x0 = zero(S)
    y0 = zero(T)
    return UnitBox{S, T, S, T}(x0, y0, width, height,
                               leftpad=leftpad, rightpad=rightpad,
                               toppad=toppad, bottompad=bottompad)
end


function UnitBox{S, T, U, V}(x0::S, y0::T, width::U, height::V;
                             leftpad=0mm, rightpad=0mm, toppad=0mm, bottompad=0mm)
    return UnitBox{S, T, U, V}(
                   x0, y0, width, height,
                   leftpad=leftpad, rightpad=rightpad,
                   toppad=toppad, bottompad=bottompad)
end


function UnitBox()
    return UnitBox{Float64, Float64, Float64, Float64}(0.0, 0.0, 1.0, 1.0)
end


typealias NullUnitBox Nullable{UnitBox}


# copy with substitution
function UnitBox(units::UnitBox;
                 x0=Nullable{Measure}(),
                 y0=Nullable{Measure}(),
                 width=Nullable{Measure}(),
                 height=Nullable{Measure}(),
                 leftpad=Nullable{AbsoluteLength}(),
                 rightpad=Nullable{AbsoluteLength}(),
                 toppad=Nullable{AbsoluteLength}(),
                 bottompad=Nullable{AbsoluteLength}())
    return UnitBox(ifelse(isa(x0, Nullable)     && isnull(x0),     units.x0,     x0),
                   ifelse(isa(y0, Nullable)     && isnull(y0),     units.y0,     y0),
                   ifelse(isa(width, Nullable)  && isnull(width),  units.width,  width),
                   ifelse(isa(height, Nullable) && isnull(height), units.height, height),
                   leftpad   = ifelse(isa(leftpad,   Nullable) && isnull(leftpad),   units.leftpad,   leftpad),
                   rightpad  = ifelse(isa(rightpad,  Nullable) && isnull(rightpad),  units.rightpad,  rightpad),
                   toppad    = ifelse(isa(toppad,    Nullable) && isnull(toppad),    units.toppad,    toppad),
                   bottompad = ifelse(isa(bottompad, Nullable) && isnull(bottompad), units.bottompad, bottompad))
end


Measures.width(units::UnitBox) = units.width
Measures.height(units::UnitBox) = units.height


function ispadded(units::UnitBox)
    return units.leftpad != 0mm || units.rightpad != 0mm ||
           units.toppad != 0mm || units.bottompad != 0mm
end


function isxflipped{S, T, U, V}(units::UnitBox{S, T, U, V})
    return units.width < zero(U)
end


function isyflipped{S, T, U, V}(units::UnitBox{S, T, U, V})
    return units.height < zero(V)
end


function hasunits(::Type, x::Measure)
    return false
end


function hasunits{u, T}(::Type{Length{u}}, x::Length{u, T})
    return true
end


function hasunits(T::Type, x::Measures.BinaryOp)
    return hasunits(T, x.a) || hasunits(T, x.b)
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
immutable Rotation{P <: Vec}
    theta::Float64
    offset::P

    # copy constructor
    function Rotation(theta::Float64, offset::P)
        new(theta, offset)
    end
end


function Rotation()
    Rotation(0.0, (0.5w, 0.5h))
end

function Rotation{P <: Vec}(theta::Float64, offset::P)
    Rotation{P}(theta, offset)
end

function Rotation(theta::Number)
    Rotation{Vec2}(convert(Float64, theta), (0.5w, 0.5h))
end

function Rotation(theta::Number, offset::XYTupleOrVec)
    Rotation(convert(Float64, theta), (x_measure(offset[1]), y_measure(offset[2])))
end

function Rotation(theta::Number, offset_x, offset_y)
    Rotation(convert(Float64, theta), (x_measure(offset_x), y_measure(offset_y)))
end



copy(rot::Rotation) = Rotation(rot)


function convert(::Type{Transform}, rot::Rotation)
    if rot.theta == 0.0
        return IdentityTransform()
    else
        ct = cos(rot.theta)
        st = sin(rot.theta)
        x0 = rot.offset[1] - (ct * rot.offset[1] - st * rot.offset[2])
        y0 = rot.offset[2] - (st * rot.offset[1] + ct * rot.offset[2])
        return MatrixTransform([ct  -st  x0.value
                                st   ct  y0.value
                                0.0 0.0  1.0])
    end
end


# Mirror about a point at a given angle
type Mirror
    theta::Float64
    point::Vec

    function Mirror()
        new(0.0, (0.5w, 0.5h))
    end

    function Mirror(theta::Number)
        Mirror(convert(Float64, theta), 0.5w, 0.5h)
    end

    function Mirror(theta::Number, offset::XYTupleOrVec)
        new(convert(Float64, theta), (x_measure(offset[1]), y_measure(offset[2])))
    end

    function Mirror(theta::Number, offset_x, offset_y)
        new(convert(Float64, theta), (offset_x, offset_y))
    end

    # copy constructor
    function Mirror(mir::Mirror)
        new(copy(mir.theta),
            copy(mir.offset))
    end
end


function convert(::Type{Transform}, mir::Mirror)
    n = [cos(mir.theta), sin(mir.theta)]
    x0 = mir.point[1]
    y0 = mir.point[2]

    offset = (2I - 2n*n') * [x0.abs, y0.abs]
    scale  = (2n*n' - I)
    M = vcat(hcat(scale, offset), [0 0 1])

    MatrixTransform(M)
end


copy(mir::Mirror) = Mirror(mir)


# Resolution
# ----------

function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, a::Length)
    return resolve(box, a)
end


function resolve_position(box::AbsoluteBox, units::UnitBox, t::Transform, a::Length{:cx})
    return ((a.value - units.x0) / width(units)) * box.a[1]
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, a::Length{:cx})
    return abs(a.value / width(units)) * box.a[1]
end


function resolve_position(box::AbsoluteBox, units::UnitBox, t::Transform, a::Length{:cy})
    return ((a.value - units.y0) / height(units)) * box.a[2]
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, a::Length{:cy})
    return abs(a.value / height(units)) * box.a[2]
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::Vec2)
    xy = (resolve_position(box, units, t, p[1]) + box.x0[1],
          resolve_position(box, units, t, p[2]) + box.x0[2])
    return xy
end

function resolve(box::AbsoluteBox, units::UnitBox, t::MatrixTransform, p::Vec2)
    x = resolve_position(box, units, t, p[1]) + box.x0[1]
    y = resolve_position(box, units, t, p[2]) + box.x0[2]
    xy = t.M * [x.value, y.value, 1]
    return (xy[1]mm, xy[2]mm)
end

function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, a::BoundingBox)
    return BoundingBox(resolve(box, units, t, a.x0),
                       (resolve(box, units, t, a.a[1]), resolve(box, units, t, a.a[2])))
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, a::Rotation)
    return Rotation(a.theta, resolve(box, units, t, a.offset))
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, u::UnitBox)
    if !ispadded(u)
        return u
    else
        leftpad   = resolve(box, units, t, u.leftpad)
        rightpad  = resolve(box, units, t, u.rightpad)
        toppad    = resolve(box, units, t, u.toppad)
        bottompad = resolve(box, units, t, u.bottompad)

        # just give up trying to pad the units if it's impossible
        if leftpad + rightpad >= box.a[1] ||
           toppad + bottompad >= box.a[2]
            return UnitBox(u.x0, u.y0, u.width, u.height)
        end

        width = u.width * (box.a[1] / (box.a[1] - leftpad - rightpad))
        height = u.height * (box.a[2] / (box.a[2] - toppad - bottompad))
        x0 = u.x0 - width * (leftpad / box.a[1])
        y0 = u.y0 - height * (toppad / box.a[2])

        return UnitBox(x0, y0, width, height)
    end
end


# Equivalent to the resolve functions in Measures, but pass through the `units`
# and `transform` parameters.
function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, x::Neg)
    return -resolve(box, units, t, x.a)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, x::Add)
    return resolve(box, units, t, x.a) + resolve(box, units, t, x.b)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, x::Mul)
    return resolve(box, units, t, x.a) * x.b
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, x::Div)
    return resolve(box, units, t, x.a) / x.b
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, x::Min)
    return min(resolve(box, units, t, x.a), resolve(box, units, t, x.b))
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, x::Max)
    return max(resolve(box, units, t, x.a), resolve(box, units, t, x.b))
end


resolve_position(box::AbsoluteBox, units::UnitBox, t::Transform, a) = resolve(box, units, t, a)


function resolve_position(box::AbsoluteBox, units::UnitBox, t::Transform, op::Add)
    return resolve_position(box, units, t, op.a) + resolve_position(box, units, t, op.b)
end
