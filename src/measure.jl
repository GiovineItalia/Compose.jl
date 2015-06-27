

using Measures: Add, Min, Max, Div, Mul, Neg


# Measure Constants
# -----------------

# Measure instances are usually constructed by multiplying by one of these
# constants. E.g. "10mm"


# TODO: Possibly we should have two relative unit types.
# One to indicate a position in the context, and one to indicate a size.

const cx = Length{:cx, Float64}(1.0)
const cy = Length{:cy, Float64}(1.0)

# Pixels are not typically used in Compose in preference of absolute
# measurements or measurements relative to parent canvases. So for the
# 'px' constant, we just punt and give something do something vaguely
# reasonable.

const assumed_ppmm = 3.78 # equivalent to 96 DPI
const px = mm/assumed_ppmm

typealias XYTupleOrVec Union(NTuple{2}, Vec)
typealias MeasureOrNumber Union(Measure, Number)

# Interpretation of bare numbers
# ------------------------------

x_measure(a::Measure) = a
x_measure(a) = a * cx

y_measure(a::Measure) = a
y_measure(a) = a * cy

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
                     leftpad::Measure=0mm,
                     rightpad::Measure=0mm,
                     toppad::Measure=0mm,
                     bottompad::Measure=0mm)
        return new(x0, y0, width, height, leftpad, rightpad, toppad, bottompad)
    end
end


function UnitBox{S,T}(width::S, height::T;
                      leftpad::Measure=0mm,
                      rightpad::Measure=0mm,
                      toppad::Measure=0mm,
                      bottompad::Measure=0mm)
    x0 = zero(S)
    y0 = zero(T)

    return UnitBox{S, T, S, T}(x0, y0, width, height,
                               leftpad=leftpad, rightpad=rightpad,
                               toppad=toppad, bottompad=bottompad)
end


function UnitBox(x0, y0, width, height;
                 leftpad::Measure=0mm,
                 rightpad::Measure=0mm,
                 toppad::Measure=0mm,
                 bottompad::Measure=0mm)
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


width(units::UnitBox) = units.width
width(units::UnitBox) = units.height


function NilUnitBox()
    return UnitBox{Nothing, Nothing, Nothing, Nothing}(
                nothing, nothing, nothing, nothing)
end


const nil_unit_box = NilUnitBox()


function isabsolute(units::UnitBox)
    return units.leftpad == 0mm && units.rightpad == 0mm &&
           units.toppad == 0mm && units.bottompad == 0mm
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
    @show P
    Rotation{P}(theta, offset)
end

function Rotation(theta::Number)
    Rotation{SimpleVec}(convert(Float64, theta), (0.5w, 0.5h))
end

function Rotation(theta::Number, offset::XYTupleOrVec)
    Rotation(convert(Float64, theta), convert(Vec, offset))
end

function Rotation(theta::Number, offset_x, offset_y)
    Rotation(convert(Float64, theta), (offset_x, offset_y))
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
        return MatrixTransform([ct  -st  x0.abs
                                st   ct  y0.abs
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
        new(convert(Float64, theta), convert(Vec, offset))
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


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, a::Length{:cx})
    return (a.value / width(units)) * width(box).value * mm
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, a::Length{:cy})
    return (a.value / height(units)) * height(box).value * mm
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::Vec)
    map(x -> resolve(box, units, t, x), p) + box.x0
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, a::BoundingBox)
    BoundingBox(resolve(box, units, t, box.x0), map(x -> resolve(box, units, t, x), box.a))
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, a::Rotation)
    return Rotation(a.theta, resolve(box, units, t, a.offset))
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, a::UnitBox)
    error("TODO")
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





#function absolute_units(units::UnitBox, t::Transform, parent_units::UnitBox,
                        #box::AbsoluteBox)
    #if isabsolute(units)
        #return units
    #else
        #leftpad   = absolute_units(units.leftpad, t, parent_units, box)
        #rightpad  = absolute_units(units.rightpad, t, parent_units, box)
        #toppad    = absolute_units(units.toppad, t, parent_units, box)
        #bottompad = absolute_units(units.bottompad, t, parent_units, box)

        ## just give up trying to pad the units if it's impossible
        #if leftpad + rightpad >= box.width ||
           #toppad + bottompad >= box.height
            #return UnitBox(units.x0, units.y0, units.width, units.height)
       #end

        #width = units.width * (box.width / (box.width - leftpad - rightpad))
        #height = units.height * (box.height / (box.height - toppad - bottompad))
        #x0 = units.x0 - width * (leftpad / box.width)
        #y0 = units.y0 - height * (toppad / box.height)

        #return UnitBox(x0, y0, width, height)
    #end
#end


## Convert a Rotation to a Transform
#function absolute_units(rot::Rotation,
                        #t::MatrixTransform,
                        #unit_box::UnitBox,
                        #parent_box::AbsoluteBox)

    #absrot = Rotation(rot.theta,
                      #absolute_units(rot.offset, t, unit_box, parent_box))

    #rott = convert(Transform, absrot)
    #if isa(rott, IdentityTransform)
        #theta = 0.0
    #else
        #theta = atan2(rott.M[2,1], rott.M[1,1])
    #end

    #return Rotation(theta, absrot.offset)
#end

#function absolute_units(rot::Rotation,
                        #t::IdentityTransform,
                        #unit_box::UnitBox,
                        #parent_box::AbsoluteBox)

    #return Rotation{SimpleVec}(rot.theta, absolute_units(rot.offset, t, unit_box, parent_box))
#end


#function absolute_units(mir::Mirror,
                        #t::MatrixTransform,
                        #unit_box::UnitBox,
                        #parent_box::AbsoluteBox)

    #theta = atan2(t.M[2,1], t.M[1,1])
    #Mirror(mir.theta + theta,
            #absolute_units(mir.point, t, unit_box, parent_box))
#end

#function absolute_units(mir::Mirror,
                        #t::IdentityTransform,
                        #unit_box::UnitBox,
                        #parent_box::AbsoluteBox)

    #Mirror(mir.theta,
           #absolute_units(mir.point, t, unit_box, parent_box))
#end


#function absolute_position_cx(::MeasureNil, unit_box::UnitBox,
                              #parent_box::AbsoluteBox)
    #0.0
#end


#function absolute_position_cx(cx, unit_box::UnitBox,
                              #parent_box::AbsoluteBox)
    #(@compat Float64(((cx - unit_box.x0) / unit_box.width))) * parent_box.width
#end


#function absolute_position_cy(::MeasureNil, unit_box::UnitBox,
                              #parent_box::AbsoluteBox)
    #0.0
#end


#function absolute_position_cy(cy, unit_box::UnitBox,
                              #parent_box::AbsoluteBox)
    #(@compat Float64(((cy - unit_box.y0) / unit_box.height))) * parent_box.height
#end


#function absolute_x_position(u::Measure,
                             #t::Transform,
                             #unit_box::UnitBox,
                             #parent_box::AbsoluteBox)
    #parent_box.x0 +
      #u.abs +
      #absolute_position_cx(u.cx, unit_box, parent_box) +
      #absolute_position_cy(u.cy, unit_box, parent_box) +
      #u.cw * parent_box.width +
      #u.ch * parent_box.height
#end


#function absolute_y_position(u::Measure,
                             #t::Transform,
                             #unit_box::UnitBox,
                             #parent_box::AbsoluteBox)
    #parent_box.y0 +
      #u.abs +
      #absolute_position_cx(u.cx, unit_box, parent_box) +
      #absolute_position_cy(u.cy, unit_box, parent_box) +
      #u.cw * parent_box.width +
      #u.ch * parent_box.height
#end


## Convert a BoundingBox to a AbsoluteBox
#function absolute_units(box::BoundingBox,
                        #t::Transform,
                        #unit_box::UnitBox,
                        #parent_box::AbsoluteBox)
    #AbsoluteBox(
        #absolute_x_position(box.x0, t, unit_box, parent_box),
        #absolute_y_position(box.y0, t, unit_box, parent_box),
        #absolute_units(box.width, t, unit_box, parent_box),
        #absolute_units(box.height, t, unit_box, parent_box))
#end


## Convert a Vec to a Vec in absolute units
#function absolute_units(point::Vec,
                        #t::MatrixTransform,
                        #unit_box::UnitBox,
                        #parent_box::AbsoluteBox)
    #x = absolute_x_position(point.x, t, unit_box, parent_box)
    #y = absolute_y_position(point.y, t, unit_box, parent_box)
    #xyt = t.M * [x, y, 1.0]
    #return Vec(Measure(xyt[1]), Measure(xyt[2]))
#end


#function absolute_units(point::Vec,
                        #t::IdentityTransform,
                        #unit_box::UnitBox,
                        #parent_box::AbsoluteBox)
    #x = absolute_x_position(point.x, t, unit_box, parent_box)
    #y = absolute_y_position(point.y, t, unit_box, parent_box)
    #return Vec(Measure(x), Measure(y))
#end

