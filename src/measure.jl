
import Base: zero, copy, show, *, /, max

# All measures in Compose are specified as the sum
immutable Measure{T <: Number}
    abs::T # absolute measurement in millimeters
    cx::T  # canvas x-units
    cy::T  # canvas y-units
    cw::T  # proportion of canvas width
    ch::T  # proportion of canvas height

    function Measure(abs::T, cx::T, cy::T, cw::T, ch::T)
        new(abs, cx, cy, cw, ch)
    end

    function Measure(; abs::T=zero(T), cx::T=zero(T), cy::T=zero(T),
                     cw::T=zero(T), ch::T=zero(T))
        new(abs, cx, cy, cw, ch)
    end
end


function Measure(abs, cx, cy, cw, ch)
    abs, cx, cy, cw, ch = promote(abs, cx, cy, cw, ch)
    Measure{typeof(abs)}(abs, cx, cy, cw, ch)
end


function Measure(; abs=0.0, cx=0.0, cy=0.0, cw=0.0, ch=0.0)
    abs, cx, cy, cw, ch = promote(abs, cx, cy, cw, ch)
    T = typeof(abs)
    Measure{T}(abs, cx, cy, cw, ch)
end


function zero{T}(::Type{Measure{T}})
    Measure{T}()
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

function +(a::Measure, b::Measure)
    Measure(a.abs + b.abs, a.cx + b.cx, a.cy + b.cy, a.cw + b.cw, a.ch + b.ch)
end


function -(a::Measure, b::Measure)
    Measure(a.abs - b.abs, a.cx - b.cx, a.cy - b.cy, a.cw - b.cw, a.ch - b.ch)
end


function *(a::Measure, b::Measure)
    error("A Measure can not be multiplied by another Measure.")
end


function /{T, S}(a::Measure{T}, b::Measure{S})
    factor = nothing

    for unit in (:abs, :cx, :cy, :cw, :ch)
        vala = getfield(a, unit)
        valb = getfield(b, unit)

        if vala == zero(T) && valb == zero(S)
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


copy(point::Point) = Point(point)


