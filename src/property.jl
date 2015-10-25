
abstract Property <: Primitive
typealias PropertyNode{T<:Property} Union{T,AbstractArray{T}}

# Meaningless isless function used to sort in optimize_batching
function Base.isless{T <: Property}(a::T, b::T)
    for field in fieldnames(T)
        x = getfield(a, field)
        y = getfield(b, field)
        if isa(x, Colorant)
            if color_isless(x, y)
                return true
            elseif color_isless(y, x)
                return false
            end
        else
            if x < y
                return true
            elseif x > y
                return false
            end
        end
    end
    return false
end

# Some properties can be applied multiple times, most cannot.
function isrepeatable{P<:Property}(p::Type{P})
    return false
end


# Property primitive catchall: most properties don't need measure transforms
function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 primitive::Property)
    return primitive
end


# Stroke
# ------

immutable Stroke <: Property
	color::RGBA{Float64}
end


function stroke(c::(@compat Void))
    return Stroke(RGBA{Float64}(0, 0, 0, 0))
end


function stroke(c::@compat(Union{Colorant, AbstractString}))
	return Stroke(parse_colorant(c))
end


function stroke(cs::AbstractArray)
	return [Stroke(c == nothing ? RGBA{Float64}(0, 0, 0, 0) : parse_colorant(c)) for c in cs]
end

prop_string(::PropertyNode{Stroke}) = "s"

# Fill
# ----

immutable Fill <: Property
	color::RGBA{Float64}
end


function fill(c::(@compat Void))
    return Fill(RGBA{Float64}(0.0, 0.0, 0.0, 0.0))
end


function fill(c::@compat(Union{Colorant, AbstractString}))
	return Fill(parse_colorant(c))
end


function fill(cs::AbstractArray)
	return [Fill(c == nothing ? RGBA{Float64}(0.0, 0.0, 0.0, 0.0) : parse_colorant(c)) for c in cs]
end

prop_string(::PropertyNode{Fill}) = "f"


# StrokeDash
# ----------

immutable StrokeDash <: Property
    value::Vector{Measure}
end


function strokedash(values::AbstractArray)
    return StrokeDash(values)
end


function strokedash(values::AbstractArray{AbstractArray})
    return StrokeDash[StrokeDash(value) for value in values]
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 primitive::StrokeDash)
    StrokeDash([resolve(box, units, t, v)
                    for v in primitive.value])
end

prop_string(::PropertyNode{StrokeDash}) = "sd"

# StrokeLineCap
# -------------


abstract LineCap
immutable LineCapButt <: LineCap end
immutable LineCapSquare <: LineCap end
immutable LineCapRound <: LineCap end


immutable StrokeLineCap <: Property
    value::LineCap

    function StrokeLineCap(value::LineCap)
        return new(value)
    end

    function StrokeLineCap(value::Type{LineCap})
        return new(value())
    end
end


function strokelinecap(value::@compat(Union{LineCap, Type{LineCap}}))
    return StrokeLineCap(value)
end


function strokelinecap(values::AbstractArray)
    return [StrokeLineCap(value) for value in values]
end

prop_string(::PropertyNode{StrokeLineCap}) = "slc"

# StrokeLineJoin
# --------------

abstract LineJoin
immutable LineJoinMiter <: LineJoin end
immutable LineJoinRound <: LineJoin end
immutable LineJoinBevel <: LineJoin end


immutable StrokeLineJoin <: Property
    value::LineJoin

    function StrokeLineJoin(value::LineJoin)
        return new(value)
    end

    function StrokeLineCap(value::Type{LineJoin})
        return new(value())
    end
end


function strokelinejoin(value::@compat(Union{LineJoin, Type{LineJoin}}))
    return StrokeLineJoin(value)
end


function strokelinejoin(values::AbstractArray)
    return [StrokeLineJoin(value) for value in values]
end

prop_string(::PropertyNode{StrokeLineJoin}) = "slj"

# LineWidth
# ---------

immutable LineWidth <: Property
    value::Measure

    function LineWidth(value)
        return new(size_measure(value))
    end
end


function linewidth(value::@compat(Union{Measure, Number}))
    return LineWidth(value)
end


function linewidth(values::AbstractArray)
    return [LineWidth(value) for value in values]
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 primitive::LineWidth)
    return LineWidth(resolve(box, units, t, primitive.value))
end

prop_string(::PropertyNode{LineWidth}) = "lw"

# Visible
# -------

immutable Visible <: Property
    value::Bool
end

function visible(value::Bool)
    return Visible(value)
end


function visible(values::AbstractArray)
    return [Visible(value) for value in values]
end

prop_string(::PropertyNode{Visible}) = "v"


# FillOpacity
# -----------

immutable FillOpacity <: Property
    value::Float64

    function FillOpacity(value_::Number)
        value = @compat Float64(value_)
        if value < 0.0 || value > 1.0
            error("Opacity must be between 0 and 1.")
        end
        return new(value)
    end
end

function fillopacity(value::Float64)
    return FillOpacity(value)
end


function fillopacity(values::AbstractArray)
    return [FillOpacity(value) for value in values]
end

prop_string(::PropertyNode{FillOpacity}) = "fo"


# StrokeOpacity
# -------------

immutable StrokeOpacity <: Property
    value::Float64

    function StrokeOpacity(value_::Number)
        value = @compat Float64(value_)
        if value < 0.0 || value > 1.0
            error("Opacity must be between 0 and 1.")
        end
        return new(value)
    end
end


function strokeopacity(value::Float64)
    return StrokeOpacity(value)
end


function strokeopacity(values::AbstractArray)
    return [StrokeOpacity(value) for value in values]
end

prop_string(::PropertyNode{StrokeOpacity}) = "so"

# Clip
# ----

immutable Clip{P <: Vec} <: Property
    points::Vector{P}
end


function clip()
    return Clip(Array(Vec, 0))
end


function clip{T <: XYTupleOrVec}(points::AbstractArray{T})
    XM, YM = narrow_polygon_point_types(Vector[points])
    if XM == Any
        XM = Length{:cx, Float64}
    end
    if YM == Any
        YM = Length{:cy, Float64}
    end
    VecType = Tuple{XM, YM}

    return [Clip(VecType[(x_measure(point[1]), y_measure(point[2])) for point in points])]
end


function clip(point_arrays::AbstractArray...)
    XM, YM = narrow_polygon_point_types(point_arrays)
    VecType = XM == YM == Any ? Vec : Vec{XM, YM}
    PrimType = XM == YM == Any ? Clip : Clip{VecType}

    clipprims = Array(PrimType, length(point_arrays))
    for (i, point_array) in enumerate(point_arrays)
        clipprims[i] = Clip(VecType[(x_measure(point[1]), y_measure(point[2]))
                                             for point in point_array])
    end
    return clipprims
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 primitive::Clip)
    return Clip{AbsoluteVec2}(
        AbsoluteVec2[resolve(box, units, t, point) for point in primitive.points])
end

prop_string(::PropertyNode{Clip}) = "clp"

# Font
# ----

immutable Font <: Property
    family::AbstractString
end


function font(family::AbstractString)
    return Font(family)
end


function font(families::AbstractArray)
    return [Font(family) for family in families]
end

prop_string(::PropertyNode{Font}) = "fnt"

function Base.hash(primitive::Font, h::UInt64)
    return hash(primitive.family, h)
end


function Base.(:(==))(a::Font, b::Font)
    return a.family == b.family
end


# FontSize
# --------

immutable FontSize <: Property
    value::Measure

    function FontSize(value)
        return new(size_measure(value))
    end
end


function fontsize(value::@compat(Union{Number, Measure}))
    return FontSize(value)
end


function fontsize(values::AbstractArray)
    return [FontSize(value) for value in values]
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 primitive::FontSize)
    return FontSize(resolve(box, units, t, primitive.value))
end

prop_string(::PropertyNode{FontSize}) = "fsz"

# SVGID
# -----

immutable SVGID <: Property
    value::AbstractString
end


function svgid(value::AbstractString)
    return SVGID(value)
end


function svgid(values::AbstractArray)
    return [SVGID(value) for value in values]
end

prop_string(::PropertyNode{SVGID}) = "svgid"


function Base.hash(primitive::SVGID, h::UInt64)
    return hash(primitive.value, h)
end


function Base.(:(==))(a::SVGID, b::SVGID)
    return a.value == b.value
end


# SVGClass
# --------

immutable SVGClass <: Property
    value::ASCIIString
end


function svgclass(value::AbstractString)
    return SVGClass(value)
end


function svgclass(values::AbstractArray)
    return [SVGClass(value) for value in values]
end

function prop_string(svgc::PropertyNode{SVGClass})
    if isscalar(svgc)
        return string("svgc(", svgc.value, ")")
    else
        return string("svgc(", svgc.value, "...)")
    end
end

function Base.hash(primitive::SVGClass, h::UInt64)
    return hash(primitive.value, h)
end


function Base.(:(==))(a::SVGClass, b::SVGClass)
    return a.value == b.value
end


# SVGAttribute
# ------------

immutable SVGAttribute <: Property
    attribute::ASCIIString
    value::ASCIIString
end


function svgattribute(attribute::AbstractString, value)
    return [SVGAttribute(attribute, string(value))]
end


function svgattribute(attribute::AbstractString, values::AbstractArray)
    return [SVGAttribute(attribute, string(value))
             for value in values]
end


function svgattribute(attributes::AbstractArray, values::AbstractArray)
    return @makeprimitives SVGAttribute,
            (attribute in attributes, value in values),
            SVGAttribute(attribute, string(value))
end

prop_string(::PropertyNode{SVGAttribute}) = "svga"

function Base.hash(primitive::SVGAttribute, h::UInt64)
    h = hash(primitive.attribute, h)
    h = hash(primitive.value, h)
    return h
end


function Base.(:(==))(a::SVGAttribute, b::SVGAttribute)
    return a.attribute == b.attribute && a.value == b.value
end


# JSInclude
# ---------

immutable JSInclude <: Property
    value::AbstractString
    jsmodule::@compat(Union{(@compat Void), @compat Tuple{AbstractString, AbstractString}})
end


function jsinclude(value::AbstractString, module_name=nothing)
    return JSInclude(value, module_name)
end

# Don't bother with a vectorized version of this. It wouldn't really make #
# sense.

prop_string(::PropertyNode{JSInclude}) = "jsip"

# JSCall
# ------

immutable JSCall <: Property
    code::AbstractString
    args::Vector{Measure}
end


function jscall(code::AbstractString, arg::Vector{Measure}=Measure[])
    return JSCall(code, arg)
end


function jscall(codes::AbstractArray,
                args::AbstractArray{Vector{Measure}}=Vector{Measure}[Measure[]])
    return @makeprimitives JSCall,
            (code in codes, arg in args),
            JSCall(code, arg)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 primitive::JSCall)
    # we are going to build a new string by scanning across "code" and
    # replacing %x with translated x values, %y with translated y values
    # and %s with translated size values.
    newcode = IOBuffer()

    i = 1
    validx = 1
    while true
        j = search(primitive.code, '%', i)

        if j == 0
            write(newcode, primitive.code[i:end])
            break
        end

        write(newcode, primitive.code[i:j-1])
        if j == length(primitive.code)
            write(newcode, '%')
            break
        elseif primitive.code[j+1] == '%'
            write(newcode, '%')
        elseif primitive.code[j+1] == 'x'
            val = resolve(box, units, t, (primitive.args[validx], 0mm))
            write(newcode, svg_fmt_float(val[1].value))
            validx += 1
        elseif primitive.code[j+1] == 'y'
            val = resolve(box, units, t, (0mm, primitive.args[validx]))
            write(newcode, svg_fmt_float(val[2].value))
            validx += 1
        elseif primitive.code[j+1] == 's'
            val = resolve(box, units, t, primitive.args[validx])
            write(newcode, svg_fmt_float(val.value))
            validx += 1
        else
            write(newcode, '%', primitive.code[j+1])
        end

        i = j + 2
    end

    return takebuf_string(newcode), Measure[]
end


function isrepeatable(p::PropertyNode{JSCall})
    return true
end


function Base.isless(a::Fill, b::Fill)
    return color_isless(a.color, b.color)
end

function Base.isless(a::Stroke, b::Stroke)
    return color_isless(a.color, b.color)
end


prop_string(::PropertyNode{JSCall}) = "jsc"
