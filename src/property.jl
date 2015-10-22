
abstract PropertyPrimitive <: Primitive
typealias PropertyNode{T<:PropertyPrimitive} Union{T,AbstractArray{T}}

# Meaningless isless function used to sort in optimize_batching
function Base.isless{T <: PropertyPrimitive}(a::T, b::T)
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
function isrepeatable{P<:PropertyPrimitive}(p::Type{P})
    return false
end


# Property primitive catchall: most properties don't need measure transforms
function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 primitive::PropertyPrimitive)
    return primitive
end


# Stroke
# ------

immutable StrokePrimitive <: PropertyPrimitive
	color::RGBA{Float64}
end


function stroke(c::(@compat Void))
    return StrokePrimitive(RGBA{Float64}(0, 0, 0, 0))
end


function stroke(c::@compat(Union{Colorant, AbstractString}))
	return StrokePrimitive(parse_colorant(c))
end


function stroke(cs::AbstractArray)
	return [StrokePrimitive(c == nothing ? RGBA{Float64}(0, 0, 0, 0) : parse_colorant(c)) for c in cs]
end

prop_string(::Type{StrokePrimitive}) = "s"

# Fill
# ----

immutable FillPrimitive <: PropertyPrimitive
	color::RGBA{Float64}
end


function fill(c::(@compat Void))
    return FillPrimitive(RGBA{Float64}(0.0, 0.0, 0.0, 0.0))
end


function fill(c::@compat(Union{Colorant, AbstractString}))
	return FillPrimitive(parse_colorant(c))
end


function fill(cs::AbstractArray)
	return [FillPrimitive(c == nothing ? RGBA{Float64}(0.0, 0.0, 0.0, 0.0) : parse_colorant(c)) for c in cs]
end

prop_string(::Type{FillPrimitive}) = "f"


# StrokeDash
# ----------

immutable StrokeDashPrimitive <: PropertyPrimitive
    value::Vector{Measure}
end


function strokedash(values::AbstractArray)
    return StrokeDashPrimitive(values)
end


function strokedash(values::AbstractArray{AbstractArray})
    return StrokeDashPrimitive[StrokeDashPrimitive(value) for value in values]
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 primitive::StrokeDashPrimitive)
    StrokeDashPrimitive([resolve(box, units, t, v)
                    for v in primitive.value])
end

prop_string(::Type{StrokeDashPrimitive}) = "sd"

# StrokeLineCap
# -------------


abstract LineCap
immutable LineCapButt <: LineCap end
immutable LineCapSquare <: LineCap end
immutable LineCapRound <: LineCap end


immutable StrokeLineCapPrimitive <: PropertyPrimitive
    value::LineCap

    function StrokeLineCapPrimitive(value::LineCap)
        return new(value)
    end

    function StrokeLineCapPrimitive(value::Type{LineCap})
        return new(value())
    end
end


function strokelinecap(value::@compat(Union{LineCap, Type{LineCap}}))
    return StrokeLineCapPrimitive(value)
end


function strokelinecap(values::AbstractArray)
    return [StrokeLineCapPrimitive(value) for value in values]
end

prop_string(::Type{StrokeLineCapPrimitive}) = "slc"

# StrokeLineJoin
# --------------

abstract LineJoin
immutable LineJoinMiter <: LineJoin end
immutable LineJoinRound <: LineJoin end
immutable LineJoinBevel <: LineJoin end


immutable StrokeLineJoinPrimitive <: PropertyPrimitive
    value::LineJoin

    function StrokeLineJoinPrimitive(value::LineJoin)
        return new(value)
    end

    function StrokeLineCapPrimitive(value::Type{LineJoin})
        return new(value())
    end
end


function strokelinejoin(value::@compat(Union{LineJoin, Type{LineJoin}}))
    return StrokeLineJoinPrimitive(value)
end


function strokelinejoin(values::AbstractArray)
    return [StrokeLineJoinPrimitive(value) for value in values]
end

prop_string(::Type{StrokeLineJoinPrimitive}) = "slj"

# LineWidth
# ---------

immutable LineWidthPrimitive <: PropertyPrimitive
    value::Measure

    function LineWidthPrimitive(value)
        return new(size_measure(value))
    end
end


function linewidth(value::@compat(Union{Measure, Number}))
    return LineWidthPrimitive(value)
end


function linewidth(values::AbstractArray)
    return [LineWidthPrimitive(value) for value in values]
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 primitive::LineWidthPrimitive)
    return LineWidthPrimitive(resolve(box, units, t, primitive.value))
end

prop_string(::Type{LineWidthPrimitive}) = "lw"

# Visible
# -------

immutable VisiblePrimitive <: PropertyPrimitive
    value::Bool
end

function visible(value::Bool)
    return VisiblePrimitive(value)
end


function visible(values::AbstractArray)
    return [VisiblePrimitive(value) for value in values]
end

prop_string(::Type{VisiblePrimitive}) = "v"


# FillOpacity
# -----------

immutable FillOpacityPrimitive <: PropertyPrimitive
    value::Float64

    function FillOpacityPrimitive(value_::Number)
        value = @compat Float64(value_)
        if value < 0.0 || value > 1.0
            error("Opacity must be between 0 and 1.")
        end
        return new(value)
    end
end

function fillopacity(value::Float64)
    return FillOpacityPrimitive(value)
end


function fillopacity(values::AbstractArray)
    return [FillOpacityPrimitive(value) for value in values]
end

prop_string(::Type{FillOpacityPrimitive}) = "fo"


# StrokeOpacity
# -------------

immutable StrokeOpacityPrimitive <: PropertyPrimitive
    value::Float64

    function StrokeOpacityPrimitive(value_::Number)
        value = @compat Float64(value_)
        if value < 0.0 || value > 1.0
            error("Opacity must be between 0 and 1.")
        end
        return new(value)
    end
end


function strokeopacity(value::Float64)
    return StrokeOpacityPrimitive(value)
end


function strokeopacity(values::AbstractArray)
    return [StrokeOpacityPrimitive(value) for value in values]
end

prop_string(::Type{StrokeOpacityPrimitive}) = "so"

# Clip
# ----

immutable ClipPrimitive{P <: Vec} <: PropertyPrimitive
    points::Vector{P}
end


function clip()
    return ClipPrimitive(Array(Vec, 0))
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

    return [ClipPrimitive(VecType[(x_measure(point[1]), y_measure(point[2])) for point in points])]
end


function clip(point_arrays::AbstractArray...)
    XM, YM = narrow_polygon_point_types(point_arrays)
    VecType = XM == YM == Any ? Vec : Vec{XM, YM}
    PrimType = XM == YM == Any ? ClipPrimitive : ClipPrimitive{VecType}

    clipprims = Array(PrimType, length(point_arrays))
    for (i, point_array) in enumerate(point_arrays)
        clipprims[i] = ClipPrimitive(VecType[(x_measure(point[1]), y_measure(point[2]))
                                             for point in point_array])
    end
    return clipprims
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 primitive::ClipPrimitive)
    return ClipPrimitive{AbsoluteVec2}(
        AbsoluteVec2[resolve(box, units, t, point) for point in primitive.points])
end

prop_string(::Type{ClipPrimitive}) = "clp"

# Font
# ----

immutable FontPrimitive <: PropertyPrimitive
    family::AbstractString
end


function font(family::AbstractString)
    return FontPrimitive(family)
end


function font(families::AbstractArray)
    return [FontPrimitive(family) for family in families]
end

prop_string(::Type{FontPrimitive}) = "fnt"

function Base.hash(primitive::FontPrimitive, h::UInt64)
    return hash(primitive.family, h)
end


function Base.(:(==))(a::FontPrimitive, b::FontPrimitive)
    return a.family == b.family
end


# FontSize
# --------

immutable FontSizePrimitive <: PropertyPrimitive
    value::Measure

    function FontSizePrimitive(value)
        return new(size_measure(value))
    end
end


function fontsize(value::@compat(Union{Number, Measure}))
    return FontSizePrimitive(value)
end


function fontsize(values::AbstractArray)
    return [FontSizePrimitive(value) for value in values]
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 primitive::FontSizePrimitive)
    return FontSizePrimitive(resolve(box, units, t, primitive.value))
end

prop_string(::Type{FontSizePrimitive}) = "fsz"

# SVGID
# -----

immutable SVGIDPrimitive <: PropertyPrimitive
    value::AbstractString
end


function svgid(value::AbstractString)
    return SVGIDPrimitive(value)
end


function svgid(values::AbstractArray)
    return [SVGIDPrimitive(value) for value in values]
end

prop_string(::Type{SVGIDPrimitive}) = "svgid"


function Base.hash(primitive::SVGIDPrimitive, h::UInt64)
    return hash(primitive.value, h)
end


function Base.(:(==))(a::SVGIDPrimitive, b::SVGIDPrimitive)
    return a.value == b.value
end


# SVGClass
# --------

immutable SVGClassPrimitive <: PropertyPrimitive
    value::ASCIIString
end


function svgclass(value::AbstractString)
    return SVGClassPrimitive(value)
end


function svgclass(values::AbstractArray)
    return [SVGClassPrimitive(value) for value in values]
end

function prop_string(svgc::Type{SVGClassPrimitive})
    if isscalar(svgc)
        return string("svgc(", svgc.primitives[1].value, ")")
    else
        return string("svgc(", svgc.primitives[1].value, "...)")
    end
end

function Base.hash(primitive::SVGClassPrimitive, h::UInt64)
    return hash(primitive.value, h)
end


function Base.(:(==))(a::SVGClassPrimitive, b::SVGClassPrimitive)
    return a.value == b.value
end


# SVGAttribute
# ------------

immutable SVGAttributePrimitive <: PropertyPrimitive
    attribute::ASCIIString
    value::ASCIIString
end


function svgattribute(attribute::AbstractString, value)
    return [SVGAttributePrimitive(attribute, string(value))]
end


function svgattribute(attribute::AbstractString, values::AbstractArray)
    return [SVGAttributePrimitive(attribute, string(value))
             for value in values]
end


function svgattribute(attributes::AbstractArray, values::AbstractArray)
    return @makeprimitives SVGAttributePrimitive,
            (attribute in attributes, value in values),
            SVGAttributePrimitive(attribute, string(value))
end

prop_string(::Type{SVGAttributePrimitive}) = "svga"

function Base.hash(primitive::SVGAttributePrimitive, h::UInt64)
    h = hash(primitive.attribute, h)
    h = hash(primitive.value, h)
    return h
end


function Base.(:(==))(a::SVGAttributePrimitive, b::SVGAttributePrimitive)
    return a.attribute == b.attribute && a.value == b.value
end


# JSInclude
# ---------

immutable JSIncludePrimitive <: PropertyPrimitive
    value::AbstractString
    jsmodule::@compat(Union{(@compat Void), @compat Tuple{AbstractString, AbstractString}})
end


function jsinclude(value::AbstractString, module_name=nothing)
    return JSIncludePrimitive(value, module_name)
end

# Don't bother with a vectorized version of this. It wouldn't really make #
# sense.

prop_string(::Type{JSIncludePrimitive}) = "jsip"

# JSCall
# ------

immutable JSCallPrimitive <: PropertyPrimitive
    code::AbstractString
    args::Vector{Measure}
end


function jscall(code::AbstractString, arg::Vector{Measure}=Measure[])
    return JSCallPrimitive(code, arg)
end


function jscall(codes::AbstractArray,
                args::AbstractArray{Vector{Measure}}=Vector{Measure}[Measure[]])
    return @makeprimitives JSCallPrimitive,
            (code in codes, arg in args),
            JSCallPrimitive(code, arg)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 primitive::JSCallPrimitive)
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


function isrepeatable(p::Type{JSCallPrimitive})
    return true
end


function Base.isless(a::FillPrimitive, b::FillPrimitive)
    return color_isless(a.color, b.color)
end

function Base.isless(a::StrokePrimitive, b::StrokePrimitive)
    return color_isless(a.color, b.color)
end


prop_string(::Type{JSCallPrimitive}) = "jsc"
