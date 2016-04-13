

abstract PropertyPrimitive


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


immutable Property{P <: PropertyPrimitive} <: ComposeNode
	primitives::Vector{P}
end


function isempty(p::Property)
    return isempty(p.primitives)
end


function isscalar(p::Property)
    return length(p.primitives) == 1
end


# Some properties can be applied multiple times, most cannot.
function isrepeatable(p::Property)
    return false
end


function resolve{T}(box::AbsoluteBox, units::UnitBox, t::Transform, p::Property{T})
    return Property{T}([resolve(box, units, t, primitive) for primitive in p.primitives])
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

typealias Stroke Property{StrokePrimitive}


function stroke(c::(@compat Void))
    return Stroke([StrokePrimitive(RGBA{Float64}(0, 0, 0, 0))])
end


function stroke(c::@compat(Union{Colorant, AbstractString}))
	return Stroke([StrokePrimitive(parse_colorant(c))])
end


function stroke(cs::AbstractArray)
	return Stroke([StrokePrimitive(c == nothing ? RGBA{Float64}(0, 0, 0, 0) : parse_colorant(c)) for c in cs])
end

prop_string(::Stroke) = "s"

# Fill
# ----

immutable FillPrimitive <: PropertyPrimitive
	color::RGBA{Float64}
end

typealias Fill Property{FillPrimitive}


function fill(c::(@compat Void))
    return Fill([FillPrimitive(RGBA{Float64}(0.0, 0.0, 0.0, 0.0))])
end


function fill(c::@compat(Union{Colorant, AbstractString}))
	return Fill([FillPrimitive(parse_colorant(c))])
end


function fill(cs::AbstractArray)
	return Fill([FillPrimitive(c == nothing ? RGBA{Float64}(0.0, 0.0, 0.0, 0.0) : parse_colorant(c)) for c in cs])
end

prop_string(::Fill) = "f"


# StrokeDash
# ----------

immutable StrokeDashPrimitive <: PropertyPrimitive
    value::Vector{Measure}
end

typealias StrokeDash Property{StrokeDashPrimitive}


function strokedash(values::AbstractArray)
    return StrokeDash([StrokeDashPrimitive(collect(Measure, values))])
end


function strokedash(values::AbstractArray{AbstractArray})
    return StrokeDash([StrokeDashPrimitive(collect(Measure, value)) for value in values])
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 primitive::StrokeDashPrimitive)
    return StrokeDashPrimitive([resolve(box, units, t, v)
                                for v in primitive.value])
end

prop_string(::StrokeDash) = "sd"

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

typealias StrokeLineCap Property{StrokeLineCapPrimitive}


function strokelinecap(value::@compat(Union{LineCap, Type{LineCap}}))
    return StrokeLineCap([StrokeLineCapPrimitive(value)])
end


function strokelinecap(values::AbstractArray)
    return StrokeLineCap([StrokeLineCapPrimitive(value) for value in values])
end

prop_string(::StrokeLineCap) = "slc"

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

typealias StrokeLineJoin Property{StrokeLineJoinPrimitive}


function strokelinejoin(value::@compat(Union{LineJoin, Type{LineJoin}}))
    return StrokeLineJoin([StrokeLineJoinPrimitive(value)])
end


function strokelinejoin(values::AbstractArray)
    return StrokeLineJoin([StrokeLineJoinPrimitive(value) for value in values])
end

prop_string(::StrokeLineJoin) = "slj"

# LineWidth
# ---------

immutable LineWidthPrimitive <: PropertyPrimitive
    value::Measure

    function LineWidthPrimitive(value)
        return new(size_measure(value))
    end
end

typealias LineWidth Property{LineWidthPrimitive}


function linewidth(value::@compat(Union{Measure, Number}))
    return LineWidth([LineWidthPrimitive(value)])
end


function linewidth(values::AbstractArray)
    return LineWidth([LineWidthPrimitive(value) for value in values])
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 primitive::LineWidthPrimitive)
    return LineWidthPrimitive(resolve(box, units, t, primitive.value))
end

prop_string(::LineWidth) = "lw"

# Visible
# -------

immutable VisiblePrimitive <: PropertyPrimitive
    value::Bool
end

typealias Visible Property{VisiblePrimitive}


function visible(value::Bool)
    return Visible([VisiblePrimitive(value)])
end


function visible(values::AbstractArray)
    return Visible([VisiblePrimitive(value) for value in values])
end

prop_string(::Visible) = "v"


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

typealias FillOpacity Property{FillOpacityPrimitive}


function fillopacity(value::Float64)
    return FillOpacity([FillOpacityPrimitive(value)])
end


function fillopacity(values::AbstractArray)
    return FillOpacity([FillOpacityPrimitive(value) for value in values])
end

prop_string(::FillOpacity) = "fo"


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

typealias StrokeOpacity Property{StrokeOpacityPrimitive}


function strokeopacity(value::Float64)
    return StrokeOpacity([StrokeOpacityPrimitive(value)])
end


function strokeopacity(values::AbstractArray)
    return StrokeOpacity([StrokeOpacityPrimitive(value) for value in values])
end

prop_string(::StrokeOpacity) = "so"

# Clip
# ----

immutable ClipPrimitive{P <: Vec} <: PropertyPrimitive
    points::Vector{P}
end

typealias Clip Property{ClipPrimitive}


function clip()
    return Clip([ClipPrimitive(Array(Vec, 0))])
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

    prim = ClipPrimitive(VecType[(x_measure(point[1]), y_measure(point[2])) for point in points])
    return Clip(typeof(prim)[prim])
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
    return Property{PrimType}(clipprims)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 primitive::ClipPrimitive)
    return ClipPrimitive{AbsoluteVec2}(
        AbsoluteVec2[resolve(box, units, t, point) for point in primitive.points])
end

prop_string(::Clip) = "clp"

# Font
# ----

immutable FontPrimitive <: PropertyPrimitive
    family::AbstractString
end

typealias Font Property{FontPrimitive}


function font(family::AbstractString)
    return Font([FontPrimitive(family)])
end


function font(families::AbstractArray)
    return Font([FontPrimitive(family) for family in families])
end

prop_string(::Font) = "fnt"

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

typealias FontSize Property{FontSizePrimitive}


function fontsize(value::@compat(Union{Number, Measure}))
    return FontSize([FontSizePrimitive(value)])
end


function fontsize(values::AbstractArray)
    return FontSize([FontSizePrimitive(value) for value in values])
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 primitive::FontSizePrimitive)
    return FontSizePrimitive(resolve(box, units, t, primitive.value))
end

prop_string(::FontSize) = "fsz"

# SVGID
# -----

immutable SVGIDPrimitive <: PropertyPrimitive
    value::AbstractString
end

typealias SVGID Property{SVGIDPrimitive}


function svgid(value::AbstractString)
    return SVGID([SVGIDPrimitive(value)])
end


function svgid(values::AbstractArray)
    return SVGID([SVGIDPrimitive(value) for value in values])
end

prop_string(::SVGID) = "svgid"


function Base.hash(primitive::SVGIDPrimitive, h::UInt64)
    return hash(primitive.value, h)
end


function Base.(:(==))(a::SVGIDPrimitive, b::SVGIDPrimitive)
    return a.value == b.value
end


# SVGClass
# --------

immutable SVGClassPrimitive <: PropertyPrimitive
    value::AbstractString
end

typealias SVGClass Property{SVGClassPrimitive}


function svgclass(value::AbstractString)
    return SVGClass([SVGClassPrimitive(value)])
end


function svgclass(values::AbstractArray)
    return SVGClass([SVGClassPrimitive(value) for value in values])
end

function prop_string(svgc::SVGClass)
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

typealias SVGAttribute Property{SVGAttributePrimitive}


function svgattribute(attribute::AbstractString, value)
    return SVGAttribute([SVGAttributePrimitive(attribute, string(value))])
end


function svgattribute(attribute::AbstractString, values::AbstractArray)
    return SVGAttribute([SVGAttributePrimitive(attribute, string(value))
                         for value in values])
end


function svgattribute(attributes::AbstractArray, values::AbstractArray)
    return SVGAttribute(
        @makeprimitives SVGAttributePrimitive,
            (attribute in attributes, value in values),
            SVGAttributePrimitive(attribute, string(value)))
end

prop_string(::SVGAttribute) = "svga"

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

typealias JSInclude Property{JSIncludePrimitive}


function jsinclude(value::AbstractString, module_name=nothing)
    return JSInclude([JSIncludePrimitive(value, module_name)])
end

# Don't bother with a vectorized version of this. It wouldn't really make #
# sense.

prop_string(::JSInclude) = "jsip"

# JSCall
# ------

immutable JSCallPrimitive <: PropertyPrimitive
    code::AbstractString
    args::Vector{Measure}
end

typealias JSCall Property{JSCallPrimitive}


function jscall(code::AbstractString, arg::Vector{Measure}=Measure[])
    return JSCall([JSCallPrimitive(code, arg)])
end


function jscall(codes::AbstractArray,
                args::AbstractArray{Vector{Measure}}=Vector{Measure}[Measure[]])
    return JSCall(
        @makeprimitives JSCallPrimitive,
            (code in codes, arg in args),
            JSCallPrimitive(code, arg))
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

    return JSCallPrimitive(takebuf_string(newcode), Measure[])
end


function isrepeatable(p::JSCall)
    return true
end


function Base.isless(a::FillPrimitive, b::FillPrimitive)
    return color_isless(a.color, b.color)
end

function Base.isless(a::StrokePrimitive, b::StrokePrimitive)
    return color_isless(a.color, b.color)
end


prop_string(::JSCall) = "jsc"
