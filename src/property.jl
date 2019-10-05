abstract type PropertyPrimitive end

# Meaningless isless function used to sort in optimize_batching
function Base.isless(a::T, b::T) where T <: PropertyPrimitive
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


struct Property{P <: PropertyPrimitive} <: ComposeNode
    primitives::Vector{P}
end

isempty(p::Property) = isempty(p.primitives)

isscalar(p::Property) = length(p.primitives) == 1


# Some properties can be applied multiple times, most cannot.
isrepeatable(p::Property) = false

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::Property{T}) where T =
        Property{T}([resolve(box, units, t, primitive) for primitive in p.primitives])

# Property primitive catchall: most properties don't need measure transforms
resolve(box::AbsoluteBox, units::UnitBox, t::Transform, primitive::PropertyPrimitive) = primitive


# Stroke
# ------

struct StrokePrimitive <: PropertyPrimitive
	color::RGBA{Float64}
end

const Stroke = Property{StrokePrimitive}

stroke(c::Nothing) = Stroke([StrokePrimitive(RGBA{Float64}(0, 0, 0, 0))])
stroke(c::Union{Colorant, AbstractString}) = Stroke([StrokePrimitive(parse_colorant(c))])
stroke(cs::AbstractArray) = Stroke([StrokePrimitive(c == nothing ?
        RGBA{Float64}(0, 0, 0, 0) : parse_colorant(c)) for c in cs])

prop_string(::Stroke) = "s"


# Fill
# ----

struct FillPrimitive <: PropertyPrimitive
	color::RGBA{Float64}
end

const Fill = Property{FillPrimitive}

fill(c::Nothing) = Fill([FillPrimitive(RGBA{Float64}(0.0, 0.0, 0.0, 0.0))])

"""
    fill(c)

Define a fill color, where `c` can be a `Colorant` or `String`.
"""
fill(c::Union{Colorant, AbstractString}) = Fill([FillPrimitive(parse_colorant(c))])

"""
    fill(cs::AbstractArray)

Arguments can be passed in arrays in order to perform multiple drawing operations at once.
"""
fill(cs::AbstractArray) = Fill([FillPrimitive(c == nothing ?
        RGBA{Float64}(0.0, 0.0, 0.0, 0.0) : parse_colorant(c)) for c in cs])

prop_string(::Fill) = "f"


# StrokeDash
# ----------

struct StrokeDashPrimitive <: PropertyPrimitive
    value::Vector{Measure}
end

const StrokeDash = Property{StrokeDashPrimitive}

strokedash(values::AbstractArray) = StrokeDash([StrokeDashPrimitive(collect(Measure, values))])
strokedash(values::AbstractArray{<:AbstractArray}) =
        StrokeDash([StrokeDashPrimitive(collect(Measure, value)) for value in values])

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, primitive::StrokeDashPrimitive) =
        StrokeDashPrimitive([resolve(box, units, t, v) for v in primitive.value])

prop_string(::StrokeDash) = "sd"


# StrokeLineCap
# -------------

abstract type LineCap end
struct LineCapButt <: LineCap end
struct LineCapSquare <: LineCap end
struct LineCapRound <: LineCap end

struct StrokeLineCapPrimitive <: PropertyPrimitive
    value::LineCap
end

StrokeLineCapPrimitive(value::Type{LineCap}) = StrokeLineCapPrimitive(value())

const StrokeLineCap = Property{StrokeLineCapPrimitive}

strokelinecap(value::Union{LineCap, Type{LineCap}}) =
        StrokeLineCap([StrokeLineCapPrimitive(value)])
strokelinecap(values::AbstractArray) =
        StrokeLineCap([StrokeLineCapPrimitive(value) for value in values])

prop_string(::StrokeLineCap) = "slc"


# StrokeLineJoin
# --------------

abstract type LineJoin end
struct LineJoinMiter <: LineJoin end
struct LineJoinRound <: LineJoin end
struct LineJoinBevel <: LineJoin end

struct StrokeLineJoinPrimitive <: PropertyPrimitive
    value::LineJoin
end

StrokeLineCapPrimitive(value::Type{LineJoin}) = new(value())

const StrokeLineJoin = Property{StrokeLineJoinPrimitive}

strokelinejoin(value::Union{LineJoin, Type{LineJoin}}) =
        StrokeLineJoin([StrokeLineJoinPrimitive(value)])
strokelinejoin(values::AbstractArray) =
        StrokeLineJoin([StrokeLineJoinPrimitive(value) for value in values])

prop_string(::StrokeLineJoin) = "slj"


# LineWidth
# ---------

struct LineWidthPrimitive <: PropertyPrimitive
    value::Measure

    function LineWidthPrimitive(value)
        return new(size_measure(value))
    end
end

const LineWidth = Property{LineWidthPrimitive}

linewidth(value::Union{Measure, Number}) = LineWidth([LineWidthPrimitive(value)])
linewidth(values::AbstractArray) =
        LineWidth([LineWidthPrimitive(value) for value in values])

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, primitive::LineWidthPrimitive) =
        LineWidthPrimitive(resolve(box, units, t, primitive.value))

prop_string(::LineWidth) = "lw"


# Visible
# -------

struct VisiblePrimitive <: PropertyPrimitive
    value::Bool
end

const Visible = Property{VisiblePrimitive}

visible(value::Bool) = Visible([VisiblePrimitive(value)])
visible(values::AbstractArray) = Visible([VisiblePrimitive(value) for value in values])

prop_string(::Visible) = "v"


# FillOpacity
# -----------

struct FillOpacityPrimitive <: PropertyPrimitive
    value::Float64

    function FillOpacityPrimitive(value_::Number)
        value = Float64(value_)
        (value < 0.0 || value > 1.0) && error("Opacity must be between 0 and 1.")
        return new(value)
    end
end

const FillOpacity = Property{FillOpacityPrimitive}

"""
    fillopacity(value)

Define a fill opacity, where 0≤value≤1.  For svg, nested contexts  will inherit from parent contexts e.g. `(context(), fillopacity(a), (context(), fill(c::String), circle()))`.
"""
fillopacity(value::Float64) = FillOpacity([FillOpacityPrimitive(value)])

"""
    fillopacity(values::AbstractArray)

Arguments can be passed in arrays in order to perform multiple drawing operations at once.
"""
fillopacity(values::AbstractArray) =
        FillOpacity([FillOpacityPrimitive(value) for value in values])

prop_string(::FillOpacity) = "fo"


# StrokeOpacity
# -------------

struct StrokeOpacityPrimitive <: PropertyPrimitive
    value::Float64

    function StrokeOpacityPrimitive(value_::Number)
        value = Float64(value_)
        (value < 0.0 || value > 1.0) && error("Opacity must be between 0 and 1.")
        return new(value)
    end
end

const StrokeOpacity = Property{StrokeOpacityPrimitive}

strokeopacity(value::Float64) = StrokeOpacity([StrokeOpacityPrimitive(value)])
strokeopacity(values::AbstractArray) =
        StrokeOpacity([StrokeOpacityPrimitive(value) for value in values])

prop_string(::StrokeOpacity) = "so"


# Clip
# ----

struct ClipPrimitive{P <: Vec} <: PropertyPrimitive
    points::Vector{P}
end

const Clip = Property{ClipPrimitive}

clip() = Clip([ClipPrimitive(Array{Vec}(undef, 0))])


"""
    clip(points::AbstractArray)

`clip()` is a property.  Only forms inside the clip shape will be visible.
"""
function clip(points::AbstractArray{T}) where T <: XYTupleOrVec
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


"""
    clip(point_arrays::AbstractArray...)

Arguments can be passed in arrays in order to perform multiple clipping operations at once.
"""
function clip(point_arrays::AbstractArray...)
    XM, YM = narrow_polygon_point_types(point_arrays)
    VecType = XM == YM == Any ? Vec : Vec{XM, YM}
    PrimType = XM == YM == Any ? ClipPrimitive : ClipPrimitive{VecType}

    clipprims = Array{PrimType}(undef, length(point_arrays))
    for (i, point_array) in enumerate(point_arrays)
        clipprims[i] = ClipPrimitive(VecType[(x_measure(point[1]), y_measure(point[2]))
                                             for point in point_array])
    end
    return Property{PrimType}(clipprims)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, primitive::ClipPrimitive) =
        ClipPrimitive{AbsoluteVec2}( AbsoluteVec2[
            resolve(box, units, t, point) for point in primitive.points])

prop_string(::Clip) = "clp"


# Font
# ----

struct FontPrimitive <: PropertyPrimitive
    family::AbstractString
end

const Font = Property{FontPrimitive}

font(family::AbstractString) = Font([FontPrimitive(family)])
font(families::AbstractArray) =
        Font([FontPrimitive(family) for family in families])

prop_string(::Font) = "fnt"

Base.hash(primitive::FontPrimitive, h::UInt) = hash(primitive.family, h)

==(a::FontPrimitive, b::FontPrimitive) = a.family == b.family


# FontSize
# --------

struct FontSizePrimitive <: PropertyPrimitive
    value::Measure

    function FontSizePrimitive(value)
        return new(size_measure(value))
    end
end

const FontSize = Property{FontSizePrimitive}

fontsize(value::Union{Number, Measure}) = FontSize([FontSizePrimitive(value)])

fontsize(values::AbstractArray) = FontSize([FontSizePrimitive(value) for value in values])

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, primitive::FontSizePrimitive) =
        FontSizePrimitive(resolve(box, units, t, primitive.value))

prop_string(::FontSize) = "fsz"


# SVGID
# -----

struct SVGIDPrimitive <: PropertyPrimitive
    value::AbstractString
end

const SVGID = Property{SVGIDPrimitive}

svgid(value::AbstractString) = SVGID([SVGIDPrimitive(value)])
svgid(values::AbstractArray) = SVGID([SVGIDPrimitive(value) for value in values])

prop_string(::SVGID) = "svgid"

Base.hash(primitive::SVGIDPrimitive, h::UInt) = hash(primitive.value, h)

==(a::SVGIDPrimitive, b::SVGIDPrimitive) = a.value == b.value


# SVGClass
# --------

struct SVGClassPrimitive <: PropertyPrimitive
    value::String
end

const SVGClass = Property{SVGClassPrimitive}

svgclass(value::AbstractString) = SVGClass([SVGClassPrimitive(value)])
svgclass(values::AbstractArray) =
        SVGClass([SVGClassPrimitive(value) for value in values])

function prop_string(svgc::SVGClass)
    if isscalar(svgc)
        return string("svgc(", svgc.primitives[1].value, ")")
    else
        return string("svgc(", svgc.primitives[1].value, "...)")
    end
end

Base.hash(primitive::SVGClassPrimitive, h::UInt) = hash(primitive.value, h)

==(a::SVGClassPrimitive, b::SVGClassPrimitive) = a.value == b.value


# SVGAttribute
# ------------

struct SVGAttributePrimitive <: PropertyPrimitive
    attribute::String
    value::String
end

const SVGAttribute = Property{SVGAttributePrimitive}

svgattribute(attribute::AbstractString, value) =
        SVGAttribute([SVGAttributePrimitive(attribute, string(value))])
svgattribute(attribute::AbstractString, values::AbstractArray) =
        SVGAttribute([SVGAttributePrimitive(attribute, string(value)) for value in values])

svgattribute(attributes::AbstractArray, values::AbstractArray) =
    SVGAttribute( @makeprimitives SVGAttributePrimitive,
            (attribute in attributes, value in values),
            SVGAttributePrimitive(attribute, string(value)))

prop_string(::SVGAttribute) = "svga"

function Base.hash(primitive::SVGAttributePrimitive, h::UInt)
    h = hash(primitive.attribute, h)
    h = hash(primitive.value, h)
    return h
end

==(a::SVGAttributePrimitive, b::SVGAttributePrimitive) =
        a.attribute == b.attribute && a.value == b.value


# JSInclude
# ---------

struct JSIncludePrimitive <: PropertyPrimitive
    value::AbstractString
    jsmodule::Union{Nothing, Tuple{AbstractString, AbstractString}}
end

const JSInclude = Property{JSIncludePrimitive}

jsinclude(value::AbstractString, module_name=nothing) =
        JSInclude([JSIncludePrimitive(value, module_name)])

# Don't bother with a vectorized version of this. It wouldn't really make #
# sense.

prop_string(::JSInclude) = "jsip"

# JSCall
# ------

struct JSCallPrimitive <: PropertyPrimitive
    code::AbstractString
    args::Vector{Measure}
end

const JSCall = Property{JSCallPrimitive}

jscall(code::AbstractString, arg::Vector{Measure}=Measure[]) = JSCall([JSCallPrimitive(code, arg)])

jscall(codes::AbstractArray, args::AbstractArray{Vector{Measure}}=Vector{Measure}[Measure[]]) =
        JSCall( @makeprimitives JSCallPrimitive,
            (code in codes, arg in args),
            JSCallPrimitive(code, arg))

function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, primitive::JSCallPrimitive)
    # we are going to build a new string by scanning across "code" and
    # replacing %x with translated x values, %y with translated y values
    # and %s with translated size values.
    newcode = IOBuffer()

    i = 1
    validx = 1
    while true
        j = findnext(primitive.code, "%", i)

        if j === nothing
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

    return JSCallPrimitive(String(take!(newcode)), Measure[])
end

isrepeatable(p::JSCall) = true

Base.isless(a::FillPrimitive, b::FillPrimitive) = color_isless(a.color, b.color)
Base.isless(a::StrokePrimitive, b::StrokePrimitive) = color_isless(a.color, b.color)

prop_string(::JSCall) = "jsc"



# Arrow Property
# -------

struct ArrowPrimitive <: PropertyPrimitive
    value::Bool
end

const Arrow = Property{ArrowPrimitive}


"""
    arrow()

    `arrow() = arrow(true)`
"""
arrow() = Arrow([ArrowPrimitive(true)])


"""
    arrow(value::Bool)

    `arrow()` is a property of arcs, lines and curves. The color of the arrowhead is the same as `stroke()`, but for svg the results will be browser-dependent. 
"""
arrow(value::Bool) = Arrow([ArrowPrimitive(value)])

"""
    arrow(values::AbstractArray)

Arguments can be passed in arrays in order to perform multiple drawing operations at once.
"""
arrow(values::AbstractArray) = Arrow([ArrowPrimitive(value) for value in values])

prop_string(::Arrow) = "arrow"




