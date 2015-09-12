

abstract PropertyPrimitive


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


function absolute_units{T}(p::Property{T}, t::Transform, units::UnitBox,
                           box::AbsoluteBoundingBox)
    return Property{T}([absolute_units(primitive, t, units, box)
                        for primitive in p.primitives])
end


# Property primitive catchall: most properties don't need measure transforms
function absolute_units{T <: PropertyPrimitive}(primitive::T, t::Transform,
                                                units::UnitBox,
                                                box::AbsoluteBoundingBox)
    return primitive
end


# Stroke
# ------

immutable StrokePrimitive <: PropertyPrimitive
	color::RGBA{Float64}
end

typealias Stroke Property{StrokePrimitive}


function stroke(c::Nothing)
    return Stroke([StrokePrimitive(RGBA{Float64}(0, 0, 0, 0))])
end


function stroke(c::Union(Colorant, String))
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


function fill(c::Nothing)
    return Fill([FillPrimitive(RGBA{Float64}(0.0, 0.0, 0.0, 0.0))])
end


function fill(c::Union(Colorant, String))
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


function absolute_units(primitive::StrokeDashPrimitive, t::Transform,
                        units::UnitBox, box::AbsoluteBoundingBox)
    return StrokeDashPrimitive([Measure(absolute_units(v, t, units, box))
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


function strokelinecap(value::Union(LineCap, Type{LineCap}))
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


function strokelinejoin(value::Union(LineJoin, Type{LineJoin}))
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


function linewidth(value::Union(Measure, Number))
    return LineWidth([LineWidthPrimitive(value)])
end


function linewidth(values::AbstractArray)
    return LineWidth([LineWidthPrimitive(value) for value in values])
end


function absolute_units(primitive::LineWidthPrimitive, t::Transform,
                        units::UnitBox, box::AbsoluteBoundingBox)
    return LineWidthPrimitive(Measure(absolute_units(primitive.value, t, units, box)))
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

immutable ClipPrimitive{P <: Point} <: PropertyPrimitive
    points::Vector{P}
end

typealias Clip Property{ClipPrimitive}


function clip()
    return Clip([ClipPrimitive(Array(Point, 0))])
end


function clip(points::XYTupleOrPoint...)
    return Clip([ClipPrimitive([convert(Point, point) for point in points])])
end


function clip(point_arrays::AbstractArray...)
    clipprims = Array(ClipPrimitive, length(point_arrays))
    for (i, point_array) in enumerate(point_arrays)
        clipprims[i] = ClipPrimitive([convert(Point, point)
                                      for point in point_array])
    end
    return Clip(clipprims)
end


function absolute_units(primitive::ClipPrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return ClipPrimitive(SimplePoint[absolute_units(point, t, units, box)
                                     for point in primitive.points])
end

prop_string(::Clip) = "clp"

# Font
# ----

immutable FontPrimitive <: PropertyPrimitive
    family::String
end

typealias Font Property{FontPrimitive}


function font(family::String)
    return Font([FontPrimitive(family)])
end


function font(families::AbstractArray)
    return Font([FontPrimitive(family) for family in families])
end

prop_string(::Font) = "fnt"

# FontSize
# --------

immutable FontSizePrimitive <: PropertyPrimitive
    value::Measure

    function FontSizePrimitive(value)
        return new(size_measure(value))
    end
end

typealias FontSize Property{FontSizePrimitive}


function fontsize(value::Union(Number, Measure))
    return FontSize([FontSizePrimitive(value)])
end


function fontsize(values::AbstractArray)
    return FontSize([FontSizePrimitive(value) for value in values])
end


function absolue_units(primitive::FontSizePrimitive, t::Transform,
                       units::UnitBox, box::AbsoluteBoundingBox)
    return FontSizePrimitive(Measure(absolute_units(primitive.value, t, units, box)))
end

prop_string(::FontSize) = "fsz"

# SVGID
# -----

immutable SVGIDPrimitive <: PropertyPrimitive
    value::String
end

typealias SVGID Property{SVGIDPrimitive}


function svgid(value::String)
    return SVGID([SVGIDPrimitive(value)])
end


function svgid(values::AbstractArray)
    return SVGID([SVGIDPrimitive(value) for value in values])
end

prop_string(::SVGID) = "svgid"


# SVGClass
# --------

immutable SVGClassPrimitive <: PropertyPrimitive
    value::String
end

typealias SVGClass Property{SVGClassPrimitive}


function svgclass(value::String)
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

# SVGAttribute
# ------------

immutable SVGAttributePrimitive <: PropertyPrimitive
    attribute::String
    value::String
end

typealias SVGAttribute Property{SVGAttributePrimitive}


function svgattribute(attribute::String, value)
    return SVGAttribute([SVGAttributePrimitive(attribute, string(value))])
end


function svgattribute(attribute::String, values::AbstractArray)
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

# JSInclude
# ---------

immutable JSIncludePrimitive <: PropertyPrimitive
    value::String
    jsmodule::Union(Nothing, @compat Tuple{String, String})
end

typealias JSInclude Property{JSIncludePrimitive}


function jsinclude(value::String, module_name=nothing)
    return JSInclude([JSIncludePrimitive(value, module_name)])
end

# Don't bother with a vectorized version of this. It wouldn't really make #
# sense.

prop_string(::JSInclude) = "jsip"

# JSCall
# ------

immutable JSCallPrimitive <: PropertyPrimitive
    code::String
    args::Vector{Measure}
end

typealias JSCall Property{JSCallPrimitive}


function jscall(code::String, arg::Vector{Measure}=Measure[])
    return JSCall([JSCallPrimitive(code, arg)])
end


function jscall(codes::AbstractArray,
                args::AbstractArray{Vector{Measure}}=Vector{Measure}[Measure[]])
    return JSCall(
        @makeprimitives JSCallPrimitive,
            (code in codes, arg in args),
            JSCallPrimitive(code, arg))
end


function absolute_units(primitive::JSCallPrimitive, t::Transform,
                        units::UnitBox, box::AbsoluteBoundingBox)
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
            val = absolute_x_position(primitive.args[validx], t, units, box)
            write(newcode, svg_fmt_float(val))
            validx += 1
        elseif primitive.code[j+1] == 'y'
            val = absolute_y_position(primitive.args[validx], t, units, box)
            write(newcode, svg_fmt_float(val))
            validx += 1
        elseif primitive.code[j+1] == 's'
            val = absolute_units(primitive.args[validx], t, units, box)
            write(newcode, svg_fmt_float(val.abs))
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

prop_string(::JSCall) = "jsc"
