
# Property: a thing that changes how other things are drawn.

import Base.fill

export stroke, fill, linewidth, font, fontsize, visible, clip, opacity, svgid,
       svgclass, svglink, onactive, onclick, onfocusin, onfocusout, onload,
       onmousedown, onmousemove, onmouseout, onmouseover, onmouseup, svgmask,
       svgdefmask, svgembed, svgattribute, d3style, d3embed, d3hook, strokeopacity,
       strokedash, strokelinecap, strokelinejoin,
       LineCapButt, LineCapSquare, LineCapRound, LineJoinMiter, LineJoinRound, LineJoinBevel

# A property primitive is something can be directly applied.
abstract PropertyPrimitive


# A property is a (possibly empty) sequence of property primitives. The set of
# Properties with the combine operator forms a monoid (specifically, it can be
# thought of as a free monoid). The empty_property constant is the identity
# element.
abstract Property


# An enumeration, really.  Once (if?) Julia has better support for
# enums, we should use those
abstract LineCap
immutable LineCapButt <: LineCap end
immutable LineCapSquare <: LineCap end
immutable LineCapRound <: LineCap end

# This should also be an enum, ideally.
abstract LineJoin
immutable LineJoinMiter <: LineJoin end
immutable LineJoinRound <: LineJoin end
immutable LineJoinBevel <: LineJoin end


# An empty (i.e., nop) property which form the identity element of the Property
# monoid.
immutable EmptyProperty <: Property end
const empty_property = EmptyProperty()


copy(p::EmptyProperty) = p


# A non-empty sequence of property primitives.
type PropertySeq <: Property
    primitive::PropertyPrimitive
    next::Property

    function PropertySeq(primitive::PropertyPrimitive,
                         next::Property)
        new(primitive, next)
    end

    function PropertySeq(primitive::PropertyPrimitive)
        new(primitive, empty_property)
    end

    # shallow copy constructor
    function PropertySeq(ps::PropertySeq)
        new(ps.primitive, ps.next)
    end
end


copy(ps::PropertySeq) = PropertySeq(ps)


function length(p::Property)
    n = 0
    while !is(p, empty_property)
        n += 1
        p = p.next
    end
    n
end


# Combination of properties, which is simply list concatenation.
function combine(head::Property, rest::Property...)
    a = b = head === empty_property ? head : copy(head)
    for p in rest
        if p === empty_property
            continue
        elseif a === empty_property
            a = b = copy(p)
        else
            while !is(b.next, empty_property)
                b.next = copy(b.next)
                b = b.next
            end
            b.next = p
        end
    end
    a
end


combine(head::Property, ::Nothing) = head


# Unit conversion functions.

function absolute_units(property::EmptyProperty,
                        t::Transform,
                        unit_box::UnitBox,
                        box::AbsoluteBoundingBox)
    property
end


# Convert a sequence of properties to a sequence of properties in native
# coordinates.
function absolute_units(property::PropertySeq,
                        t::Transform,
                        unit_box::UnitBox,
                        box::AbsoluteBoundingBox)
    p = property = copy(property)
    while !is(p, empty_property)
        p.primitive = absolute_units(p.primitive, t, unit_box, box)
        p.next = copy(p.next)
        p = p.next
    end
    property
end


# Catchall for properties that don't require unit conversion.
function absolute_units(property::PropertyPrimitive,
                        t::Transform,
                        unit_box::UnitBox,
                        box::AbsoluteBoundingBox)
    property
end


# A property primitive controlling the fill color (or lack of color) of a form.
immutable Fill <: PropertyPrimitive
    value::ColorOrNothing

    Fill(value::ColorOrNothing) = new(value)
    Fill(value::String) = new(color(value))
end


# Singleton sequence contructor.
fill(value) = PropertySeq(Fill(value))


# A property primitive controlling the stroke color (or lack of color) of a form.
immutable Stroke <: PropertyPrimitive
    value::ColorOrNothing

    Stroke(value::ColorOrNothing) = new(value)
    Stroke(value::String) = new(color(value))
end


# Singleton sequence contructor.
stroke(value) = PropertySeq(Stroke(value))


# A property primitive controlling the stroke's dash pattern.
immutable StrokeDash <: PropertyPrimitive
    value::Array{Measure,1}

    StrokeDash(values) = new(map(size_measure, values))
end


# Singleton sequence constructor.
strokedash(values) = PropertySeq(StrokeDash(values))


# A property primitive controlling the stroke's linecap.
immutable StrokeLineCap <: PropertyPrimitive
    value::LineCap

    StrokeLineCap(value) = new(value)
end

# Singleton sequence constructor
strokelinecap(value) = PropertySeq(StrokeLineCap(value))


# A property primitive controlling the stroke's linejoin.
immutable StrokeLineJoin <: PropertyPrimitive
    value::LineJoin

    StrokeLineJoin(value) = new(value)
end

# Singleton sequence constructor
strokelinejoin(value) = PropertySeq(StrokeLineJoin(value))


# A property primitive controlling the widths of lines drawn in stroke
# operations.
immutable LineWidth <: PropertyPrimitive
    value::Measure

    LineWidth(value) = new(size_measure(value))
end


# Singleton sequence contructor.
linewidth(value) = PropertySeq(LineWidth(value))


# Unit conversion
function absolute_units(property::LineWidth,
                        t::Transform,
                        unit_box::UnitBox,
                        box::AbsoluteBoundingBox)
    LineWidth(absolute_units(property.value, t, unit_box, box))
end


# Set the visible attribute in SVG output, or just skip rendering in raster
# graphics.
immutable Visible <: PropertyPrimitive
    value::Bool
end


visible(value::Bool) = PropertySeq(Visible(value))


immutable Opacity <: PropertyPrimitive
    value::Float64
end


opacity(value::Number) = PropertySeq(Opacity(convert(Float64, value)))


immutable StrokeOpacity <: PropertyPrimitive
    value::Float64
end


strokeopacity(value::Number) = PropertySeq(StrokeOpacity(convert(Float64, value)))


# Clipping path
immutable Clip <: PropertyPrimitive
    points::Vector{Point}
end


function clip(points::XYTupleOrPoint...)
    PropertySeq(Clip([convert(Point, point) for point in points]))
end


function absolute_units(property::Clip,
                        t::Transform,
                        unit_box::UnitBox,
                        box::AbsoluteBoundingBox)
    Clip([absolute_units(point, t, unit_box, box)
          for point in property.points])
end


# A property primitive assigning an ID, in particular in SVG, to enable
# manipulation of portions of the graphic.
immutable SVGID <: PropertyPrimitive
    value::String
end


# Singleton sequence contructor.
svgid(value::String) = PropertySeq(SVGID(html_escape_string(value)))


# A property setting the class field in a form's group element.
immutable SVGClass <: PropertyPrimitive
    value::String
end


# Singleton sequence contructor.
svgclass(value::String) = PropertySeq(SVGClass(html_escape_string(value)))


# A SVG <a> tag.
immutable SVGLink <: PropertyPrimitive
    target::String
end


# Singleton sequence contructor.
svglink(target::String) = PropertySeq(SVGLink(target))


# The font property primitive.
immutable Font <: PropertyPrimitive
    family::String
end


# Singletone sequence constructor.
font(family::String) = PropertySeq(Font(family))


# The font size property.
immutable FontSize <: PropertyPrimitive
    value::Measure

    FontSize(value) = new(size_measure(value))
end


# Singletone sequence constructor.
fontsize(value) = PropertySeq(FontSize(value))


# Native unit conversion.
function absolute_units(property::FontSize,
                        t::Transform,
                        unit_box::UnitBox,
                        box::AbsoluteBoundingBox)
    FontSize(absolute_units(property.value, t, unit_box, box))
end


# Events:
# These correspond to DOM events, and associate some javascript blurb that gets
# executed when the event is triggered.

const events = (:OnActivate, :OnClick, :OnFocusIn, :OnFocusOut,
                :OnLoad, :OnMouseDown, :OnMouseMove, :OnMouseOut,
                :OnMouseOver, :OnMouseUp)

for event in events
    event_lc  = symbol(lowercase(string(event)))
    @eval begin
        immutable ($event) <: PropertyPrimitive
            value::String
        end

        function ($event_lc)(value::String)
            PropertySeq(($event)(value))
        end
    end
end


# SVG Mask objects

immutable SVGMask <: PropertyPrimitive
    id::String
end


svgmask(id::String) = PropertySeq(SVGMask(id))


immutable SVGDefineMask <: PropertyPrimitive
    id::String
end


svgdefmask(id::String) = PropertySeq(SVGDefineMask(id))

# A general purpose SVG attribute.

immutable SVGAttribute <: PropertyPrimitive
    attribute::String
    value::String
end


function svgattribute(attribute::String, value::String)
    PropertySeq(SVGAttribute(attribute, value))
end


# Embedding raw markup in SVG files.

# This is a property that when processed will be saved and then appended to the
# end of the SVG file, but ignored by default on other backends. This allows us
# to use some of SVGs more obsure features without exposing all of SVG.

immutable SVGEmbed <: PropertyPrimitive
    markup::String
end


svgembed(markup::String) = PropertySeq(SVGEmbed(markup))

# D3 css styles

immutable D3Style <: PropertyPrimitive
    attribute::String
    value::String
end


function d3style(attribute::String, value::String)
    PropertySeq(D3Style(attribute, value))
end


# Embedding raw js code in D3 output.

immutable D3Embed <: PropertyPrimitive
    code::String
    args::Vector{Measure}

    function D3Embed(code::String, args::Measure...)
        property = new(code, Array(Measure, length(args)))
        for (i, arg) in enumerate(args)
            property.args[i] = arg
        end
        property
    end
end


function d3embed(code::String, args::Measure...)
    PropertySeq(D3Embed(code, args...))
end


function absolute_units(property::D3Embed,
                        t::Transform,
                        unit_box::UnitBox,
                        box::AbsoluteBoundingBox)
    # we are going to build a new string by scanning across "code" and
    # replacing %x with translated x values, %y with translated y values
    # and %s with translated size values. Blah blah blah.
    newcode = IOBuffer()

    i = 1
    validx = 1
    while true
        j = search(property.code, '%', i)

        if j == 0
            write(newcode, property.code[i:end])
            break
        end

        write(newcode, property.code[i:j-1])
        if j == length(property.code)
            write(newcode, '%')
            break
        elseif property.code[j+1] == '%'
            write(newcode, '%')
        elseif property.code[j+1] == 'x'
            val = absolute_x_position(property.args[validx], t, unit_box, box)
            write(newcode, svg_fmt_float(val))
            validx += 1
        elseif property.code[j+1] == 'y'
            val = absolute_y_position(property.args[validx], t, unit_box, box)
            write(newcode, svg_fmt_float(val))
            validx += 1
        elseif property.code[j+1] == 's'
            val = absolute_units(property.args[validx], t, unit_box, box)
            write(newcode, svg_fmt_float(val.abs))
            validx += 1
        else
            write(newcode, '%', property.code[j+1])
        end

        i = j + 2
    end

    D3Embed(takebuf_string(newcode))
end


immutable D3Hook <: PropertyPrimitive
    code::String
end

d3hook(code::String) = PropertySeq(D3Hook(code))

