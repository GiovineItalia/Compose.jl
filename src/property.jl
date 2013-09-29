
# Property: a thing that changes how other things are drawn.

import Base.fill

export stroke, fill, linewidth, font, fontsize, visible, clip, opacity, svgid,
       svgclass, svglink, onactive, onclick, onfocusin, onfocusout, onload,
       onmousedown, onmousemove, onmouseout, onmouseover, onmouseup, svgmask,
       svgdefmask, svgembed, svgattribute, d3embed, d3hook

# A property primitive is something can be directly applied.
abstract PropertyPrimitive


# A property is a (possibly empty) sequence of property primitives. The set of
# Properties with the combine operator forms a monoid (specifically, it can be
# thought of as a free monoid). The empty_property constant is the identity
# element.
abstract Property


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


# A property primitive controlling the strok color (or lack of color) of a form.
immutable Stroke <: PropertyPrimitive
    value::ColorOrNothing

    Stroke(value::ColorOrNothing) = new(value)
    Stroke(value::String) = new(color(value))
end


# Singleton sequence contructor.
stroke(value) = PropertySeq(Stroke(value))


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


# Embedding raw js code in D3 output.

immutable D3Embed <: PropertyPrimitive
    code::String
end

immutable D3Hook <: PropertyPrimitive
    code::String
end

d3hook(code::String) = PropertySeq(D3Hook(code))

