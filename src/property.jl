
# Property: a thing that changes how other things are drawn.

export stroke, fill, linewidth, font, fontsize, svgid, svgclass, svglink,
       onactive, onclick, onfocusin, onfocusout, onload, onmousedown,
       onmousemove, onmouseout, onmouseover, onmouseup

require("Compose/src/backend.jl")
require("Compose/src/color.jl")
require("Compose/src/measure.jl")

# A property primitive is something can be directly applied.
abstract PropertyPrimitive


# A property is a (possibly empty) sequence of property primitives. The set of
# Properties with the combine operator forms a monoid (specifically, it can be
# thought of as a free monoid). The empty_property constant is the identity
# element.
abstract Property


# An empty (i.e., nop) property which form the identity element of the Property
# monoid.
type EmptyProperty <: Property end
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

function native_measure(property::EmptyProperty,
                        t::NativeTransform,
                        unit_box::BoundingBox,
                        box::NativeBoundingBox,
                        backend::Backend)
    property
end


# Convert a sequence of properties to a sequence of properties in native
# coordinates.
function native_measure(property::PropertySeq,
                        t::NativeTransform,
                        unit_box::BoundingBox,
                        box::NativeBoundingBox,
                        backend::Backend)
    p = property = copy(property)
    while !is(p, empty_property)
        p.primitive = native_measure(p.primitive, t, unit_box, box, backend)
        p.next = copy(p.next)
        p = p.next
    end
    property
end


# Catchall for properties that don't require unit conversion.
function native_measure(property::PropertyPrimitive,
                        t::NativeTransform,
                        unit_box::BoundingBox,
                        box::NativeBoundingBox,
                        backend::Backend)
    property
end


# A property primitive controlling the fill color (or lack of color) of a form.
type Fill <: PropertyPrimitive
    value::ColorOrNothing

    Fill(value::ColorOrNothing) = new(value)
    Fill(value::String) = new(color(value))
end


# Singleton sequence contructor.
fill(value) = PropertySeq(Fill(value))


# A property primitive controlling the strok color (or lack of color) of a form.
type Stroke <: PropertyPrimitive
    value::ColorOrNothing

    Stroke(value::ColorOrNothing) = new(value)
    Stroke(value::String) = new(color(value))
end


# Singleton sequence contructor.
stroke(value) = PropertySeq(Stroke(value))


# A property primitive controlling the widths of lines drawn in stroke
# operations.
type LineWidth <: PropertyPrimitive
    value::Measure

    LineWidth(value::MeasureOrNumber) = new(size_measure(value))
end


# Singleton sequence contructor.
linewidth(value::MeasureOrNumber) = PropertySeq(LineWidth(value))


# Unit conversion
function native_measure(property::LineWidth,
                        t::NativeTransform,
                        unit_box::BoundingBox,
                        box::NativeBoundingBox,
                        backend::Backend)
    LineWidth(native_measure(property.value, t, unit_box, box, backend))
end


# A property primitive assigning an ID, in particular in SVG, to enable
# manipulation of portions of the graphic.
type SVGID <: PropertyPrimitive
    value::String
end


# Singleton sequence contructor.
svgid(value::String) = PropertySeq(SVGID(value))


# A property setting the class field in a form's group element.
type SVGClass <: PropertyPrimitive
    value::String
end


# Singleton sequence contructor.
svgclass(value::String) = PropertySeq(SVGClass(value))


# A SVG <a> tag.
type SVGLink <: PropertyPrimitive
    target::String
end


# Singleton sequence contructor.
svglink(target::String) = PropertySeq(SVGLink(target))


# The font property primitive.
type Font <: PropertyPrimitive
    family::String
end


# Singletone sequence constructor.
font(family::String) = PropertySeq(Font(family))


# The font size property.
type FontSize <: PropertyPrimitive
    value::Measure

    FontSize(value::MeasureOrNumber) = new(size_measure(value))
end


# Singletone sequence constructor.
fontsize(value::MeasureOrNumber) = PropertySeq(FontSize(value))


# Native unit conversion.
function native_measure(property::FontSize,
                        t::NativeTransform,
                        unit_box::BoundingBox,
                        box::NativeBoundingBox,
                        backend::Backend)
    FontSize(native_measure(property.value, t, unit_box, box, backend))
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
        type ($event) <: PropertyPrimitive
            value::String
        end

        function ($event_lc)(value::String)
            PropertySeq(($event)(value))
        end
    end
end

