
# Property: a thing that changes how other things are drawn.

require("backend.jl")
require("color.jl")
require("measure.jl")

# A property primitive is something can be directly applied.
abstract PropertyPrimitive


# The identity element of the Property monoid.
type EmptyProperty end
const empty_property = EmptyProperty()


# A non-empty sequence of property primitives.
type PropertySeq
    primitive::PropertyPrimitive
    next::Union(EmptyProperty, PropertySeq)

    function PropertySeq(primitive::PropertyPrimitive,
                         next::Union(EmptyProperty, PropertySeq))
        new(primitive, next)
    end

    function PropertySeq(primitive::PropertyPrimitive)
        new(primitive, empty_property)
    end
end


function copy(a::PropertySeq)
    PropertySeq(a.primitive, a.next)
end


# A property is a (possibly empty) sequence of property primitives. The set of
# Properties with the compose operator forms a monoid (specifically, it can be
# thought of as a free monoid). The empty_property constant is the identity
# element.
const Property = Union(EmptyProperty, PropertySeq)


# The compose function for properties.
#
# This operation prepends b to a. Append might be used, but since we are using a
# list and operators or left associative, prepend is used for efficiency.
function compose(a::PropertySeq, b::Property)
    d = copy(b)
    while !is(d.next, empty_property)
        d.next = copy(d.next)
    end
    d.next = a
end


# Composition with the identity element.
compose(a::EmptyProperty, b::EmptyProperty) = a
compose(a::EmptyProperty, b::PropertySeq) = a
compose(a::PropertySeq, b::EmptyProperty) = b


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
    ps = [empty_property]
    while !is(property, empty_property)
        append!(ps, native_measure(property.primitive, t,
                                   unit_box, box, backend))
        property = property.next
    end
    reverse!(ps)

    reduce(compose, ps)
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
type FillPrimitive <: PropertyPrimitive
    value::ColorOrNothing

    FillPrimitive(value::ColorOrNothing) = new(value)
    FillPrimitive(value::String) = new(color(value))
end


# Singleton sequence contructor.
Fill(value) = PropertySeq(FillPrimitive(value))


# A property primitive controlling the strok color (or lack of color) of a form.
type StrokePrimitive <: PropertyPrimitive
    value::ColorOrNothing

    StrokePrimitive(value::ColorOrNothing) = new(value)
    StrokePrimitive(value::String) = new(color(value))
end


# Singleton sequence contructor.
Stroke(value) = PropertySeq(StrokePrimitive(value))


# A property primitive controlling the widths of lines drawn in stroke
# operations.
type LineWidthPrimitive <: PropertyPrimitive
    value::Measure

    LineWidthPrimitive(value::MeasureOrNumber) = new(size_measure(value))
end


# Singleton sequence contructor.
LineWidth(value::MeasureOrNumber) = PropertySeq(LineWidthPrimitive(value))


# Unit conversion
function native_measure(property::LineWidthPrimitive,
                        t::NativeTransform,
                        unit_box::BoundingBox,
                        box::NativeBoundingBox,
                        backend::Backend)
    LineWidthPrimitive(native_measure(property.value, t, unit_box, box, backend))
end


# A property primitive assigning an ID, in particular in SVG, to enable
# manipulation of portions of the graphic.
type IDPrimitive <: PropertyPrimitive
    value::String
end


# Singleton sequence contructor.
ID(value::String) = PropertySeq(IDPrimitive(value))


# The font property primitive.
type FontPrimitive <: PropertyPrimitive
    family::String
end


# Singletone sequence constructor.
Font(family::String) = PropertySeq(FontPrimitive(family))


# The font size property.
type FontSizePrimitive <: PropertyPrimitive
    value::Measure

    FontSize(value::MeasureOrNumber) = new(size_measure(value))
end


# Singletone sequence constructor.
FontSize(value::MeasureOrNumber) = PropertySeq(FontSizePrimitive(value))


# Native unit conversion.
function native_measure(property::FontSizePrimitive,
                        t::NativeTransform,
                        unit_box::BoundingBox,
                        box::NativeBoundingBox,
                        backend::Backend)
    FontSizePrimitive(native_measure(property.value, t, unit_box, box, backend))
end


# Events:
# These correspond to DOM events, and associate some javascript blurb that gets
# executed when the event is triggered.

const events = (:OnActivate, :OnClick, :OnFocusIn, :OnFocusOut,
                :OnLoad, :OnMouseDown, :OnMouseMove, :OnMouseOut,
                :OnMouseOver, :OnMouseUp)

for event in events
    event_prim  = symbol(join([string(event), "Primitive"]))
    @eval begin
        type ($event_prim) <: PropertyPrimitive
            value::String
        end

        function ($event)(value::String)
            PropertySeq(($event_prim)(value))
        end
    end
end

