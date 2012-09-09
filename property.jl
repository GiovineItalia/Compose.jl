
# Property: a thing that changes how things are drawn.

require("backend.jl")
require("color.jl")
require("measure.jl")

# A bare property.
abstract PropertyType

# A container for one or more properties.
type Property
    specifics::Vector{PropertyType}

    function Property()
        new(PropertyType[])
    end

    function Property(specifics::Vector{PropertyType})
        new(specifics)
    end
end


function copy(a::Property)
    Property(copy(a.specifics))
end


function isempty(a::Property)
    isempty(a.specifics)
end


function native_measure(backend::Backend, t::NativeTransform, unit_box::BoundingBox,
                        box::NativeBoundingBox, property::Property)
    native_property = Property()
    native_property.specifics = [native_measure(backend, t, unit_box, box, p)
                                 for p in propert.specifics]
    native_property
end


# Catchall for properties that don't require unit conversion.
function native_measure(backend::Backend, t::NativeTransform, unit_box::BoundingBox,
                        box::NativeBoundingBox, property::PropertyType)
    property
end


type Fill <: PropertyType
    value::ColorOrNothing

    function Fill(value::ColorOrNothing)
        Property(PropertyType[new(value)])
    end

    function Fill(value::String)
        Property(PropertyType[new(color(value))])
    end

    function Fill(p::Fill)
        new(copy(p.value))
    end

    Fill() = new()
end

copy(p::Fill) = Fill(p)


function FillBare(value::ColorOrNothing)
    p = Fill()
    p.value = value
    p
end


type Stroke <: PropertyType
    value::ColorOrNothing

    function Stroke(value::ColorOrNothing)
        Property(PropertyType[new(value)])
    end

    function Stroke(value::String)
        Property(PropertyType[new(color(value))])
    end

    function Stroke(p::Stroke)
        new(copy(p.value))
    end

    Stroke() = new()
end

copy(p::Stroke) = Stroke(p)


function StrokeBare(value::ColorOrNothing)
    p = Stroke()
    p.value = value
    p
end


type LineWidth <: PropertyType
    value::Measure

    function LineWidth(value::MeasureOrNumber)
        Property(PropertyType[new(size_measure(value))])
    end

    function LineWidth(p::LineWidth)
        new(copy(p.value))
    end

    LineWidth() = new()
end

copy(p::LineWidth) = LineWidth(p)


function LineWidthBare(value::MeasureOrNumber)
    p = LineWidth()
    p.value = size_measure(value)
    p
end


function native_measure(backend::Backend, t::NativeTransform, unit_box::BoundingBox,
                        box::NativeBoundingBox, property::LineWidth)
    LineWidthBare(native_measure(property.value, t, unit_box, box, backend))
end


type ID <: PropertyType
    value::String

    function ID(value::String)
        Property(PropertyType[new(value)])
    end

    function ID(p::ID)
        new(p.value)
    end
end

copy(p::ID) = ID(p)


# Events

const events = (:OnActivate, :OnClick, :OnFocusIn, :OnFocusOut,
                :OnLoad, :OnMouseDown, :OnMouseMove, :OnMouseOut,
                :OnMouseOver, :OnMouseUp)

for event in events
    @eval begin
        type ($event) <: PropertyType
            value::String

            function ($event)(value::String)
                Property(PropertyType[new(value)])
            end

            function ($event)(p::($event))
                new(p.value)
            end
        end

        copy(p::($event)) = ($event)(p)
    end
end


