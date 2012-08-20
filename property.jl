
# Property: a thing that changes how things are drawn.

require("backend.jl")

# A bare property.
abstract PropertyType

# A container for one or more properties.
type Property
    specifics::Vector{PropertyType}

    function Property()
        new(PropertyType[])
    end
end


function copy(a::Property)
    Property(copy(a.specifics))
end


function isempty(a::Property)
    isempty(a.specifics)
end


# Catchall nop method for applying a property that isn't imlpmented on a backend
function draw(backend::Backend, property::Property)
    for p in property.specifics
        draw(backend, p)
    end
end

function draw(backend::Backend, property::PropertyType)
end

# TODO: Fill, Stroke, LineWidth, etc



