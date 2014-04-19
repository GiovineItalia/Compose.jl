

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


immutable StrokePrimitive <: PropertyPrimitive
	color::Maybe(ColorValue)
end

typealias Stroke Property{StrokePrimitive}

function stroke(c::Nothing)
    return Stroke([StrokePrimitive(c)])
end

function stroke(c::Union(ColorValue, String))
	return Stroke([StrokePrimitive(color(c))])
end

function stroke(cs::AbstractArray)
	return Stroke([StrokePrimitive(c == nothing ? c : color(c)) for c in cs])
end


immutable FillPrimitive <: PropertyPrimitive
	color::Union(ColorValue, Nothing)
end

typealias Fill Property{FillPrimitive}

function fill(c::Nothing)
    return Fill([FillPrimitive(c)])
end

function fill(c::Union(ColorValue, String))
	return Fill([FillPrimitive(color(c))])
end

function fill(cs::AbstractArray)
	return Fill([FillPrimitive(c == nothing ? c : color(c)) for c in cs])
end

