
# Some limited serialization to json.

function json{T}(xs::Vector{T})
    @sprintf("[%s]", join([json(x) for x in xs], ","))
end

function json{T <: Union(Symbol, String), S}(xs::Dict{T, S})
    @sprintf("{%s}", join([@sprintf("%s:%s", quote_string(k), v)
                           for (k, v) in xs], ","))
end

json(x::String)  = quote_string(x)
json(x::Int)     = string(x)
json(x::Float)   = string(x)
json(i::Nothing) = "null"
json(x::Any)     = quote_string(string(x))

