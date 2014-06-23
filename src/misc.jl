

function iszero{T}(x::T)
    return x == zero(T)
end


function Maybe(T::Type)
    return Union(T, Nothing)
end


# Cycle-zip. Zip two or more arrays, cycling the short ones.
function cyclezip(xs::AbstractArray...)
    if any(map(isempty, xs))
        return {}
    end
    n = maximum([length(x) for x in xs])
    return take(zip([cycle(x) for x in xs]...), n)
end


