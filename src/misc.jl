

function iszero{T}(x::T)
    return x == zero(T)
end


function Maybe(T::Type)
    return Union(T, Nothing)
end


# Cycle-zip. Zip two or more arrays, cycling the short ones.
function cyclezip(xs::AbstractArray...)
    if any(map(isempty, xs))
        return []
    end
    n = maximum([length(x) for x in xs])
    return takestrict(zip([cycle(x) for x in xs]...), n)
end


# This generates optimized code for a reoccuring pattern in forms and patters
# that looks like:
#
#   return Circle([CirclePrimitive(Point(x, y), x_measure(r))
#                  for (x, y, r) in cyclezip(xs, ys, rs)])
#
# This macro does the equivalent with
#
#   return Circle(@makeprimitives CirclePrimitive,
#                        (x in xs, y in ys, r in rs),
#                        CirclePrimitive(Point(x, y), x_measure(r)))
#
# but much more efficiently.
macro makeprimitives(args)
    @assert args.head == :tuple
    @assert length(args.args) == 3
    T, iterators, constructor = args.args

    maxlen_ex = quote begin n = 0 end end
    iter_ex = quote begin end end

    for iterator in iterators.args
        @assert iterator.head == :in
        var = iterator.args[1]
        arr = iterator.args[2]

        push!(maxlen_ex.args, quote
            if isempty($(arr))
                primitives = Array($(T), 0)
                @goto done
            end end)
        push!(maxlen_ex.args, quote n = max(n, length($(arr))) end)
        push!(iter_ex.args, quote
            $(var) = $(arr)[((i - 1) % length($(arr))) + 1]
        end)
    end

    quote
        $(maxlen_ex)
        primitives = Array($(T), n)
        for i in 1:n
            $(iter_ex)
            primitives[i] = $(constructor)
        end
        @label done
        primitives
    end
end


