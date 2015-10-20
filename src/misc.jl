

#function all_equal(xs::AbstractArray)
    #if isempty(xs)
        #return true
    #end
    #x = xs[1]
    #for i in 2:length(xs)
        #if xs[i] != x
            #return false
        #end
    #end
    #return x
#end


function iszero{T}(x::T)
    return x == zero(T)
end


function Maybe(T::Type)
    return @compat(Union{T, (@compat Void)})
end


# Cycle-zip. Zip two or more arrays, cycling the short ones.
function cyclezip(xs::AbstractArray...)
    if any(map(isempty, xs))
        return Any[]
    end
    n = maximum([length(x) for x in xs])
    return takestrict(zip([cycle(x) for x in xs]...), n)
end


# This generates optimized code for a reoccuring pattern in forms and patterns
# that looks like:
#
#   return Circle([CirclePrimitive(Point(x, y), x_measure(r))
#                  for (x, y, r) in cyclezip(xs, ys, rs)])
#
# This macro does the equivalent with
#
#   return @makeform (x in xs, y in ys, r in rs),
#                    CirclePrimitive(Point(x, y), x_measure(r)))
#
# but much more efficiently.
macro makeform(args)
    @assert args.head == :tuple
    @assert length(args.args) == 2
    iterators, constructor = args.args

    maxlen_ex = quote begin n = 0 end end
    type_ex = quote begin end end
    iter_ex = quote begin end end

    for iterator in iterators.args
        @assert iterator.head == :in
        var = iterator.args[1]
        arr = iterator.args[2]
        ivar = symbol(string("i_", var))

        push!(maxlen_ex.args,
            quote
                n = max(n, length($(arr)))
                if isempty($(arr))
                    error("Form cannot be constructed from an empty array")
                end
            end)
        push!(type_ex.args, quote
              $(ivar) = 1
              $(var) = $(arr)[1]
              end)
        push!(iter_ex.args, quote
              $(ivar) += 1
              $(ivar) = $(ivar) > length($(arr)) ? 1 : $(ivar)
              $(var) = $(arr)[$(ivar)]
        end)
    end

    esc(quote
        $(maxlen_ex)

        $(type_ex)
        prim1 = $(constructor)
        T = typeof(prim1)

        primitives = Array(T, n)
        primitives[1] = prim1
        for i in 2:n
            $(iter_ex)
            primitives[i] = $(constructor)::T
        end
        primitives
    end)
end


# TODO: Remove after replacing usage in property.jl
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


function narrow_polygon_point_types{XM <: Measure, YM <: Measure}(
            point_arrays::AbstractArray{Vector{Tuple{XM, YM}}})
    return (XM, YM)
end


type_params{XM, YM}(p::Type{Tuple{XM, YM}}) = (Any, Any)
type_params{XM <: Measure, YM <: Measure}(p::Type{Tuple{XM, YM}}) = (XM, YM)

function narrow_polygon_point_types(point_arrays::AbstractArray)
    if !isempty(point_arrays) && all([eltype(arr) <: Vec for arr in point_arrays])
        xm, ym = type_params(eltype(point_arrays[1]))
        for i in 2:length(point_arrays)
            if type_params(eltype(point_arrays[i])) != (xm, ym)
                return Any, Any
            end
        end
        return xm, ym
    else
        return Any, Any
    end
end


function narrow_polygon_point_types{P <: Tuple}(ring_arrays::Vector{Vector{Vector{P}}})
    type_params{XM, YM}(p::Type{Tuple{XM, YM}}) = (XM, YM)

    xm = nothing
    ym = nothing
    for point_arrays in ring_arrays
        if !isempty(point_arrays) && all([eltype(arr) <: Vec for arr in point_arrays])
            if xm == nothing
                xm, ym = type_params(eltype(point_arrays[1]))
            end
            for i in 2:length(point_arrays)
                if type_params(eltype(point_arrays[i])) != (xm, ym)
                    return Any, Any
                end
            end
        end
    end
    if xm == nothing
        return Any, Any
    else
        return xy, ym
    end
end


