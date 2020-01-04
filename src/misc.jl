iszero(x::T) where T = x == zero(T)

Maybe(T::Type) = Union{T, Nothing}

function in_expr_args(ex::Expr)
    ex.head === :in && return ex.args[1], ex.args[2]
(ex.head === :call && length(ex.args) == 3 && ex.args[1] === :in) &&
    return ex.args[2], ex.args[3]
    error("Not an `in` expression")
end

# Cycle-zip. Zip two or more arrays, cycling the short ones.
function cyclezip(xs::AbstractArray...)
    any(map(isempty, xs)) && return Any[]
    n = maximum([length(x) for x in xs])
    return takestrict(zip([Iterators.cycle(x) for x in xs]...), n)
end


# This generates optimized code for a reoccuring pattern in forms and patterns
# that looks like:
#
#   return Circle([CirclePrimitive((x, y), x_measure(r))
#                  for (x, y, r) in cyclezip(xs, ys, rs)])
#
# This macro does the equivalent with
#
#   return @makeform (x in xs, y in ys, r in rs),
#                    CirclePrimitive((x, y), x_measure(r)))
#
# but much more efficiently.
macro makeform(args...)
    @assert 1 <= length(args) <= 2
    tag = length(args) == 2 ? args[2] : empty_tag
    args = args[1]
    @assert args.head == :tuple
    @assert length(args.args) == 2
    iterators, constructor = args.args

    maxlen_ex = quote begin n = 0 end end
    type_ex = quote begin end end
    iter_ex = quote begin end end

    for iterator in iterators.args
        var, arr = in_expr_args(iterator::Expr)
        ivar = Symbol(string("i_", var))

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

        primitives = Array{T}(undef, n)
        primitives[1] = prim1
        for i in 2:n
            $(iter_ex)
            primitives[i] = $(constructor)::T
        end
        Form{T}(primitives, $(tag))
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
        var, arr = in_expr_args(iterator::Expr)

        push!(maxlen_ex.args, quote
            if isempty($(esc(arr)))
                primitives = Array{$(T)}(undef, 0)
                @goto done
            end end)
        push!(maxlen_ex.args, quote n = max(n, length($(esc(arr)))) end)
        push!(iter_ex.args, quote
            $(esc(var)) = $(esc(arr))[((i - 1) % length($(esc(arr)))) + 1]
        end)
    end

    quote
        $(maxlen_ex)
        primitives = Array{$(esc(T))}(undef, n)
        for i in 1:n
            $(iter_ex)
            primitives[i] = $(esc(constructor))
        end
        @label done
        primitives
    end
end


narrow_polygon_point_types(point_arrays::AbstractArray{Vector{Tuple{XM, YM}}}) where {XM <: Measure, YM <: Measure} = (XM, YM)

type_params(p::Type{Tuple{XM, YM}}) where {XM, YM} = (Any, Any)
type_params(p::Type{Tuple{XM, YM}}) where {XM <: Measure, YM <: Measure} = (XM, YM)
type_params(p::Type{Union{}}) = (Any, Any)

function narrow_polygon_point_types(point_arrays::AbstractArray)
    if !isempty(point_arrays) && all([eltype(arr) <: Vec for arr in point_arrays])
        xm, ym = type_params(eltype(point_arrays[1]))
        for i in 2:length(point_arrays)
            type_params(eltype(point_arrays[i])) == (xm, ym) || return Any, Any
        end
        return xm, ym
    else
        return Any, Any
    end
end

function narrow_polygon_point_types(ring_arrays::Vector{Vector{Vector{P}}}) where P <: Tuple
    type_params(p::Type{Tuple{XM, YM}}) where {XM, YM} = (XM, YM)

    xm = nothing
    ym = nothing
    for point_arrays in ring_arrays
        if !isempty(point_arrays) && all([eltype(arr) <: Vec for arr in point_arrays])
            if xm == nothing
                xm, ym = type_params(eltype(point_arrays[1]))
            end
            for i in 2:length(point_arrays)
                type_params(eltype(point_arrays[i])) == (xm, ym) || Any, Any
            end
        end
    end
    if xm == nothing
        return Any, Any
    else
        return xy, ym
    end
end


# Hacks to make Dates time work as coordinates

if !hasmethod(/, (Dates.Day, Dates.Day))
    /(a::Dates.Day, b::Dates.Day) = a.value / b.value
end

if !hasmethod(/, (Dates.Day, Real))
    /(a::Dates.Day, b::Real) = Dates.Day(round(Int64, (a.value / b)))
end
/(a::Dates.Day, b::AbstractFloat) = convert(Dates.Millisecond, a) / b

if !hasmethod(/, (Dates.Millisecond, Dates.Millisecond))
    /(a::Dates.Millisecond, b::Dates.Millisecond) = a.value / b.value
end

if !hasmethod(/, (Dates.Millisecond, Real))
    /(a::Dates.Millisecond, b::Real) = Dates.Millisecond(round(Int64, (a.value / b)))
end
/(a::Dates.Millisecond, b::AbstractFloat) = Dates.Millisecond(round(Int64, (a.value / b)))


if !hasmethod(-, (Dates.Date, Dates.DateTime))
    -(a::Dates.Date, b::Dates.DateTime) = convert(Dates.DateTime, a) - b
end

+(a::Dates.Date, b::Dates.Millisecond) = convert(Dates.DateTime, a) + b

if !hasmethod(-, (Dates.DateTime, Dates.Date))
    -(a::Dates.DateTime, b::Dates.Date) = a - convert(Dates.DateTime, b)
end


if !hasmethod(/, (Dates.Day, Dates.Millisecond))
    /(a::Dates.Day, b::Dates.Millisecond) = convert(Dates.Millisecond, a) / b
end

if !hasmethod(/, (Dates.Millisecond, Dates.Day))
    /(a::Dates.Millisecond, b::Dates.Day) = a / convert(Dates.Millisecond, b)
end


for T in [Dates.Hour, Dates.Minute, Dates.Second, Dates.Millisecond]
    if !hasmethod(-, (Dates.Date, T))
        @eval begin
            -(a::Dates.Date, b::$(T)) = convert(Dates.DateTime, a) - b
        end
    end
end


*(a::AbstractFloat, b::Dates.Day) = Dates.Day(round(Int64, (a * b.value)))
*(a::Dates.Day, b::AbstractFloat) = b * a
*(a::AbstractFloat, b::Dates.Millisecond) = Dates.Millisecond(round(Int64, (a * b.value)))
*(a::Dates.Millisecond, b::AbstractFloat) = b * a

