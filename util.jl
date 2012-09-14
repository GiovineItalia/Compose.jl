

Maybe(T::Type) = Union(T, Nothing)


# Float64 -> String, trimming trailing zeros when appropriate.
# This is largely taken from cairo's function _cairo_dtostr.
function fmt_float(x::Float64)
    if x < 0.1
        a = @sprintf("%0.18f", x)
    else
        a = @sprintf("%f", x)
    end

    n = length(a)
    while a[n] == '0'
        n -= 1
    end

    if a[n] == '.'
        n -= 1
    end

    a[1:n]
end


