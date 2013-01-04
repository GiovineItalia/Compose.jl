

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


# Characters to escape in XML/SVG/HTML string.
const html_escaped_chars =
    {'&' => "&amp;",
     '\'' => "&#39;",
     '<' => "&lt;",
     '>' => "&gt;",
     '"' => "&quot;"}

# Escape a string for printing it html or svg documents.
function html_escape_string(io, s::String)
    for c in s
        if has(html_escaped_chars, c)
            print(io, html_escaped_chars[c])
        else
            print(io, c)
        end
    end
end

html_escape_string(s::String) = sprint(length(s), html_escape_string, s)

