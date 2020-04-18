# Font handling when pango and fontconfig are not available.

# Define this even if we're not calling pango, since cairo needs it.
const PANGO_SCALE = 1024.0

# Serialized glyph sizes for commont fonts.
const glyphsizes = open(fd -> JSON.parse(read(fd, String)),
                        joinpath(@__DIR__, "..", "deps", "glyphsize.json"))

# It's better to overestimate text extents than to underestimes, since the later
# leads to overlaping where the former just results in some extra space. So, we
# scale estimated text extends by this number. Width on the other hand tends be
# overestimated since it doesn't take kerning into account.
const text_extents_scale_x = 1.0
const text_extents_scale_y = 1.0

# Normalized Levenshtein distance between two strings.
function levenshtein(a::AbstractString, b::AbstractString)
    a = replace(lowercase(a), r"\s+"=>"")
    b = replace(lowercase(b), r"\s+"=>"")
    n = length(a)
    m = length(b)
    D = zeros(UInt, n + 1, m + 1)

    D[:,1] = 0:n
    D[1,:] = 0:m

    for i in 2:n, j in 2:m
        if a[i - 1] == b[j - 1]
            D[i, j] = D[i - 1, j - 1]
        else
            D[i, j] = 1 +  min(D[i - 1, j - 1], D[i, j - 1], D[i - 1, j])
        end
    end

    return D[n,m] / max(n, m)
end

# Find the nearst typeface from the glyph size table.
let
    matched_font_cache = Dict{AbstractString, AbstractString}()
    global match_font

    function match_font(families::AbstractString)
        haskey(matched_font_cache, families) && return matched_font_cache[families]

        smallest_dist = Inf
        best_match = "Helvetica"
        for family in [lowercase(strip(family, [' ', '"', '\''])) for family in split(families, ',')]
            for available_family in keys(glyphsizes)
                d = levenshtein(family, available_family)
                if d < smallest_dist
                    smallest_dist = d
                    best_match = available_family
                end
            end
        end

        matched_font_cache[families] = best_match
        return best_match
    end
end

# Approximate width of a text in millimeters.
#
# Args:
#   widths: A glyph width table from glyphsizes.
#   text: Any string.
#   size: Font size in points.
#
# Returns:
#   Approximate text width in millimeters.
#
function text_width(widths::Dict, text::AbstractString, size::Float64)
    stripped_text = replace(text, r"<[^>]*>"=>"")
    width = 0
    for c in stripped_text
        width += get(widths, string(c), widths["w"])
    end
    width
end

function max_text_extents(font_family::AbstractString, size::Measure,
                          texts::AbstractString...)
    isa(size, AbsoluteLength) ||
            error("text_extents requries font size be in absolute units")
    scale = size / 12pt
    font_family = match_font(font_family)
    glyphheight = glyphsizes[font_family]["height"]
    widths = glyphsizes[font_family]["widths"]

    fontsize = size/pt
    chunkwidths = Float64[]
    textheights = Float64[]
    for text in texts
        textheight = 0.0
        for chunk in split(text, '\n')
            chunkheight = glyphheight
            if match(r"<su(p|b)>", chunk) != nothing
                chunkheight *= 1.5
            end
            textheight += chunkheight
            push!(chunkwidths, text_width(widths, chunk, fontsize))
        end
        push!(textheights, textheight)
    end

    width = maximum(chunkwidths)
    height = maximum(textheights)
    (text_extents_scale_x * scale * width * mm,
     text_extents_scale_y * scale * height * mm)
end

function text_extents(font_family::AbstractString, size::Measure, texts::AbstractString...)
    scale = size / 12pt
    font_family = match_font(font_family)
    glyphheight = glyphsizes[font_family]["height"]
    glyphwidths = glyphsizes[font_family]["widths"]
    fontsize = size/pt

    extents = Array{Tuple{Measure, Measure}}(undef, length(texts))
    for (i, text) in enumerate(texts)
        chunkwidths = Float64[]
        textheight = 0.0
        for chunk in split(text, "\n")
            chunkheight = glyphheight
            if match(r"<su(p|b)>", text) != nothing
                chunkheight *= 1.5
            end
            textheight += chunkheight
            push!(chunkwidths, text_width(glyphwidths, chunk, fontsize))

        end
        width = maximum(chunkwidths)
        extents[i] = (text_extents_scale_x * scale * width * mm,
                      text_extents_scale_y * scale * textheight * mm)
    end

    return extents
end

const carriage_shift0 = 1.1
const carriage_shift_supsub = 0.1
const supsub_shift = 0.4

# Amazingly crude fallback to parse pango markup into svg.
function pango_to_svg(text::AbstractString)
    pat = r"<(/?)\s*([^>]*)\s*>"
    output = IOBuffer()
    output_line = IOBuffer()
    textlines = split(text, "\n")
    carriage_shift = carriage_shift0
    for (itextline,textline) in enumerate(textlines)
        input = codeunits(textline)
        lastpos = 1
        baseline_shift = 0.0
        sup = sub = false
        open_tag = false

        for mat in eachmatch(pat, textline)
            write(output_line, input[lastpos:mat.offset-1])
            lastpos = mat.offset + length(mat.match)

            closing_tag = mat.captures[1] == "/"

            open_tag && !closing_tag && write(output_line, "</tspan>")

            if closing_tag
                write(output_line, "</tspan>")
            else
                if mat.captures[2] == "sup"
                    write(output_line, "<tspan dy=\"-$(supsub_shift)em\" font-size=\"83%\">")
                    baseline_shift = -supsub_shift * 0.83
                    sup = true
                elseif mat.captures[2] == "sub"
                    write(output_line, "<tspan dy=\"$(supsub_shift)em\" font-size=\"83%\">")
                    baseline_shift = supsub_shift * 0.83
                    sub = true
                elseif mat.captures[2] == "i"
                    write(output_line, "<tspan font-style=\"italic\">")
                elseif mat.captures[2] == "b"
                    write(output_line, "<tspan font-weight=\"bold\">")
                end
            end

            if closing_tag && baseline_shift != 0.0
                if lastpos < length(input)
                    @printf(output_line, "<tspan dy=\"%0.4fem\">", -baseline_shift)
                    baseline_shift = 0.0
                    open_tag = true
                else
                    open_tag = false
                end
            end
        end
        write(output_line, input[lastpos:end])
        open_tag && write(output_line, "</tspan>")
        itextline>1 && @printf(output,
                               "<tspan x=\"0\" dy=\"%0.4fem\">",
                               carriage_shift + sup*carriage_shift_supsub)
        write(output, String(take!(output_line)))
        itextline>1 && write(output, "</tspan>")
        carriage_shift = carriage_shift0 - baseline_shift + sub*carriage_shift_supsub
    end
    String(take!(output))
end
