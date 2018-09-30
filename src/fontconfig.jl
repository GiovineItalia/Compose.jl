# Use the freetype/fontconfig backend to find the best match to a font
# description.
#
# Args:
#   desc: A string giving the font description. This can
#         also provide a comma-separated list of families. E.g.,
#         "Helvetica, Arial 10"
#
# Returns:
#   A pointer to a PangoFontDescription with the closest match.
#
let available_font_families = Set{AbstractString}()
    for font_pattern in Fontconfig.list()
        push!(available_font_families, lowercase(Fontconfig.format(font_pattern, "%{family}")))
    end

    meta_families = Set(["serif", "sans", "sans-serif", "monospace", "cursive", "fantasy"])

    global match_font
    function match_font(families::AbstractString, size::Float64)
        matched_family = "sans-serif"
        for family in [lowercase(strip(family, [' ', '"', '\''])) for family in split(families, ',')]
            if family in available_font_families || family in meta_families
                matched_family = family
                break
            end
        end
        Fontconfig.format(match(Fontconfig.Pattern(family=family)), "%{family}")
    end
end
