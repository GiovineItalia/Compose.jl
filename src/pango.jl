# Estimation of text extents using pango.

const libpangocairo = Cairo.libpangocairo
const libpango = Cairo.libpango
const libgobject = Cairo.libgobject

# Cairo text backend
const CAIRO_FONT_TYPE_TOY = 0
const CAIRO_FONT_TYPE_FT = 1
const CAIRO_FONT_TYPE_WIN32 = 2
const CAIRO_FONT_TYPE_QUARTZ = 3
const CAIRO_FONT_TYPE_USER = 4

# Mirroring a #define in the pango header.
const PANGO_SCALE = 1024.0

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
        family = Fontconfig.format(match(Fontconfig.Pattern(family=matched_family)), "%{family}")
        desc = @sprintf("%s %fpx", family, size)
        fd = ccall((:pango_font_description_from_string, libpango), Ptr{Cvoid}, (Ptr{UInt8},), desc)
        return fd
    end
end

# Thin wrapper for a pango_layout object.
mutable struct PangoLayout
    layout::Ptr{Cvoid}
end

function PangoLayout()
    layout = ccall((:pango_layout_new, libpango),
                   Ptr{Cvoid}, (Ptr{Cvoid},), pango_cairo_ctx[])
    # TODO: finalizer?

    PangoLayout(layout)
end

# Set the layout's font.
function pango_set_font(pangolayout::PangoLayout, family::AbstractString, pts::Number)
    fd = match_font(family, pts)
    ccall((:pango_layout_set_font_description, libpango),
          Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}), pangolayout.layout, fd)
end

# Find the width and height of a string.
#
# Args:
#   pangolayout: a pango layout object, with font, etc, set.
#   text: a string we might like to draw.
#
# Returns:
#   A (width, height) tuple in absolute units.
#
function pango_text_extents(pangolayout::PangoLayout, text::AbstractString)
    textarray = convert(String, text)
    ccall((:pango_layout_set_markup, libpango),
          Cvoid, (Ptr{Cvoid}, Ptr{UInt8}, Int32),
          pangolayout.layout, textarray, sizeof(textarray))

    extents = Array{Int32}(undef, 4)
    ccall((:pango_layout_get_extents, libpango),
          Cvoid, (Ptr{Cvoid}, Ptr{Int32}, Ptr{Int32}),
          pangolayout.layout, extents, C_NULL)

    width, height = (extents[3] / PANGO_SCALE)pt, (extents[4] / PANGO_SCALE)pt
end

# Find the minimum width and height needed to fit any of the given strings.
#
# (A "user-friendly" wrapper for pango_text_extents.)
#
# Args:
#   font_family: Something like a font name.
#   pts: Font size in points.
#   texts: One or more strings.
#
# Returns:
#   A (width, height) tuple in absolute units.
#
function max_text_extents(font_family::AbstractString, pts::Float64, texts::AbstractString...)
    pango_set_font(pangolayout[]::PangoLayout, font_family, pts)
    max_width  = 0mm
    max_height = 0mm
    for text in texts
        (width, height) = pango_text_extents(pangolayout[]::PangoLayout, text)
        max_width  = max_width.value  < width.value  ? width  : max_width
        max_height = max_height.value < height.value ? height : max_height
    end
    return (max_width, max_height)
end

# Same as max_text_extents but with font_size in arbitrary absolute units.
function max_text_extents(font_family::AbstractString, size::Measure,
                      texts::AbstractString...)
    isa(size, AbsoluteLength) || error("text_extents requries font size be in absolute units")
    return max_text_extents(font_family, size/pt, texts...)
end

# Return an array with the extents of each element
function text_extents(font_family::AbstractString, pts::Float64, texts::AbstractString...)
    pango_set_font(pangolayout[]::PangoLayout, font_family, pts)
    return [pango_text_extents(pangolayout[]::PangoLayout, text) for text in texts]
end

text_extents(font_family::AbstractString, size::Measure, texts::AbstractString...) =
        text_extents(font_family, size/pt, texts...)

const pango_attrs = [
    (:PANGO_ATTR_LANGUAGE,        :PangoAttrLanguage),
    (:PANGO_ATTR_FAMILY,          :PangoAttrString),
    (:PANGO_ATTR_STYLE,           :PangoAttrInt),
    (:PANGO_ATTR_WEIGHT,          :PangoAttrInt),
    (:PANGO_ATTR_VARIANT,         :PangoAttrInt),
    (:PANGO_ATTR_STRETCH,         :PangoAttrInt),
    (:PANGO_ATTR_SIZE,            :PangoAttrSize),
    (:PANGO_ATTR_FONT_DESC,       :PangoAttrFontDesc),
    (:PANGO_ATTR_FOREGROUND,      :PangoAttrColor),
    (:PANGO_ATTR_BACKGROUND,      :PangoAttrColor),
    (:PANGO_ATTR_UNDERLINE,       :PangoAttrInt),
    (:PANGO_ATTR_STRIKETHROUGH,   :PangoAttrInt),
    (:PANGO_ATTR_RISE,            :PangoAttrInt),
    (:PANGO_ATTR_SHAPE,           :PangoAttrShape),
    (:PANGO_ATTR_SCALE,           :PangoAttrFloat),
    (:PANGO_ATTR_FALLBACK,        :PangoAttrFallback),
    (:PANGO_ATTR_LETTER_SPACING,  :PangoAttrInt),
    (:PANGO_ATTR_UNDERLINE_COLOR, :PangoAttrColor),
    (:PANGO_ATTR_ABSOLUTE_SIZE,   :PangoAttrSize),
    (:PANGO_ATTR_GRAVITY,         :PangoAttrInt),
    (:PANGO_ATTR_GRAVITY_HINT,    :PangoAttrInt)]

for (i, (attr, t)) in enumerate(pango_attrs)
    @eval begin
        const $attr = $i
    end
end

const PANGO_STYLE_NORMAL  = 0
const PANGO_STYLE_OBLIQUE = 1
const PANGO_STYLE_ITALIC  = 2

const PANGO_WEIGHT_THIN = 100
const PANGO_WEIGHT_ULTRALIGHT = 200
const PANGO_WEIGHT_LIGHT = 300
const PANGO_WEIGHT_BOOK = 380
const PANGO_WEIGHT_NORMAL = 400
const PANGO_WEIGHT_MEDIUM = 500
const PANGO_WEIGHT_SEMIBOLD = 600
const PANGO_WEIGHT_BOLD = 700
const PANGO_WEIGHT_ULTRABOLD = 800
const PANGO_WEIGHT_HEAVY = 900
const PANGO_WEIGHT_ULTRAHEAVY = 1000

# A Julia manifestation of a set of pango attributes
mutable struct PangoAttr
    rise::Maybe(Int)
    scale::Maybe(Float64)
    style::Maybe(Int)
    weight::Maybe(Int)
end

PangoAttr() = PangoAttr(nothing, nothing, nothing, nothing)

isempty(attr::PangoAttr) = all([getfield(attr, name) === nothing for name in fieldnames(PangoAttr)])

# Set an attribute in a PangoAttr
#
# Args:
#   attr: A PangoAttr to update.
#   attr_name: A pango attribute name (e.g., :PANGO_ATTR_RISE)
#   value: The value with which to update the attribute.
#
# Returns:
#   The attr.
function update_pango_attr(attr::PangoAttr, attr_name::Symbol, value)
    if attr_name == :PANGO_ATTR_RISE
        attr.rise = Int64(value)
    elseif attr_name == :PANGO_ATTR_SCALE
        attr.scale = value
    elseif attr_name == :PANGO_ATTR_STYLE
        attr.style = Int64(value)
    elseif attr_name == :PANGO_ATTR_WEIGHT
        attr.weight = Int64(value)
    end
    attr
end

# Unpack the first part of a pango attribute
#
# Args:
#   ptr: A pointer to a PangoAttribute
#   t: The type of the attribute (e.g. PangoAttrInt)
#
# Returns:
#   A tuple of the form (start_idx, end_idx, value)
#
function unpack_pango_attr(ptr::Ptr{Cvoid}, t::Symbol)
    ptr += sizeof(Ptr{Cvoid}) # skip `klass` pointer
    ptr = convert(Ptr{UInt32}, ptr)
    idx = unsafe_wrap(Array, ptr, (2,), own=false)
    ptr += 2 * sizeof(UInt32)
    ptr = convert(Ptr{Cvoid}, ptr)

    if t == :PangoAttrInt
        value = unpack_pango_int(ptr)
    elseif t == :PangoAttrFloat
        value = unpack_pango_float(ptr)
    else
        value = nothing
    end

    (idx[1], idx[2], value)
end

# Unpack a pango int attribute.
#
# Args:
#   ptr: A point to a PangoAttrInt plus sizeof(PangoAttribute)
#
# Returns:
#   And int value.
unpack_pango_int(ptr::Ptr{Cvoid}) = unsafe_wrap(Array, convert(Ptr{Int32}, ptr), (1,), own=false)[1]
unpack_pango_float(ptr::Ptr{Cvoid}) = unsafe_wrap(Array, convert(Ptr{Float64}, ptr), (1,), own=false)[1]

#function unpack_pango_size(ptr::Ptr{Cvoid})
    #ptr = convert(Ptr{Int32}, ptr)
    #size = point_to_array(ptr, (1,))[1]
    #ptr = convert(Ptr{UInt32}, ptr)
    #absolute = point_to_array(ptr, (1,))[1] & 0x1

    #println(size, absolute)
    #nothing
#end

# TODO: unpacking other attributes

# Unpack a list of pango attributes
#
# Args:
#   ptr: A pointer to a PangoAttrList
#
# Returns:
#   A list of the form [(start_idx, attribute), ...] in which the start_idx
#   values are increasing and the attribute is a set of attributes that
#   should be applied starting at that position.
#
function unpack_pango_attr_list(ptr::Ptr{Cvoid})
    attr_it = ccall((:pango_attr_list_get_iterator, libpango),
                    Ptr{Cvoid}, (Ptr{Cvoid},), ptr)

    # Alias some ugly C calls.
    attr_it_next = () -> ccall((:pango_attr_iterator_next, libpango),
                               Int32, (Ptr{Cvoid},), attr_it)

    attr_it_get = attr_name -> ccall((:pango_attr_iterator_get, libpango),
                                     Ptr{Cvoid}, (Ptr{Cvoid}, Int32),
                                     attr_it, eval(attr_name))

    attr_it_range = () -> begin
        start_idx = Array{Int32}(undef, 1)
        end_idx = Array{Int32}(undef, 1)
        ccall((:pango_attr_iterator_range, libpango),
              Cvoid, (Ptr{Cvoid}, Ptr{Int32}, Ptr{Int32}),
              attr_it, start_idx, end_idx)
        (start_idx[1], end_idx[1])
    end


    attrs = Array{Tuple{Int, PangoAttr}}(undef, 0)

    while attr_it_next() != 0
        attr = PangoAttr()
        local start_idx

        for (attr_name, attr_type) in pango_attrs
            c_attr = attr_it_get(attr_name)
            (start_idx, end_idx) = attr_it_range()

            if c_attr != C_NULL
                (_, _, value) = unpack_pango_attr(c_attr, attr_type)
                update_pango_attr(attr, attr_name, value)
            end
        end

        push!(attrs, (start_idx, attr))
    end

    ccall((:pango_attr_iterator_destroy, libpango),
          Cvoid, (Ptr{Cvoid},), attr_it)

  attrs
end

function pango_to_svg(text::AbstractString)
    # TODO: do c_stripped_text and c_attr_list need to be freed?
    c_stripped_text = Ref{Ptr{UInt8}}()
    c_attr_list = Ref{Ptr{Cvoid}}()

    output = IOBuffer()
    output_line = IOBuffer()

    textlines = split(text, "\n")
    carriage_shift = carriage_shift0 
    for (itextline,textline) in enumerate(textlines)
        ret = ccall((:pango_parse_markup, libpango),
                    Int32, (Cstring, Int32, UInt32, Ptr{Ptr{Cvoid}},
                            Ptr{Ptr{UInt8}}, Ptr{UInt32}, Ptr{Cvoid}),
                    textline, -1, 0, c_attr_list, c_stripped_text,
                    C_NULL, C_NULL)
        ret == 0 && error("Could not parse pango markup.")

        input = codeunits(unsafe_string(c_stripped_text[]))

        lastpos = 1
        baseline_shift = 0.0
        sup = sub = false
        open_tag = false

        for (idx, attr) in unpack_pango_attr_list(c_attr_list[])
            write(output_line, input[lastpos:idx])
            lastpos = idx + 1

            closing_tag = isempty(attr)

            open_tag && !closing_tag && write(output_line, "</tspan>")
 
            if closing_tag
                write(output_line, "</tspan>")
            else
                write(output_line, "<tspan")

                # pango doesn't know the real font size here it seems,
                # so ignore it's absolute rise and use a relative shift
                # that matches fontfallback's behavior
                if attr.rise !== nothing
                    if attr.rise<0
                        baseline_shift = +supsub_shift
                        sub = true
                    else
                        baseline_shift = -supsub_shift
                        sup = true
                    end
                    @printf(output_line, " dy=\"%0.1fem\"", baseline_shift)
                end

                if attr.scale !== nothing
                    @printf(output_line, " font-size=\"%0.4f%%\"", 100.0 * attr.scale)
                    baseline_shift *= attr.scale
                end

                if attr.style !== nothing
                    if attr.style == PANGO_STYLE_NORMAL
                        @printf(output_line, " font-style=\"%s\"", "normal")
                    elseif attr.style == PANGO_STYLE_OBLIQUE
                        @printf(output_line, " font-style=\"%s\"", "oblique")
                    elseif attr.style == PANGO_STYLE_ITALIC
                        @printf(output_line, " font-style=\"%s\"", "italic")
                    end
                end

                attr.weight === nothing || @printf(output_line,
                                                   " font-weight=\"%d\"",
                                                   attr.weight)

                write(output_line, ">")
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
