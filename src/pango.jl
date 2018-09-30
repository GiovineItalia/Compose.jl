# Estimation of text extents using pango.

# Cairo text backend
const CAIRO_FONT_TYPE_TOY = 0
const CAIRO_FONT_TYPE_FT = 1
const CAIRO_FONT_TYPE_WIN32 = 2
const CAIRO_FONT_TYPE_QUARTZ = 3
const CAIRO_FONT_TYPE_USER = 4

# Mirroring a #define in the pango header.
const PANGO_SCALE = 1024.0

pango_fmt_float(x::Float64) = @sprintf("%0.4f", x)

# Thin wrapper for a pango_layout object.
mutable struct PangoLayout
    layout::Ptr{Cvoid}
end

function PangoLayout()
    layout = Pango.pango_layout_new(pango_cairo_ctx)
    # TODO: finalizer?

    PangoLayout(layout)
end

# Set the layout's font.
function pango_set_font(pangolayout::PangoLayout, family::AbstractString, pts::Number)
    family = match_font(family, pts)

    desc = @sprintf("%s %fpx", family, size)
    fd = Pango.pango_font_description_from_string(desc)

    Pango.pango_layout_set_font_description(pangolayout.layout, fd)
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
    textarray = convert(Vector{UInt8}, convert(String, text))
    Pango.pango_layout_set_markup(pangolayout.layout, textarray, length(textarray))

    extents = Array{Int32}(undef, 4)
    Pango.pango_layout_get_extents(pangolayout.layout, extents, C_NULL)

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
    Pango.pango_set_font(pangolayout::PangoLayout, font_family, pts)
    max_width  = 0mm
    max_height = 0mm
    for text in texts
        (width, height) = Pango.pango_text_extents(pangolayout::PangoLayout, text)
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
    Pango.pango_set_font(pangolayout::PangoLayout, font_family, pts)
    return [Pango.pango_text_extents(pangolayout::PangoLayout, text) for text in texts]
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
    idx = unsafe_wrap(Array, ptr, (2,), false)
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
unpack_pango_int(ptr::Ptr{Cvoid}) = unsafe_wrap(Array, convert(Ptr{Int32}, ptr), (1,), false)[1]
unpack_pango_float(ptr::Ptr{Cvoid}) = unsafe_wrap(Array, convert(Ptr{Float64}, ptr), (1,), false)[1]

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
    attr_it = Pango.pango_attr_list_get_iterator(ptr)

    # Alias some ugly C calls.
    attr_it_next = () -> Pango.pango_attr_iterator_next(attr_it)

    attr_it_get = attr_name -> Pango.pango_attr_iterator_get(attr_it, eval(attr_name))

    attr_it_range = () -> begin
        start_idx = Array{Int32}(undef, 1)
        end_idx = Array{Int32}(undef, 1)
        Pango.pango_attr_iterator_range(attr_it, start_idx, end_idx)
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

    Pango.pango_attr_iterator_destroy(attr_it)

  attrs
end


function pango_to_svg(text::AbstractString)
    c_stripped_text = Ref{Cstring}()
    c_attr_list = Ref{Ptr{Cvoid}}()

    ret = Pango.pango_parse_markup(text, -1, 0, c_attr_list, c_stripped_text, C_NULL, C_NULL)

    ret == 0 && error("Could not parse pango markup.")

    # TODO: do c_stripped_text and c_attr_list need to be freed?

    text = unsafe_string(c_stripped_text[])

    last_idx = 1
    open_tag = false
    baseline_shift = 0.0

    tagged_text = sprint() do io
        for (idx, attr) in unpack_pango_attr_list(c_attr_list[])
            write(io, text[last_idx:idx])
            last_idx = idx + 1

            open_tag && write(io, "</tspan>")

            if isempty(attr) && baseline_shift == 0.0
                open_tag = false
                continue
            end

            open_tag = true

            write(io, "<tspan style=\"dominant-baseline:inherit\"")

            # "baseline-shift" is not currently supported Firefox or IE.
            # if !(attr.rise === nothing)
            #     @printf(io, " baseline-shift=\"%s\"",
            #             fmt_float(((attr.rise / PANGO_SCALE)pt).abs))
            # end

            if !(attr.rise === nothing)
                bs = -((attr.rise / PANGO_SCALE)pt).value
                @printf(io, " dy=\"%s\"", Pango.pango_fmt_float(bs))
                baseline_shift = bs
            elseif baseline_shift != 0.0
                @printf(io, " dy=\"%s\"", Pango.pango_fmt_float(-baseline_shift))
                baseline_shift = 0.0
            end

            if !(attr.scale === nothing)
                @printf(io, " font-size=\"%s%%\"",
                        Pango.pango_fmt_float(100.0 * attr.scale))
                baseline_shift *= attr.scale
            end

            if !(attr.style === nothing)
                if attr.style == PANGO_STYLE_NORMAL
                    @printf(io, " font-style=\"%s\"", "normal")
                elseif attr.style == PANGO_STYLE_OBLIQUE
                    @printf(io, " font-style=\"%s\"", "oblique")
                elseif attr.style == PANGO_STYLE_ITALIC
                    @printf(io, " font-style=\"%s\"", "italic")
                end
            end

            attr.weight === nothing || @printf(io, " font-weight=\"%d\"", attr.weight)

            write(io, ">")
        end

        write(io, text[last_idx:end])

        open_tag && write(io, "</tspan>")
    end

    tagged_text
end
