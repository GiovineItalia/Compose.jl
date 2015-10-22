# The Patchable backend

import Patchwork
import Patchwork.Elem

# Names that can be easily imported
export Patchable

type Patchable <: Backend
    width::Length{:mm}
    height::Length{:mm}

    jsheader::Vector{AbstractString}
    jsmodules::Vector{Tuple{AbstractString, AbstractString}}
    clip_paths::Dict{Clip, AbstractString}
    parent_stack::Vector
    property_stack::Vector
    function Patchable(width, height)
        width = size_measure(width)
        height = size_measure(height)
        new(
            width,
            height,
            AbstractString[],
            Tuple{AbstractString, AbstractString}[],
            Dict{Clip, AbstractString}(),
            Elem[Elem(:svg, :svg)],
            Any[])
    end
end

vector_properties(img::Patchable) = if !isempty(img.property_stack)
    img.property_stack[end].vector_properties
end

type PatchablePropertyFrame
    has_scalar_properties::Bool
    vector_properties::Dict
end

function add_to_frame{P<:Property}(img::Patchable, property::P, frame, scalar_properties, applied_properties)
    push!(scalar_properties, property)
    push!(applied_properties, P)
    frame.has_scalar_properties = true
end

function add_to_frame{P<:Property}(img::Patchable, property::AbstractArray{P}, frame, scalar_properties, applied_properties)
    frame.vector_properties[P] = property
    img.vector_properties[P] = property
end

function push_property_frame(img::Patchable, properties)
    applied_properties = Set{Type}() # Don't double apply the same property
    scalar_properties = Array(PropertyNode, 0)
    vector_props = Dict()
    frame = PatchablePropertyFrame(false, vector_props)
    for property in properties
        if !isrepeatable(proptype(property)) && (proptype(property) in applied_properties)
            continue
        else
            add_to_frame(img, property, frame, scalar_properties, applied_properties)
        end
    end
    push!(img.property_stack, frame)
    if isempty(scalar_properties)
        return
    end
    # TODO: current_id???

    props = Dict()
    for p in scalar_properties
        make_prop!(img, p, props)
    end
    push!(img.parent_stack, Elem(:svg, :g) & props)
end

function pop_property_frame(img::Patchable)
    frame = pop!(img.property_stack)
    if frame.has_scalar_properties
        last = pop!(img.parent_stack)
        img.parent_stack[end] <<= last
    end
end

iswithjs(::Patchable) = false
iswithousjs(::Patchable) = true

clip_path_id(img, path) =
   if (haskey(img.clip_paths, path))
       return img.clip_paths[path]
   else
       id = string("clippath-", length(img.clip_paths) + 1)
       img.clip_paths[path] = id
       return id
   end

# Specialize absolute form drawing method for Patchwork backend
function draw(backend::Patchable, box::AbsoluteBox, units::UnitBox, t::Transform, prims::FormNode)
    absform = [resolve(box, units, t, primitive)
                  for prim in prims]
    backend.parent_stack[end] <<= make_elem(backend, absform)
end

function finish(img::Patchable)
    root = img.parent_stack[end]
    @assert isa(root, Elem{:svg, :svg})
    root &= @compat Dict(
        :width=>string(img.width),
        :height=>string(img.height),
        :viewBox=>string("0 0 ", img.width.value, ' ', img.height.value),
        :stroke=>svg_fmt_color(default_stroke_color),
        :fill=>svg_fmt_color(default_fill_color)
    )

    root &= @compat Dict("stroke-width" => svg_fmt_float(default_line_width.value),
                 "font-size" => svg_fmt_float(default_font_size.value))
    if !isempty(img.clip_paths)
        defs = Elem(:svg, :defs)
        for (path, id) in img.clip_paths
            defs <<= Elem(:svg, :clipPath,
                          Elem(:svg, :path, d=svg_fmt_path(path.points)),
                          id=id)
        end
        root = root << defs
    end
    img.parent_stack[end] = Elem(:svg, :svg)
    root
end

root_box(img::Patchable) =
    BoundingBox(0.0mm, 0.0mm, img.width, img.height)

init_context(::Patchable, ::Context) = Elem(:svg, :g)

typealias SVGPart @compat(Union{Elem, Dict, (@compat Void)})

function properties_at_index(img, prop_vecs, i)
    props = Dict()
    for (proptype, property) in prop_vecs
        if i > length(property)
            error("Vector of properties and vector of forms have different length")
        end
        make_prop!(img, property[i], props)
    end
    props
end

# Form Drawing
# ------------

function draw{F<:Form}(img::Patchable, form::F)
    acc = Array(Any, length(form))
    properties = vector_properties(img)

    elem = make_elem(img, form)
    if properties !== nothing && !isempty(properties)
        props = properties_at_index(img, properties, 1)
        elem &= props
    end
    img.parent_stack[end] <<= elem
end

function draw{F<:Form}(img::Patchable, form::AbstractArray{F})
    img.parent_stack[end] <<= make_elems(img, form)
end

function make_elems(img::Patchable, form)
    acc = Array(Any, length(form))
    properties = vector_properties(img)

    for i in 1:length(form)
        elem = make_elem(img, form[i])
        if properties !== nothing && !isempty(properties)
            props = properties_at_index(img, properties, i)
            elem &= props
        end
        acc[i] = elem
    end
    acc
end

function make_elem(img::Patchable, prim::Bitmap)
    warn("Patchable backend does not yet support bitmap primitives")
    nothing
end

make_elem(img::Patchable, prim::Circle) =
    Elem(:svg, :circle,
         cx=prim.center[1].value,
         cy=prim.center[2].value,
         r=prim.radius.value)

make_elem(img::Patchable, prim::Curve) =
    Elem(:svg, :path,
         fill="none",
         path=string("M"
                     , prim.anchor0[1].value, ','
                     , prim.anchor0[2].value, "C"
                     , prim.ctrl0[1].value, ','
                     , prim.ctrl0[2].value, ' '
                     , prim.ctrl1[1].value, ','
                     , prim.ctrl1[2].value, ' '
                     , prim.anchor1[1].value, ','
                     , prim.anchor1[2].value))

function make_elem(img::Patchable, prim::Ellipse)
    cx = prim.center[1].value
    cy = prim.center[2].value
    rx = sqrt((prim.x_point[1].value - cx)^2 +
              (prim.x_point[2].value - cy)^2)
    ry = sqrt((prim.y_point[1].value - cx)^2 +
              (prim.y_point[2].value - cy)^2)
    theta = rad2deg(atan2(prim.x_point[2].value - cy,
                          prim.x_point[1].value - cx))

    if !all(isfinite([cx, cy, rx, ry, theta]))
        return nothing
    end
    el = Elem(:svg, :ellipse,
              cx=cx, cy=cy, rx=rx, ry=ry)
    if abs(theta) > 1e-4
        el = el & @compat Dict(:transform => string("rotate(",
                                        svg_fmt_float(theta), ' ',
                                        svg_fmt_float(cx), ' ',
                                        svg_fmt_float(cy), ')'))
    end
    el
end

function svg_fmt_path(points::Vector, bridge_gaps::Bool=false)
    io=IOBuffer()
    print_svg_path(io, points, bridge_gaps)
    takebuf_string(io)
end

function make_elem(img::Patchable, prim::Line)
    n = length(prim.points)
    if n <= 1; return; end

    Elem(:svg, :path, fill="none", d=svg_fmt_path(prim.points, true))
end

function svg_fmt_path_ops(ops)
    io = IOBuffer()
    for op in ops
        svg_print_path_op(io, op)
    end
    takebuf_string(io)
end

make_elem(img::Patchable, prim::Path) =
    Elem(:svg, :path, d=svg_fmt_path_ops(prim.ops))

function make_elem(img::Patchable, prim::Polygon)
     n = length(prim.points)
     if n <= 1; return; end

     Elem(:svg, :path, d=svg_fmt_path(prim.points, true) * " z")
end

function make_elem(img::Patchable, prim::ComplexPolygon)
    Elem(:svg, :path, d=join(map(r -> svg_fmt_path(r, true), prim.rings), "") * " z")
end

function make_elem(img::Patchable, prim::Rectangle)
    width = max(prim.width.value, 0.01)
    height = max(prim.height.value, 0.01)

    Elem(:svg, :rect,
         x=prim.corner[1].value,
         y=prim.corner[2].value,
         width=width,
         height=height)
end

function make_elem(img::Patchable, prim::Text)
    el = pango_to_elems(prim.value) & @compat Dict(
            :x=>prim.position[1].value,
            :y=>prim.position[2].value)
    if is(prim.halign, hcenter)
        el &= @compat Dict("text-anchor" => "middle")
    elseif is(prim.halign, hright)
        el &= @compat Dict("text-anchor" => "end")
    end
    if is(prim.valign, vcenter)
        el &= @compat Dict(:dy=>"0.35em")
    elseif is(prim.halign, vtop)
        el &= @compat Dict(:dy=>"0.6mm")
    end

    if abs(prim.rot.theta) > 1e-4
        el &= @compat Dict(:transform => string("rotate(",
                                     rad2deg(prim.rot.theta), ' ',
                                     svg_fmt_float(prim.rot.offset[1].value), ' ',
                                     svg_fmt_float(prim.rot.offset[2].value), ')'))
     end
     el

end

# Property s
# -------------------
function make_prop{P<:Property}(img::Patchable, prop::AbstractArray{P, Property})
    dict = Dict()
    for prim in prop
        make_prop!(img, prim, dict)
    end
    dict
end

function make_prop!(img::Patchable, prim::Property, dict)
    item = make_prop(img, prim)
    if !is(item, nothing)
        k, v = item

        dict[k] = v
    end
end

function make_prop(img::Patchable, prim::Clip)
    id = clip_path_id(img, prim)
    "clip-path", "url(#$id)"
end

make_prop(img::Patchable, prim::FillOpacity) =
    :opacity, prim.value

function make_prop!(img::Patchable, prim::Fill, props)
    if isa(prim.color, TransparentColor)
        props[:fill] = svg_fmt_color(color(prim.color))
        props["fill-opacity"] = prim.color.alpha
    elseif isa(prim.color, Color)
        props[:fill] = svg_fmt_color(prim.color)
    else
        props[:fill] = "none"
    end
end

make_prop(img::Patchable, prim::Font) =
    "font-family", escape_string(prim.family)

make_prop(img::Patchable, prim::FontSize) =
    "font-size", prim.value.value

function make_prop(img::Patchable, prim::JSCall)
    nothing
end

function make_prop(img::Patchable, prim::JSInclude)
    push!(img.jsheader, prim.value)
    if prim.jsmodule != nothing
        push!(img.jsmodules, prim.jsmodule)
    end
    nothing
end

make_prop(img::Patchable, prim::LineWidth) =
    "stroke-width", prim.value.value

make_prop(img::Patchable, prim::SVGAttribute) = nothing
    #prim.attribute, escape_string(prim.value)

make_prop(img::Patchable, prim::SVGClass) =
    :class, escape_string(prim.value)

make_prop(img::Patchable, prim::SVGID) =
    :id, escape_string(prim.value)

function make_prop(img::Patchable, prim::StrokeDash)
    if isempty(prim.value)
        "stroke-dasharray", :none
    else
        "stroke-dasharray", join([v.value for v in prim.value], ',')
    end
end

make_prop(img::Patchable, prim::StrokeLineCap) =
    "stroke-linecap", svg_fmt_linecap(prim.value)

make_prop(img::Patchable, prim::StrokeLineJoin) =
    "stroke-linejoin", svg_fmt_linejoin(prim.value)

make_prop(img::Patchable, prim::StrokeOpacity) =
    "stroke-opacity", prim.value

function make_prop!(img::Patchable, prim::Stroke, dict)
    if isa(prim.color, TransparentColor)
        dict[:stroke] = svg_fmt_color(color(prim.color))
        dict["stroke-opacity"] = prim.color.alpha
    else
        dict[:stroke] = svg_fmt_color(prim.color)
    end
end

make_prop(img::Patchable, prim::Visible) =
    :visibility, prim.value ? "visible" : "hidden"

# Pango markup to Patchwork
function pango_to_elems(text::AbstractString)
    pat = r"<(/?)\s*([^>]*)\s*>"
    input = text
    output = Elem[Elem(:svg, :text)] # Stack
    lastpos = 1

    baseline_shift = 0.0
    open_tag = false

    for mat in eachmatch(pat, text)
        txt = input[lastpos:mat.offset-1]
        if length(txt) > 0
            output[end] <<= Patchwork.Text(txt)
        end

        closing_tag = mat.captures[1] == "/"

        if open_tag && !closing_tag
            el = pop!(output)
            output[end] <<= el
        end

        if mat.captures[2] == "sup"
            if mat.captures[1] == "/"
                el = pop!(output)
                output[end] <<= el
            else
                el = Elem(:svg, :tspan,
                         style=(@compat Dict("dominant-baseline" => :inherit)),
                         dy="-0.6em") & @compat Dict("font-size" => "83%")
                push!(output, el)
                baseline_shift = -0.6 * 0.83
            end
        elseif mat.captures[2] == "sub"
            if mat.captures[1] == "/"
                el = pop!(output)
                output[end] <<= el
            else
                el = Elem(:svg, :tspan,
                          style=(@compat Dict("dominant-baseline" => :inherit)),
                          dy="0.6em") & @compat Dict("font-size" => "83%")
                push!(output, el)
                baseline_shift = 0.6 * 0.83
            end
        elseif mat.captures[2] == "i"
            if mat.captures[1] == "/"
                el = pop!(output)
                output[end] <<= el
            else
                el = Elem(:svg, :tspan,
                          style = @compat Dict("dominant-baseline" => :inherit)) &
                                    @compat Dict("font-style"=>"italic")
                push!(output, el)
            end
        elseif mat.captures[2] == "b"
            if mat.captures[1] == "/"
                el = pop!(output)
                output[end] <<= el
            else
                el = Elem(:svg, :tspan,
                          style=@compat Dict("dominant-baseline" => :inherit)) &
                                    @compat Dict("font-style"=>"bold")
                push!(output, el)
            end
        end

        if closing_tag && baseline_shift != 0.0
            el = Elem(:svg, :tspan,
                      dy="$(-baseline_shift)em")
            push!(output, el)
            baseline_shift = 0.0
            open_tag = true
        end

        lastpos = mat.offset + length(mat.match)
    end
    txt = input[lastpos:end]
    if length(txt) > 0
        output[end] <<= Patchwork.Text(string(txt))
    end
    if open_tag
        el = pop!(output)
        output[end] <<= el
    end
    output[1]
end

# writemime for signals
if isinstalled("Reactive")

    import Base: writemime
    import Reactive: Signal, lift

    if isdefined(Main, :IJulia)
        import IJulia: metadata
        metadata{T <: ComposeNode}(::Signal{T}) = Dict()
    end

    function writemime{T <: ComposeNode}(io::IO, m::MIME"text/html", ctx::Signal{T})
        writemime(io, m, lift(c -> draw(
            Patchable(
                Compose.default_graphic_width,
                Compose.default_graphic_height
            ), c), ctx))
    end
end
