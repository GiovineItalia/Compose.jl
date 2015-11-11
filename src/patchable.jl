# The Patchable backend

import Patchwork
import Patchwork.Elem

# Names that can be easily imported
export Patchable

type Patchable <: Backend
    width::Length{:mm}
    height::Length{:mm}

    jsheader::Vector{AbstractString}
    jsmodules::Vector{(@compat Tuple{AbstractString, AbstractString})}
    clip_paths::Dict{ClipPrimitive, AbstractString}
    parent_stack::Vector
    property_stack::Vector
    function Patchable(width, height)
        width = size_measure(width)
        height = size_measure(height)
        new(
            width,
            height,
            String[],
            (@compat Tuple{AbstractString, AbstractString})[],
            Dict{ClipPrimitive, AbstractString}(),
            Elem[Elem(:svg, :svg)],
            Any[])
    end
end

vector_properties(img::Patchable) = if !isempty(img.property_stack)
    img.property_stack[end].vector_properties
end

immutable PatchablePropertyFrame
    has_scalar_properties::Bool
    vector_properties::Dict
end

function push_property_frame(img::Patchable, properties)
    applied_properties = Set{Type}() # Don't double apply the same property
    scalar_properties = Array(Property, 0)
    vector_props = Dict()
    for property in properties
        if !isrepeatable(property) && (typeof(property) in applied_properties)
            continue
        elseif isscalar(property)
            push!(scalar_properties, property)
            push!(applied_properties, typeof(property))
        else
            vector_props[typeof(property)] = property
        end
    end
    push!(img.property_stack, PatchablePropertyFrame(!isempty(scalar_properties), vector_props))
    if isempty(scalar_properties)
        return
    end
    # TODO: current_id???

    props = Dict()
    for p in scalar_properties
        draw!(img, p.primitives[1], props)
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
function draw(backend::Patchable, box::AbsoluteBox, units::UnitBox, t::Transform,
              form::Form)
    absform = Form([resolve(box, units, t, primitive)
                  for primitive in form.primitives])
    backend.parent_stack[end] <<= draw(backend, absform)
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
        if i > length(property.primitives)
            error("Vector of properties and vector of forms have different length")
        end
        draw!(img, property.primitives[i], props)
    end
    props
end

# Form Drawing
# ------------

function draw(img::Patchable, form::Form)
    acc = Array(Any, length(form.primitives))
    properties = vector_properties(img)

    for i in 1:length(form.primitives)
        elem = draw(img, form.primitives[i])
        if properties !== nothing && !isempty(properties)
            props = properties_at_index(img, properties, i)
            elem &= props
        end
        acc[i] = elem
    end
    acc
end


function draw(img::Patchable, prim::BitmapPrimitive)
    warn("Patchable backend does not yet support bitmap primitives")
    nothing
end

draw(img::Patchable, prim::CirclePrimitive) =
    Elem(:svg, :circle,
         cx=prim.center[1].value,
         cy=prim.center[2].value,
         r=prim.radius.value)

draw(img::Patchable, prim::CurvePrimitive) =
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

function draw(img::Patchable, prim::EllipsePrimitive)
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

function draw(img::Patchable, prim::LinePrimitive)
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

draw(img::Patchable, prim::PathPrimitive) =
    Elem(:svg, :path, d=svg_fmt_path_ops(prim.ops))

function draw(img::Patchable, prim::PolygonPrimitive)
     n = length(prim.points)
     if n <= 1; return; end

     Elem(:svg, :path, d=svg_fmt_path(prim.points, true) * " z")
end

function draw(img::Patchable, prim::ComplexPolygonPrimitive)
    Elem(:svg, :path, d=join(map(r -> svg_fmt_path(r, true), prim.rings), "") * " z")
end

function draw(img::Patchable, prim::RectanglePrimitive)
    width = max(prim.width.value, 0.01)
    height = max(prim.height.value, 0.01)

    Elem(:svg, :rect,
         x=prim.corner[1].value,
         y=prim.corner[2].value,
         width=width,
         height=height)
end

function draw(img::Patchable, prim::TextPrimitive)
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

# Property Primitives
# -------------------
function draw(img::Patchable, prop::Property)
    dict = Dict()
    for prim in prop.primitives
        draw!(img, prim, dict)
    end
    dict
end

function draw!(img::Patchable, prim::PropertyPrimitive, dict)
    item = draw(img, prim)
    if !is(item, nothing)
        k, v = item

        dict[k] = v
    end
end

function draw(img::Patchable, prim::ClipPrimitive)
    id = clip_path_id(img, prim)
    "clip-path", "url(#$id)"
end

draw(img::Patchable, prim::FillOpacityPrimitive) =
    :opacity, prim.value

function draw!(img::Patchable, prim::FillPrimitive, props)
    if isa(prim.color, TransparentColor)
        props[:fill] = svg_fmt_color(color(prim.color))
        props["fill-opacity"] = prim.color.alpha
    elseif isa(prim.color, Color)
        props[:fill] = svg_fmt_color(prim.color)
    else
        props[:fill] = "none"
    end
end

draw(img::Patchable, prim::FontPrimitive) =
    "font-family", escape_string(prim.family)

draw(img::Patchable, prim::FontSizePrimitive) =
    "font-size", prim.value.value

function draw(img::Patchable, prim::JSCallPrimitive)
    nothing
end

function draw(img::Patchable, prim::JSIncludePrimitive)
    push!(img.jsheader, prim.value)
    if prim.jsmodule != nothing
        push!(img.jsmodules, prim.jsmodule)
    end
    nothing
end

draw(img::Patchable, prim::LineWidthPrimitive) =
    "stroke-width", prim.value.value

draw(img::Patchable, prim::SVGAttributePrimitive) = nothing
    #prim.attribute, escape_string(prim.value)

draw(img::Patchable, prim::SVGClassPrimitive) =
    :class, escape_string(prim.value)

draw(img::Patchable, prim::SVGIDPrimitive) =
    :id, escape_string(prim.value)

function draw(img::Patchable, prim::StrokeDashPrimitive)
    if isempty(prim.value)
        "stroke-dasharray", :none
    else
        "stroke-dasharray", join([v.value for v in prim.value], ',')
    end
end

draw(img::Patchable, prim::StrokeLineCapPrimitive) =
    "stroke-linecap", svg_fmt_linecap(prim.value)

draw(img::Patchable, prim::StrokeLineJoinPrimitive) =
    "stroke-linejoin", svg_fmt_linejoin(prim.value)

draw(img::Patchable, prim::StrokeOpacityPrimitive) =
    "stroke-opacity", prim.value

function draw!(img::Patchable, prim::StrokePrimitive, dict)
    if isa(prim.color, TransparentColor)
        dict[:stroke] = svg_fmt_color(color(prim.color))
        dict["stroke-opacity"] = prim.color.alpha
    else
        dict[:stroke] = svg_fmt_color(prim.color)
    end
end

draw(img::Patchable, prim::VisiblePrimitive) =
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
