# The Patchable backend
import Patchwork
import Patchwork.Elem
# Names that can be easily imported
export Patchable

type Patchable <: Backend
    width::Float64
    height::Float64

    jsheader::Vector{String}
    jsmodules::Vector{(String, String)}
    clip_paths::Dict{ClipPrimitive, String}
    vector_properties::Vector
    function Patchable(width, height, absolute_elems=Elem[])
        width = size_measure(width)
        height = size_measure(height)
        if !isabsolute(width) || !isabsolute(height)
            error("SVG image size must be specified in absolute units.")
        end
        new(width.abs,
            height.abs,
            String[],
            (String, String)[],
            Dict{ClipPrimitive, String}(),
            Any[])
    end
end


iswithjs(::Patchable) = false
iswithousjs(::Patchable) = true

vector_properties(img::Patchable) = if !isempty(img.vector_properties)
    img.vector_properties[end]
end

clip_path_id(img, path) =
   if (haskey(img.clip_paths, path))
       return img.clip_paths[path]
   else
       id = string("clippath-", length(img.clip_paths) + 1)
       img.clip_paths[path] = id
       return id
   end

# Declare that we need the recursive drawing procedure
function draw(img::Patchable, root::Context)
    root = Elem(:svg, :svg,
         draw_recursive(img, root),
         width=string(img.width, "mm"),
         height=string(img.height, "mm"),
         viewBox=string("0 0 ", img.width, ' ', img.height),
         stroke=svg_fmt_color(default_stroke_color),
         fill=svg_fmt_color(default_fill_color))

    root &= ["stroke-width" => svg_fmt_float(default_line_width.abs),
             "font-size" => svg_fmt_float(default_font_size.abs)]

    if !isempty(img.clip_paths)
        defs = Elem(:svg, :defs)
        for (path, id) in img.clip_paths
            defs <<= Elem(:svg, :clipPath,
                          Elem(:svg, :path, d=svg_fmt_path(path.points)),
                          id=id)
        end
        root = root << defs
    end
    root
end

root_box(img::Patchable) =
    AbsoluteBoundingBox(0.0, 0.0, img.width, img.height)

init_context(::Patchable, ::Context) = nothing

typealias SVGPart Union(Elem, Dict, Nothing)

addto(::Patchable, acc::Nothing, child::Nothing) = nothing
addto(::Patchable, acc::Nothing, child::SVGPart) = child
addto(::Patchable, acc::SVGPart, child::Nothing) = acc
addto(::Patchable, acc::Elem{:svg, :g}, child::Elem) = acc << child

addto(p::Patchable, acc::Nothing, child::Array) = addto(p, Elem(:svg, :g), child)
function addto(p::Patchable, acc::Elem{:svg, :g}, child::Array)
    for i=1:length(child)
        @inbounds acc = addto(p, acc, child[i])
    end
    acc
end

addto(::Patchable, acc::Elem, child::Elem) = Elem(:svg, :g, acc, child)
addto(::Patchable, acc::Elem, child::Dict) = acc & child
addto(::Patchable, acc::Nothing, child::Dict) = Elem(:svg, :g) & child

function push_property_frame(img::Patchable, vector_props)
    push!(img.vector_properties, vector_props)
end

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

function pop_property_frame(img::Patchable)
    pop!(img.vector_properties)
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
            elem = addto(img, elem, props)
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
         cx=prim.center.x.abs,
         cy=prim.center.y.abs,
         r=prim.radius.abs)

draw(img::Patchable, prim::CurvePrimitive) =
    Elem(:svg, :path,
         fill="none",
         path=string("M" 
                     , prim.anchor0.x.abs, ','
                     , prim.anchor0.y.abs, "C"
                     , prim.ctrl0.x.abs, ','
                     , prim.ctrl0.y.abs, ' '
                     , prim.ctrl1.x.abs, ','
                     , prim.ctrl1.y.abs, ' '
                     , prim.anchor1.x.abs, ','
                     , prim.anchor1.y.abs))

function draw(img::Patchable, prim::EllipsePrimitive)
    cx = prim.center.x.abs
    cy = prim.center.y.abs
    rx = sqrt((prim.x_point.x.abs - cx)^2 +
              (prim.x_point.y.abs - cy)^2)
    ry = sqrt((prim.y_point.x.abs - cx)^2 +
              (prim.y_point.y.abs - cy)^2)
    theta = rad2deg(atan2(prim.x_point.y.abs - cy,
                          prim.x_point.x.abs - cx))

    if !all(isfinite([cx, cy, rx, ry, theta]))
        return nothing
    end
    el = Elem(:svg, :ellipse,
              cx=cx, cy=cy, rx=rx, ry=ry)
    if abs(theta) > 1e-4
        el = el & [:transform => string("rotate(",
                                        svg_fmt_float(theta), ' ',
                                        svg_fmt_float(cx), ' ',
                                        svg_fmt_float(cy), ')')]
    end
    el
end

function svg_fmt_path(points::Vector{Point}, bridge_gaps::Bool=false)
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

function draw(img::Patchable, prim::RectanglePrimitive)
    width = max(prim.width.abs, 0.01)
    height = max(prim.height.abs, 0.01)

    Elem(:svg, :rect,
         x=prim.corner.x.abs,
         y=prim.corner.y.abs,
         width=width,
         height=height)
end

function draw(img::Patchable, prim::TextPrimitive)
    el = Elem(:svg, :text, prim.value,
              x=prim.position.x.abs,
              y=prim.position.y.abs)
    if is(prim.halign, hcenter)
        el &= ["text-anchor" => "middle"]
    elseif is(prim.halign, hright)
        el &= ["text-anchor" => "end"]
    end
    if is(prim.valign, vcenter)
        el &= [:dy=>"0.35em"]
    elseif is(prim.halign, vtop)
        el &= [:dy=>"0.6mm"]
    end

    if abs(prim.rot.theta) > 1e-4
        el &= [:transform => string("rotate(",
                                     rad2deg(prim.rot.theta), ' ',
                                     svg_fmt_float(prim.rot.offset.x.abs), ' ',
                                     svg_fmt_float(prim.rot.offset.y.abs), ')')]
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
    if isa(prim.color, AlphaColorValue)
        props[:fill] = svg_fmt_color(prim.color.c)
        props["fill-opacity"] = prim.color.alpha
    elseif isa(prim.color, ColorValue)
        props[:fill] = svg_fmt_color(prim.color)
    else
        props[:fill] = "none"
    end
end

draw(img::Patchable, prim::FontPrimitive) =
    "font-family", escape_string(prim.family)

draw(img::Patchable, prim::FontSizePrimitive) =
    "font-size", prim.value.abs

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
    "stroke-width", prim.value.abs

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
        "stroke-dasharray", join([v.abs for v in prim.value], ',')
    end
end

draw(img::Patchable, prim::StrokeLineCapPrimitive) =
    "stroke-linecap", svg_fmt_linecap(prim.value)

draw(img::Patchable, prim::StrokeLineJoinPrimitive) =
    "stroke-linejoin", svg_fmt_linejoin(prim.value)

draw(img::Patchable, prim::StrokeOpacityPrimitive) =
    "stroke-opacity", prim.value

function draw!(img::Patchable, prim::StrokePrimitive, dict)
    if isa(prim.color, AlphaColorValue)
        dict[:stroke] = svg_fmt_color(prim.color.c)
        dict["stroke-opacity"] = property.color.value
    else
        dict[:stroke] = svg_fmt_color(prim.color)
    end
end

draw(img::Patchable, prim::VisiblePrimitive) =
    :visibility, prim.value ? "visible" : "hidden"

