# A container is a node in the tree that can have Forms, Properties, or other
# Containers as children.
abstract type Container <: ComposeNode end


# The basic Container which defines a coordinate transform for its children.
mutable struct Context <: Container
    # Bounding box relative to the parent's coordinates
    box::BoundingBox

    units::Union{UnitBox, Nothing}
    rot::Union{Rotation, Nothing}
    mir::Union{Mirror, Nothing}
    shear::Union{Shear, Nothing}

    # Container children
    container_children::List{Container}
    form_children::List{Form}
    property_children::List{Property}

    order::Int
    clip::Bool

    withjs::Bool
    withoutjs::Bool
    raster::Bool

    minwidth::Maybe(Float64)
    minheight::Maybe(Float64)

    # A field that can be used by layouts to indicate that one configuration is
    # preferable to another.
    penalty::Float64

    tag::Symbol

    function Context(box, units, rotation, mirror, shear,
                     container_children, form_children, property_children,
                     order, clip, withjs, withoutjs, raster, minwidth, minheight, penalty, tag)
        if isa(minwidth, AbsoluteLength)
            minwidth = minwidth.value
        end

        if isa(minheight, AbsoluteLength)
            minheight = minheight.value
        end

        return new(box, units, rotation, mirror, shear,
                   container_children, form_children, property_children,
                   order, clip, withjs, withoutjs, raster, minwidth, minheight, penalty, tag)
    end
end

Context(x0=0.0w, y0=0.0h, width=1.0w, height=1.0h;
            units=nothing,
            rotation=nothing,
            mirror=nothing,
            shear=nothing,
            order=0,
            clip=false,
            withjs=false,
            withoutjs=false,
            raster=false,
            minwidth=nothing,
            minheight=nothing,
            penalty=0.0,
            tag=empty_tag) =
        Context(BoundingBox(x0, y0, width, height), units, rotation, mirror,
            ListNull{ComposeNode}(),
            order, clip, withjs, withoutjs, raster, minwidth, minheight, penalty, tag)

Context(ctx::Context) = Context(ctx.box, ctx.units, ctx.rot, ctx.mir, ctx.shear,
            ctx.container_children, ctx.form_children, ctx.property_children,
            ctx.order, ctx.clip, ctx.withjs, ctx.withoutjs, ctx.raster,
            ctx.minwidth, ctx.minheight, ctx.penalty, ctx.tag)

"""
    context(x0=0.0w, y0=0.0h, width=1.0w, height=1.0h;
            units=nothing,
            rotation=nothing, mirror=nothing, shear=nothing,
            withjs=false, withoutjs=false,
            minwidth=nothing, minheight=nothing,
            order=0, clip=false, raster=false) -> Context

Create a node in the tree structure that defines a graphic.  Use
[`compose`](@ref) to connect child nodes to a parent node.  See also
[`Rotation`](@ref), [`Mirror`](@ref), and [`Shear`](@ref).

# Arguments
- `units`: the coordinate system for the context, defined by a [`UnitBox`](@ref).
- `order`: the Z-order of this context relative to its siblings.
- `clip`:  clip children of the canvas by its bounding box if true.
- `withjs`: ignore this context and everything under it if we are not drawing to the SVGJS backend.
- `withoutjs`: ignore this context if we *are* drawing on the SVGJS backend.
- `raster`: if possible, render this subtree as a bitmap. This requires the Cairo. If Cairo isn't available, the default rendering is used.
- `minwidth` and `minheight`: the minimum size needed to be drawn correctly, in millimeters.
"""
context(x0=0.0w, y0=0.0h, width=1.0w, height=1.0h;
            units=nothing,
            rotation=nothing,
            mirror=nothing,
            shear=nothing,
            order=0,
            clip=false,
            withjs=false,
            withoutjs=false,
            raster=false,
            minwidth=nothing,
            minheight=nothing,
            penalty=0.0,
            tag=empty_tag) =
        Context(BoundingBox(x_measure(x0), y_measure(y0), x_measure(width), y_measure(height)),
            units, rotation, mirror, shear,
            ListNull{Container}(), ListNull{Form}(), ListNull{Property}(),
            order, clip, withjs, withoutjs, raster, minwidth, minheight, penalty, tag)

children(ctx::Context) = Iterators.flatten((ctx.container_children, ctx.form_children, ctx.property_children))

# Updating context fields

set_units!(ctx::Context, units::UnitBox) =
    ctx.units = units

copy(ctx::Context) = Context(ctx)
iswithjs(ctx::Container) = ctx.withjs
iswithoutjs(ctx::Container) = ctx.withoutjs
order(cont::Container) = cont.order
minwidth(cont::Container) = cont.minwidth
minheight(cont::Container) = cont.minheight

# Normalize cx or cy coordinate to a number [0,1] according to the given
# unit box.
function cx_proportion(from::Measure,units::UnitBox)
    cux0 = units.x0
    cuw = units.width
    if cux0 == nothing
        cux0 = 0.0
        cuw = 1.0
    end
    ret_cx_proportion = from.cx != measure_nil ? (from.cx-cux0)/cuw : 0.0
    if !isfinite(ret_cx_proportion)
        ret_cx_proportion = 0.0
    end
    ret_cx_proportion
end

function cy_proportion(from::Measure,units::UnitBox)
    cuy0 = units.x0
    cuh = units.width
    if cuy0 == nothing
        cuy0 = 0.0
        cuh = 1.0
    end
    ret_cy_proportion  = from.cy != measure_nil ? (from.cy-cuy0)/cuh : 0.0
    if !isfinite(ret_cy_proportion)
        ret_cy_proportion = 0.0
    end
    ret_cy_proportion
end

function transformcoordinates(from::Measure, ctx::Context)
    Measure(;abs = from.abs) +
            (from.cw + cx_proportion(from,ctx.units))*ctx.box.width +
            (from.ch + cy_proportion(from,ctx.units))*ctx.box.height
end

function boundingbox(c::Context,linewidth::Measure=default_line_width,
                     font::AbstractString=default_font_family,
                     fontsize::Measure=default_font_size,
                     parent_abs_width = nothing,
                     parent_abs_height = nothing)
    for child in c.property_children
        for p in child.primitives
            if isa(p, LineWidthPrimitive)
                linewidth = p.value
            elseif isa(p, FontSizePrimitive)
                fontsize = p.value
            elseif isa(p, FontPrimitive)
                font = p.family
            end
        end
    end

    c_abs_width = c.box.width.abs
    if !iszero(c.box.width.cx) || !iszero(c.box.width.cy) ||
       !iszero(c.box.width.cw) || !iszero(c.box.width.ch)
        if parent_abs_width == nothing || parent_abs_height == nothing
            c_abs_width = nothing
        else
            c_abs_width = parent_abs_width*(c.box.width.cw + cx_proportion(c.box.width,c.units)) +
                parent_abs_height*(c.box.width.ch + cy_proportion(c.box.width,c.units))
        end
    end

    c_abs_height = c.box.height.abs
    if !iszero(c.box.height.cx) || !iszero(c.box.height.cy) ||
       !iszero(c.box.height.cw) || !iszero(c.box.height.ch)
        if parent_abs_width == nothing || parent_abs_height == nothing
            c_abs_height = nothing
        else
            c_abs_height = parent_abs_width*(c.box.height.cw + cx_proportion(c.box.height,c.units)) +
                parent_abs_height*(c.box.height.ch + cy_proportion(c.box.height,c.units))
        end
    end

    bb = BoundingBox(Measure(),Measure(),Measure(),Measure())
    for child in c.container_children
        if !isa(child, Context)
            error("Can not compute boundingbox for graphics with non-Context containers")
        end
        cbb = boundingbox(child, linewidth, font, fontsize, c_abs_width, c_abs_height)
        width′  = transformcoordinates(cbb.width,child)
        height′ = transformcoordinates(cbb.height,child)
        x0′     = transformcoordinates(cbb.x0,child)
        y0′     = transformcoordinates(cbb.y0,child)
        bb′ = BoundingBox(child.box.x0+x0′, child.box.y0+y0′, width′, height′)
        bb = union(bb, bb′, c.units, c_abs_width, c_abs_height)
    end

    for child in c.form_children
        for prim in child.primitives
            newbb = boundingbox(prim, linewidth, font, fontsize)
            nextbb = union(bb, newbb, c.units, c_abs_height, c_abs_height)
            bb = nextbb
        end
    end

    return bb
end

# Frequently we can't compute the contents of a container without knowing its
# absolute size, or it is one many possible layout that we want to decide
# between before rendering. A ContainerPromise lets us defer computing a subtree
# until the graphic is actually being rendered.
abstract type ContainerPromise <: Container end

# This information is passed to a container promise at drawtime.
struct ParentDrawContext
    t::Transform
    units::UnitBox
    box::Absolute2DBox
end

# TODO:
# Optionl in layouts will typically be expressed with ad hoc container promises,
# since we can that way avoid realizing layout possibilities that are not used.
# That means we need to be able to express size constraints on these.

mutable struct AdhocContainerPromise <: ContainerPromise
    # A function of the form:
    #   f(parent::ParentDrawContext) → Container
    f::Function

    # Z-order of this context relative to its siblings.
    order::Int

    # Ignore this context and everything under it if we are
    # not drawing to the SVGJS backend.
    withjs::Bool

    # Ignore this context if we are drawing on SVGJS
    withoutjs::Bool

    # Minimum sizes needed to draw the realized subtree correctly.
    minwidth::Maybe(Float64)
    minheight::Maybe(Float64)
end

function ctxpromise(f::Function; order=0, withjs::Bool=false,
                    withoutjs::Bool=false, minwidth=nothing, minheight=nothing)
    if isa(minwidth, Measure)
        minwidth = minwidth.abs
    end

    if isa(minheight, Measure)
        minheight = minwidth.abs
    end

    return AdhocContainerPromise(f, order, withjs, withoutjs, minwidth, minheight)
end

realize(promise::AdhocContainerPromise, drawctx::ParentDrawContext) = promise.f(drawctx)

function compose!(a::Context, b::Container)
    a.container_children = cons(b, a.container_children)
    return a
end

function compose!(a::Context, b::Form)
    a.form_children = cons(b, a.form_children)
    return a
end

function compose!(a::Context, b::Property)
    a.property_children = cons(b, a.property_children)
    return a
end

function compose(a::Context, b::ComposeNode)
    a = copy(a)
    compose!(a, b)
    return a
end

# higher-order compositions
"""
    compose!(a::Context, b, c, ds...) -> a

Add `b`, `c`, and `ds` to the graphic as children of `a`.
"""
compose!(a::Context, b, c, ds...) = compose!(compose!(a, b), c, ds...)
compose!(a::Context, bs::AbstractArray) = compose!(a, compose!(bs...))
compose!(a::Context, bs::Tuple) = compose!(a, compose!(bs...))
compose!(a::Context) = a

"""
    compose(a::Context, b, c, ds...) -> Context

Add `b`, `c`, and `ds` to the graphic as children of a copy of `a`.
"""
compose(a::Context, b, c, ds...) = compose(compose(a, b), c, ds...)
compose(a::Context, bs::AbstractArray) = compose(a, compose(bs...))
compose(a::Context, bs::Tuple) = compose(a, compose(bs...))
compose(a::Context) = a
compose(a, b::Nothing) = a

for (f, S, T) in [(:compose!, Property, Nothing),
                  (:compose!, Form, Nothing),
                  (:compose!, Property, Any),
                  (:compose!, Form, Any),
                  (:compose, Property, Nothing),
                  (:compose, Form, Nothing),
                  (:compose, Property, Any),
                  (:compose, Form, Any)]
    eval(
        quote
            function $(f)(a::$(S), b::$(T))
                error(@sprintf("Invalid composition: %s cannot be a root node.", $(S).name))
            end
        end)
end

"""
    draw(b::Backend, c::Context)

Output the tree-structured graphic in `c` to the given backend.

# Examples
```
draw(SVGJS("foo.svg"), compose(context(), text(0,0,"foo")))
```
"""
function draw(backend::Backend, root_container::Container)
    isfinished(backend) && error("The backend has already been drawn upon.")

    drawpart(backend, root_container, IdentityTransform(), UnitBox(), root_box(backend))
    finish(backend)
end

register_coords(backend::Backend, box, units, transform, form) = nothing

struct DrawState
    pop_poperty::Bool
    container::Container
    parent_transform::Transform
    units::UnitBox
    parent_box::Absolute2DBox
end
DrawState(container, parent_transform, units, parent_box) =
        DrawState(false, container, parent_transform, units, parent_box)
DrawState() = DrawState(true)  ### ???

# Draw without finishing the backend
#
# Drawing is basically a depth-first traversal of the tree, pushing and popping
# properties, expanding context promises, etc. as needed.
#
function drawpart(backend::Backend, container::Container,
                  parent_transform::Transform, units::UnitBox,
                  parent_box::Absolute2DBox)

    ((iswithjs(container) && !iswithjs(backend)) ||
       (iswithoutjs(container) && iswithjs(backend))) && return

    if isa(container, ContainerPromise)
        container = realize(container,
                            ParentDrawContext(parent_transform, units, parent_box))
        isa(container, Container) ||
                error("Error: A container promise function did not evaluate to a container")
        drawpart(backend, container, parent_transform, units, parent_box)
        return
    end

    ctx = optimize_batching(container)
    box = resolve(parent_box, units, parent_transform, ctx.box)

    transform = parent_transform
    if ctx.units !== nothing
        units = resolve(box, units, transform, ctx.units)
    end

    if ctx.rot !== nothing
        rot = resolve(box, units, parent_transform, ctx.rot)
        transform = combine(convert(Transform, rot), transform)
    end

    if ctx.mir !== nothing
        mir = resolve(box, units, parent_transform, ctx.mir)
        transform = combine(convert(Transform, mir), transform)
    end

    if ctx.shear !== nothing
        shear = resolve(box, units, parent_transform, ctx.shear)
        transform = combine(convert(Transform, shear), transform)
    end

    if ctx.raster && @isdefined(Cairo) && isa(backend, SVG)
        bitmapbackend = PNG(box.a[1], box.a[2], false)
        draw(bitmapbackend, ctx)
        f = bitmap("image/png", take!(bitmapbackend.out),
                   0, 0, 1w, 1h)

        c = context(ctx.box.x0[1], ctx.box.x0[2],
                    ctx.box.a[1], ctx.box.a[2],
                    units=UnitBox(),
                    order=ctx.order,
                    clip=ctx.clip)
        drawpart(backend, compose(c, f), parent_transform, units, parent_box)
        return
    end


    has_properties = false
    if !isa(ctx.property_children, ListNull) || ctx.clip
        has_properties = true
        properties = Array{Property}(undef, 0)

        child = ctx.property_children
        while !isa(child, ListNull)
            register_coords(backend, box, units, transform, child.head)
            push!(properties, resolve(parent_box, units, parent_transform, child.head))
            child = child.tail
        end

        if ctx.clip
            x0 = ctx.box.x0[1]
            y0 = ctx.box.x0[2]
            x1 = x0 + ctx.box.a[1]
            y1 = y0 + ctx.box.a[2]
            push!(properties,
            resolve(parent_box, units, parent_transform,
            clip([(x0, y0), (x1, y0), (x1, y1), (x0, y1)])))
        end

        push_property_frame(backend, properties)
    end

    trybatch = canbatch(backend)
    child = ctx.form_children
    while !isa(child, ListNull)
        register_coords(backend, box, units, transform, child.head)
        form = resolve(box, units, transform, child.head)
        if isempty(form.primitives)
            child = child.tail
            continue
        end

        if trybatch
            b = batch(form)
            if b !== nothing
                draw(backend, b)
                child = child.tail
                continue
            end
        end

        draw(backend, form)
        child = child.tail
    end

    # Order children if needed
    ordered_children = false
    child = ctx.container_children
    while !isa(child, ListNull)
        if order(child.head) != 0
            ordered_children = true
            break
        end
        child = child.tail
    end

    if ordered_children
        container_children = Array{Tuple{Int, Int, Container}}(undef, 0)
        child = ctx.container_children
        while !isa(child, ListNull)
            push!(container_children,
                  (order(child.head), 1 + length(container_children), child.head))
            child = child.tail
        end

        sort!(container_children)
        for i in 1:length(container_children)
            drawpart(backend, container_children[i][3], transform, units, box)
        end
        empty!(container_children)
    else
        child = ctx.container_children
        while !isa(child, ListNull)
            drawpart(backend, child.head, transform, units, box)
            child = child.tail
        end
    end

    has_properties && pop_property_frame(backend)
end

# Produce a tree diagram representing the tree structure of a graphic.
#
# Args:
#   root: graphic to represent
#
# Returns:
#   A Context giving a tree diagram.
#
function introspect(root::Context)
    positions = Dict{ComposeNode, Tuple{Float64, Float64}}()
    level_count = Int[]
    max_level = 0

    # TODO: It would be nice if we can try to do a better job of positioning
    # nodes within their levels

    q = Queue{Tuple{ComposeNode, Int}}()
    enqueue!(q, (root, 1))
    figs = compose!(context(), stroke("#333"), linewidth(0.5mm))
    figsize = 6mm
    while !isempty(q)
        node, level = dequeue!(q)

        if level > length(level_count)
            push!(level_count, 1)
        else
            level_count[level] += 1
        end
        max_level = max(max_level, level)

        # draw shit
        fig = context(level_count[level] - 1, level - 1)
        if isa(node, Context)
            compose!(fig, circle(0.5, 0.5, figsize/2), fill(LCHab(92, 10, 77)))
            for child in children(node)
                enqueue!(q, (child, level + 1))
            end
        elseif isa(node, Container)
            # TODO: should be slightly different than Context...
            compose!(fig, circle(0.5, 0.5, figsize/2), fill(LCHab(92, 10, 77)))
        elseif isa(node, Form)
            compose!(fig,
                (context(),
                 text(0.5cx, 0.5cy, form_string(node), hcenter, vcenter),
                 fill(colorant"black")),
                (context(),
                 rectangle(0.5cx - figsize/2, 0.5cy - figsize/2, figsize, figsize),
                 fill(LCHab(68, 74, 192))))
        elseif isa(node, Property)
            # TODO: what should the third color be?
            compose!(fig,
                polygon([(0.5cx - figsize/2, 0.5cy - figsize/2),
                         (0.5cx + figsize/2, 0.5cy - figsize/2),
                         (0.5cx, 0.5cy + figsize/2)]),
                fill(LCHab(68, 74, 29)))
        else
            error("Unknown node type $(typeof(node))")
        end
        compose!(figs, fig)

        positions[node] = (level_count[level] - 0.5, level - 0.5)
    end

    # make a second traversal of the tree to draw lines between parents and
    # children
    lines_ctx = compose!(context(order=-1), stroke(LCHab(92, 10, 77)))
    enqueue!(q, (root, 1))
    while !isempty(q)
        node, level = dequeue!(q)
        isa(node, Context) || continue
        pos = positions[node]

        for child in children(node)
            childpos = positions[child]
            compose!(lines_ctx,
                     line([(pos[1], pos[2]), (childpos[1], childpos[2])]))
            enqueue!(q, (child, level + 1))
        end
    end

    return compose!(context(units=UnitBox(0, 0, maximum(level_count), max_level)),
                    (context(order=-2), rectangle(), fill("#333")),
                    lines_ctx, figs)
end

function show(io::IO, ctx::Context)
    if get(io, :compact, false)
        print(io, "Context(")
        first = true
        for c in children(ctx)
            first || print(io, ",")
            first = false
            isa(c, AbstractArray) ? showcompact_array(io, c) : show(io, c)
        end
        print(io, ")")
    else
        invoke(show, Tuple{IO, Any}, io, ctx)
    end
end

function showcompact_array(io::IO, a::AbstractArray)
    print(io, "[")
    first = true
    for c in a
        first || print(io, ",")
        first = false
        show(io, c)
    end
    print(io, "]")
end
showcompact_array(io::IO, a::AbstractRange) = show(io, a)
show(io::IO, f::Compose.Form) = get(io, :compact, false) ? print(io, Compose.form_string(f)) : invoke(show, Tuple{IO, Any}, io, f)
show(io::IO, p::Compose.Property) = get(io, :compact, false) ? print(io, Compose.prop_string(p)) : invoke(show, Tuple{IO, Any}, io, p)
show(io::IO, cp::ContainerPromise) = get(io, :compact, false) ? print(io, typeof(cp).name.name) : invoke(show, Tuple{IO, Any}, io, cp)
