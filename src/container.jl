
# A container is a node in the tree that can have Forms, Properties, or other
# Containers as children.
abstract Container <: ComposeNode


# The basic Container which defines a coordinate transform for its children.
type Context <: Container
    # Bounding box relative to the parent's coordinates
    box::BoundingBox

    # Context coordinates used for children
    units::UnitBox

    # Rotation is degrees of
    rot::Rotation

    # Container children
    children::List{ComposeNode}

    # Z-order of this context relative to its siblings.
    order::Int

    # True if children of the canvas should be clipped by its bounding box.
    clip::Bool

    # Ignore this context and everything under it if we are
    # not drawing to the SVGJS backend.
    withjs::Bool

    # Igonre this context if wer *are* drawing on the SVGJS backend.
    withoutjs::Bool

    # If possible, render this subtree as a bitmap. This requires the Cairo. If
    # Cairo isn't available, default rendering is used.
    raster::Bool

    # Contexts may be annotated with the minimum size needed to be drawn
    # correctly, information that can be used by layout containers. Sizes
    # are absolute (i.e. millimeters).
    minwidth::Maybe(Float64)
    minheight::Maybe(Float64)

    # A field that can be used by layouts to indicate that one configuration
    # is preferable to another.
    penalty::Float64

    function Context(x0=0.0w,
                     y0=0.0h,
                     width=1.0w,
                     height=1.0h;
                     units=NilUnitBox(),
                     rotation=Rotation(),
                     order=0,
                     clip=false,
                     withjs=false,
                     withoutjs=false,
                     raster=false,
                     minwidth=nothing,
                     minheight=nothing,
                     penalty=0.0)
        return new(BoundingBox(x0, y0, width, height), units, rotation,
                   ListNull{ComposeNode}(), order, clip,
                   withjs, withoutjs, raster, minwidth, minheight, penalty)
    end

    function Context(box::BoundingBox,
                     units::UnitBox,
                     rotation::Rotation,
                     children::List{ComposeNode},
                     order::Int,
                     clip::Bool,
                     withjs::Bool,
                     withoutjs::Bool,
                     raster::Bool,
                     minwidth, minheight,
                     penalty)
        if isa(minwidth, Measure)
            minwidth = minwidth.abs
        end

        if isa(minheight, Measure)
            minheight = minheight.abs
        end

        return new(box, units, rotation, children, order,
                   clip, withjs, withoutjs, raster, minwidth, minheight,
                   penalty)
    end

    function Context(ctx::Context)
        return new(ctx.box, ctx.units, ctx.rot, ctx.children, ctx.order,
                   ctx.clip, ctx.withjs, ctx.withoutjs, ctx.raster,
                   ctx.minwidth, ctx.minheight, ctx.penalty)
    end
end


function context(x0=0.0w,
                 y0=0.0h,
                 width=1.0w,
                 height=1.0h;
                 units=NilUnitBox(),
                 rotation=Rotation(),
                 order=0,
                 clip=false,
                 withjs=false,
                 withoutjs=false,
                 raster=false,
                 minwidth=nothing,
                 minheight=nothing,
                 penalty=0.0)
    return Context(BoundingBox(x0, y0, width, height), units, rotation,
                   ListNull{ComposeNode}(), order, clip,
                   withjs, withoutjs, raster, minwidth, minheight, penalty)
end


# Updating context fields

function set_units!(ctx::Context, units::UnitBox)
    ctx.units = units
end


function copy(ctx::Context)
    return Context(ctx)
end


function iswithjs(ctx::Container)
    return ctx.withjs
end


function iswithoutjs(ctx::Container)
    return ctx.withoutjs
end


function order(cont::Container)
    return cont.order
end


function minwidth(cont::Container)
    return cont.minwidth
end


function minheight(cont::Container)
    return cont.minheight
end

function cx_percentage(from::Measure,units::UnitBox)
    cux0 = units.x0
    cuw = units.width
    if cux0 == nothing
        cux0 = 0.0
        cuw = 1.0
    end
    ret_cx_percentage = from.cx != measure_nil ? (from.cx-cux0)/cuw : 0.0
    !isfinite(ret_cx_percentage) && (ret_cx_percentage = 0.0)
    ret_cx_percentage
end

function cy_percentage(from::Measure,units::UnitBox)
    cuy0 = units.x0
    cuh = units.width
    if cuy0 == nothing
        cuy0 = 0.0
        cuh = 1.0
    end
    ret_cy_percentage  = from.cy != measure_nil ? (from.cy-cuy0)/cuh : 0.0
    !isfinite(ret_cy_percentage) && (ret_cy_percentage = 0.0)
    ret_cy_percentage
end

function transformcoordinates(from::Measure,child)
    Measure(;abs = from.abs) + (from.cw + cx_percentage(from,child.units))*child.box.width +
                (from.ch + cy_percentage(from,child.units))*child.box.height
end

function boundingbox(c::Context,linewidth::Measure=default_line_width,
                     font::String=default_font_family,
                     fontsize::Measure=default_font_size,
                     parent_abs_width = nothing,
                     parent_abs_height = nothing)
    for child in c.children
        if !isa(child, Property)
            continue
        end
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
    if (c.box.width.cx != measure_nil && c.box.width.cx != 0) ||
        (c.box.width.cx != measure_nil && c.box.width.cx != 0) ||
        (c.box.width.cw != 0.0 && c.box.width.ch != 0.0)
        if parent_abs_width == nothing || parent_abs_height == nothing
            c_abs_width = nothing
        else
            c_abs_width = parent_abs_width*(c.box.width.cw + cx_percentage(c.box.width,c.units)) +
                parent_abs_height*(c.box.width.ch + cy_percentage(c.box.width,c.units))
        end
    end

    c_abs_height = c.box.height.abs
    if (c.box.height.cx != measure_nil && c.box.height.cx != 0) ||
        (c.box.height.cx != measure_nil && c.box.height.cx != 0) ||
        (c.box.height.cw != 0.0 && c.box.height.ch != 0.0)
        if parent_abs_width == nothing || parent_abs_height == nothing
            c_abs_height = nothing
        else
            c_abs_height = parent_abs_width*(c.box.height.cw + cx_percentage(c.box.height,c.units)) +
                parent_abs_height*(c.box.height.ch + cy_percentage(c.box.height,c.units))
        end
    end

    bb = BoundingBox(Measure(),Measure(),Measure(),Measure())
    for child in c.children
        if isa(child, Property)
            continue
        elseif isa(child,Context)
            cbb = boundingbox(child, linewidth, font, fontsize, c_abs_width, c_abs_height)
            width′  = transformcoordinates(cbb.width,child)
            height′ = transformcoordinates(cbb.height,child)
            x0′     = transformcoordinates(cbb.x0,child)
            y0′     = transformcoordinates(cbb.y0,child)
            bb′ = BoundingBox(child.box.x0+x0′, child.box.y0+y0′, width′, height′)
            bb = union(bb, bb′, c.units, c_abs_width, c_abs_height)
        elseif isa(child, Container)
            error("Can not compute boundingbox for graphics with non-Context containers")
        elseif isa(child, Form)
            for prim in child.primitives
                newbb = boundingbox(prim, linewidth, font, fontsize)
                nextbb = union(bb, newbb, c.units, c_abs_height, c_abs_height)
                bb = nextbb
            end
        end
    end
    return bb
end


# Frequently we can't compute the contents of a container without knowing its
# absolute size, or it is one many possible layout that we want to decide
# between before rendering. A ContainerPromise lets us defer computing a subtree
# until the graphic is actually being rendered.
abstract ContainerPromise <: Container


# This information is passed to a container promise at drawtime.
immutable ParentDrawContext
    t::Transform
    units::UnitBox
    box::AbsoluteBoundingBox
end


# TODO:
# Optionl in layouts will typically be expressed with ad hoc container promises,
# since we can that way avoid realizing layout possibilties that are not used.
# That means we need to be able to express size constraints on these.


type AdhocContainerPromise <: ContainerPromise
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


function realize(promise::AdhocContainerPromise, drawctx::ParentDrawContext)
    return promise.f(drawctx)
end


function compose!(a::Context, b::ComposeNode)
    a.children = cons(b, a.children)
    return a
end


function compose(a::Context, b::ComposeNode)
    a = copy(a)
    compose!(a, b)
    return a
end


# higher-order compositions
function compose!(a::Context, b, c, ds...)
    return compose!(compose!(a, b), c, ds...)
end


function compose!(a::Context, bs::AbstractArray)
    compose!(a, compose!(bs...))
end


function compose!(a::Context, bs::Tuple)
    compose!(a, compose!(bs...))
end


function compose!(a::Context)
    return a
end


function compose(a::Context, b, c, ds...)
    return compose(compose(a, b), c, ds...)
end


function compose(a::Context, bs::AbstractArray)
    compose(a, compose(bs...))
end


function compose(a::Context, bs::Tuple)
    compose(a, compose(bs...))
end


function compose(a::Context)
    return a
end


function compose(a, b::Nothing)
    return a
end


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


function draw(backend::Backend, root_canvas::Context)
    drawpart(backend, root_canvas)
    finish(backend)
end


function draw(backend::Backend, root_form::Form)
    draw(backend, compose(context(), root_form))
end


# Draw without finishing the backend
#
# Drawing is basically a depth-first traversal of the tree, pushing and poping
# properties, expanding context promises, etc as needed.
#
function drawpart(backend::Backend, root_container::Container)
    S = {(root_container, IdentityTransform(), UnitBox(), root_box(backend))}

    # used to collect property children
    properties = Array(Property, 0)

    # collect and sort container children
    container_children = Array((Int, Int, Container), 0)

    while !isempty(S)
        s = pop!(S)

        # Groups of properties are in a property frame, analagous to a stack
        # frame. A marker is pushed to the stack so we know when to pop the
        # frame.
        if s == :POP_PROPERTY_FRAME
            pop_property_frame(backend)
            continue
        end

        container, parent_transform, units, parent_box = s

        if (iswithjs(container) && !iswithjs(backend)) ||
           (iswithoutjs(container) && iswithjs(backend))
            continue
        end

        if isa(container, ContainerPromise)
            container = realize(container,
                                ParentDrawContext(parent_transform, units, parent_box))
            if !isa(container, Container)
                error("Error: A container promise function did not evaluate to a container")
            end
            push!(S, (container, parent_transform, units, parent_box))
            continue
        end

        @assert isa(container, Context)
        ctx = container

        box = absolute_units(ctx.box, parent_transform, units, parent_box)
        rot = absolute_units(ctx.rot, parent_transform, units, box)
        transform = combine(convert(Transform, rot), parent_transform)

        if ctx.raster && isdefined(:Cairo) && isa(backend, SVG)
            # TODO: commented out while I search for the real source of the
            # slowness, cause it it aint this.
            bitmapbackend = PNG(box.width, box.height, false)
            draw(bitmapbackend, ctx)
            f = bitmap("image/png", takebuf_array(bitmapbackend.out),
                       0, 0, 1w, 1h)

            c = context(ctx.box.x0, ctx.box.y0,
                        ctx.box.width, ctx.box.height,
                        units=UnitBox(),
                        order=ctx.order,
                        clip=ctx.clip)
            push!(S, (compose(c, f), parent_transform, units, parent_box))
            continue
        end

        if ctx.units != nil_unit_box
            units = absolute_units(ctx.units, transform, units, box)
        end

        for child in ctx.children
            if isa(child, Property)
                push!(properties, absolute_units(child, parent_transform, units, parent_box))
            end
        end

        if ctx.clip
            x0 = ctx.box.x0
            y0 = ctx.box.y0
            x1 = x0 + ctx.box.width
            y1 = y0 + ctx.box.height
            push!(properties,
                  absolute_units(clip(Point(x0, y0), Point(x1, y0),
                                      Point(x1, y1), Point(x0, y1)),
                                 parent_transform, units, parent_box))
        end

        if !isempty(properties)
            push_property_frame(backend, properties)
            push!(S, :POP_PROPERTY_FRAME)
            empty!(properties)
        end

        for child in ctx.children
            if isa(child, Form)
                draw(backend, transform, units, box, child)
            end
        end

        for child in ctx.children
            if isa(child, Container)
                push!(container_children,
                      (order(child), 1 + length(container_children), child))
            end
        end
        sort!(container_children, rev=true)

        for (_, _, child) in container_children
            push!(S, (child, transform, units, box))
        end
        empty!(container_children)
    end
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
    positions = Dict{ComposeNode, (Float64, Float64)}()
    level_count = Int[]
    max_level = 0

    # TODO: It would be nice if we can try to do a better job of positioning
    # nodes within their levels

    q = Queue((ComposeNode, Int))
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
            for child in node.children
                enqueue!(q, (child, level + 1))
            end
        elseif isa(node, Container)
            # TODO: should be slightly different than Context...
            compose!(fig, circle(0.5, 0.5, figsize/2), fill(LCHab(92, 10, 77)))
        elseif isa(node, Form)
            compose!(fig,
                rectangle(0.5cx - figsize/2, 0.5cy - figsize/2, figsize, figsize),
                fill(LCHab(68, 74, 192)))
        elseif isa(node, Property)
            # TODO: what should the third color be?
            compose!(fig,
                polygon([(0.5cx - figsize/2, 0.5cy - figsize/2),
                         (0.5cx + figsize/2, 0.5cy - figsize/2),
                         (0.5, 0.5cy + figsize/2)]),
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
        if !isa(node, Context)
            continue
        end
        pos = positions[node]

        for child in node.children
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





