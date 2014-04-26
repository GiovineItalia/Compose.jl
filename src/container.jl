
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

    # True if this canvas should use its parent's coordinate system.
    units_inherited::Bool

    # True if children of the canvas should be clipped by its bounding box.
    clip::Bool

    # Ignore this context and everything under it if we are
    # not drawing to the d3 backend.
    d3only::Bool

    # Contexts may be annotated with the minimum size needed to be drawn
    # correctly, information that can be used by layout containers. Sizes
    # are absolute (i.e. millimeters).
    minwidth::Maybe(Float64)
    minheight::Maybe(Float64)


    function Context(x0=0.0w,
                     y0=0.0h,
                     width=1.0w,
                     height=1.0h;
                     units=UnitBox(),
                     rotation=Rotation(),
                     units_inherited=false,
                     order=0,
                     clip=false,
                     d3only=false,
                     minwidth=nothing,
                     minheight=nothing)
        return new(BoundingBox(x0, y0, width, height), units, rotation,
                   ListNull{ComposeNode}(), order, units_inherited, clip,
                   d3only, minwidth, minheight)
    end

    function Context(box::BoundingBox,
                     units::UnitBox,
                     rotation::Rotation,
                     children::List{ComposeNode},
                     order::Int,
                     units_inherited::Bool,
                     clip::Bool,
                     d3only::Bool,
                     minwidth, minheight)
        if isa(minwidth, Measure)
            minwidth = minwidth.abs
        end

        if isa(minheight, Measure)
            minheight = minheight.abs
        end

        return new(box, units, rotation, children, order, units_inherited,
                   clip, d3only, minwidth, minheight)
    end

    function Context(ctx::Context)
        return new(ctx.box, ctx.units, ctx.rot, ctx.children, ctx.order,
                   ctx.units_inherited, ctx.clip, ctx.d3only,
                   ctx.minwidth, ctx.minheight)
    end
end


function context(x0=0.0w,
                 y0=0.0h,
                 width=1.0w,
                 height=1.0h;
                 units=UnitBox(),
                 rotation=Rotation(),
                 units_inherited=false,
                 order=0,
                 clip=false,
                 d3only=false,
                 minwidth=nothing,
                 minheight=nothing)
    return Context(BoundingBox(x0, y0, width, height), units, rotation,
                   ListNull{ComposeNode}(), order, units_inherited, clip,
                   d3only, minwidth, minheight)
end


# Context deserves a short name as it will be used a lot.
const ctx = context


function copy(ctx::Context)
    return Context(ctx)
end


function isd3only(ctx::Container)
    return ctx.d3only
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
    # not drawing to the d3 backend.
    d3only::Bool

    # Minimum sizes needed to draw the realized subtree correctly.
    minwidth::Maybe(Float64)
    minheight::Maybe(Float64)
end



function ctxpromise(f::Function; order=0, d3only::Bool=false,
                    minwidth=nothing, minheight=nothing)
    if isa(minwidth, Measure)
        minwidth = minwidth.abs
    end

    if isa(minheight, Measure)
        minheight = minwidth.abs
    end

    return AdhocContainerPromise(f, order, d3only, minwidth, minheight)
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
    S = {(root_container, Transform(), UnitBox(), root_box(backend))}

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

        if isd3only(container) && !isa(backend, D3)
            continue
        end

        if isa(container, ContainerPromise)
            container = realize(container,
                                ParentDrawContext(parent_transform, units, parent_box))
            if !isa(context, Container)
                error("Error: A container promise function did not evaluate to a container")
            end
            push!(S, (container, parent_transform, units, parent_box))
            continue
        end

        @assert isa(container, Context)
        context = container

        box = absolute_units(context.box, parent_transform, units, parent_box)
        rot = absolute_units(context.rot, parent_transform, units, box)
        transform = combine(rot, parent_transform)

        if !context.units_inherited
            units = context.units
        end

        for child in context.children
            if isa(child, Property)
                push!(properties, absolute_units(child, parent_transform, units, parent_box))
            end
        end

        # TODO: when the clip property is implemented
        #=if context.clip=#
            #=push!(properties,=#
                  #=clip(Point(0w, 0h), Point(1w, 0h),=#
                       #=Point(1w, 1h), Point(0w, 1h)))=#
        #=end=#

        if !isempty(properties)
            push_property_frame(backend, properties)
            push!(S, :POP_PROPERTY_FRAME)
            empty!(properties)
        end

        for child in context.children
            if isa(child, Form)
                draw(backend, transform, units, box, child)
            end
        end

        for child in context.children
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


