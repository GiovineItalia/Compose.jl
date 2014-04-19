
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

    function Context(x0=0.0w,
                     y0=0.0h,
                     width=1.0w,
                     height=1.0h;
                     units=UnitBox(),
                     rotation=Rotation(),
                     units_inherited=false,
                     order=0,
                     clip=false,
                     d3only=false)
        return new(BoundingBox(x0, y0, width, height), units, rotation, children,
                   order, units_inherited, clip, d3only)
    end

    function Context(box::BoundingBox,
                     units::UnitBox,
                     rotation::Rotation,
                     children::List{ComposeNode},
                     order::Int,
                     units_inherited::Bool,
                     clip::Bool,
                     d3only::Bool)
        return new(box, units, rotation, children, order, units_inherited,
                   clip, d3only)
    end

    function Context(ctx::Context)
        return new(ctx.box, ctx.units, ctx.rot, ctx.children, ctx.order,
                   ctx.units_inherited, ctx.clip, ctx.d3only)
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
                 d3only=false)
    return Context(BoundingBox(x0, y0, width, height), units, rotation,
                   ListNull{ComposeNode}(), order, units_inherited, clip, d3only)
end


function copy(ctx::Context)
    return Context(ctx)
end


function isd3only(ctx::Context)
    return ctx.d3only
end


# Next thing to consider:
#   Containers need to have a notion of minimum absolute size for layouts to
#   work. For Contexts, it shoud be easy enough to compute.


# Frequently we can't compute the contents of a container without knowing its
# absolute size, or it is one many possible layout that we want to decide
# between before rendering. A ContainerPromise lets us defer computing a subtree
# until the graphic is actually being rendered.
abstract ContainerPromise <: Container

type AdhocContainerPromise <: ContainerPromise
    # A function of the form:
    #   f(box::BoundingBox, transform::Transform, units::UnitBox) → Container
    f::Function

    # Ignore this context and everything under it if we are
    # not drawing to the d3 backend.
    d3only::Bool
end


function realize(promise::AdhocContainerPromise, box::BoundingBox,
                 units::UnitBox)
    return promise.f(box, units)
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
            container = container.f(units, parent_transform, parent_box)
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
                push!(properties, child)
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
                      (order(child), 1 + length(container_children)))
            end
        end
        sort!(container_children, rev=true)

        for (_, _, child) in container_children
            push!(S, (child, transform, units, box))
        end
        empty!(container_children)
    end
end


