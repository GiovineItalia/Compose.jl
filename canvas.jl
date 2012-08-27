
# Canvas: a thing upon which other things are placed.

require("measure.jl")
require("form.jl")
require("property.jl")


type Units
    # TODO: using a bounding box implies we can use any sort of units, when in
    # fact the box used here should not have units associated with it.
    box::BoundingBox

    function Units(width::MeasureOrNumber,
                   height::MeasureOrNumber)
        new(BoundingBox(0.0, 0.0, width, height))
    end

    function Units(x0::MeasureOrNumber,
                   y0::MeasureOrNumber,
                   width::MeasureOrNumber,
                   height::MeasureOrNumber)
        new(BoundingBox(x0, y0, width, height))
    end
end


type Canvas
    box::BoundingBox
    property::Property
    children::Vector{Canvas}
    form::Form
    unit_box::BoundingBox
    rot::Rotation

    function Canvas(opts::Union(Units, Rotation)...)
        Canvas(0.0, 0.0, 1.0, 1.0, opts...)
    end

    function Canvas(x0::MeasureOrNumber,
                    y0::MeasureOrNumber,
                    width::MeasureOrNumber,
                    height::MeasureOrNumber,
                    opts::Union(Units, Rotation)...)
        c = new(BoundingBox(x0, y0, width, height),
                Property(),
                Canvas[],
                Form(),
                BoundingBox(),
                Rotation())

        for opt in opts
            if typeof(opt) == Rotation
                c.rot = opt
            elseif typeof(opt) == Units
                c.unit_box = opt.box
            end
        end

        c
    end
end


# A type packaging a canvas with the information needed to draw it.
type DrawCanvasContext
    canvas::Canvas
    t::NativeTransform
    unit_box::BoundingBox
    parent_box::NativeBoundingBox
    parent_property::Property
end


# Draw a canvas on a backend
function draw(backend::Backend, root_canvas::Canvas)
    Q = Queue()
    box = root_box(backend)
    enqueue(Q,
        DrawCanvasContext(root_canvas,
                          NativeTransform(),
                          BoundingBox(),
                          box,
                          default_property(backend)))

    while !isempty(Q)
        ctx = pop(Q)
        box = native_measure(ctx.canvas.box, ctx.t, ctx.unit_box,
                             ctx.parent_box, backend)
        rot = native_measure(ctx.canvas.rot, ctx.t, ctx.unit_box,
                             box, backend)
        t = combine(rot, ctx.t)

        property = isempty(ctx.canvas.property) ?
                ctx.parent_property : copy(ctx.parent_property)
        compose!(property, ctx.canvas.property)

        for f in ctx.canvas.form.specifics
            draw(backend, t, ctx.canvas.unit_box, box, property, f)
        end

        for child in ctx.canvas.children
            enqueue(Q, DrawCanvasContext(child, t, ctx.canvas.unit_box,
                                         box, property))
        end
    end
end


