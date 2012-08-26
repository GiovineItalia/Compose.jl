
# Canvas: a thing upon which other things are placed.

require("measure.jl")
require("form.jl")
require("property.jl")

type Canvas
    box::BoundingBox
    property::Property
    children::Vector{Canvas}
    form::Form
    unit_box::BoundingBox

    function Canvas()
        new(BoundingBox(),
            Property(),
            Canvas[],
            Form(),
            BoundingBox())
    end

    function Canvas(x0::MeasureOrNumber,
                    y0::MeasureOrNumber,
                    width::MeasureOrNumber,
                    height::MeasureOrNumber)
        new(BoundingBox(x0, y0, width, height),
            Property(),
            Canvas[],
            Form(),
            BoundingBox())
    end

    function Canvas(x0::MeasureOrNumber,
                    y0::MeasureOrNumber,
                    width::MeasureOrNumber,
                    height::MeasureOrNumber,
                    x_units::Number,
                    y_units::Number)
        new(BoundingBox(x0, y0, width, height),
            Property(),
            Canvas[],
            Form(),
            BoundingBox(0.0, 0.0,
                        convert(Float64, x_units),
                        convert(Float64, y_units)))
    end

    function Canvas(x0::MeasureOrNumber,
                    y0::MeasureOrNumber,
                    width::MeasureOrNumber,
                    height::MeasureOrNumber,
                    x_base::Number,
                    y_base::Number,
                    x_units::Number,
                    y_units::Number)
        new(BoundingBox(x0, y0, width, height),
            Property(),
            Canvas[],
            Form(),
            BoundingBox(x_base, y_base, x_units, y_units))
    end
end


# A type packaging a canvas with the information needed to draw it.
type DrawCanvasContext
    canvas::Canvas
    unit_box::BoundingBox
    parent_box::NativeBoundingBox
    parent_property::Property
end


# Draw a canvas on a backend
function draw(backend::Backend, root_canvas::Canvas)
    Q = Queue()
    enqueue(Q,
        DrawCanvasContext(root_canvas,
                          BoundingBox(),
                          root_box(backend),
                          default_property(backend)))

    while !isempty(Q)
        ctx = pop(Q)
        box = native_measure(ctx.canvas.box, ctx.unit_box,
                             ctx.parent_box, backend)

        property = isempty(ctx.canvas.property) ?
                ctx.parent_property : copy(ctx.parent_property)
        compose!(property, ctx.canvas.property)

        for f in ctx.canvas.form.specifics
            draw(backend, ctx.canvas.unit_box, box, property, f)
        end

        for child in ctx.canvas.children
            enqueue(Q, DrawCanvasContext(child, ctx.canvas.unit_box,
                                         box, property))
        end
    end
end


