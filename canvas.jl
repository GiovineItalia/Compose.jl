
# Canvas: a thing upon which other things are placed.

require("measure.jl")
require("form.jl")
require("property.jl")

type Canvas
    box::BoundingBox
    property::Property
    children::Vector{Canvas}
    form::Form
end


# A type packaging a canvas with the information needed to draw it.
type DrawCanvasContext
    canvas::Canvas
    parent_box::Property
    parent_property::Property
end


# Draw a canvas on a backend
function draw(backend::Backend, root_canvas::Canvas)
    Q = Queue()
    enqueue(Q,
        DrawCanvasContext(root_canvas,
                          root_box(backend),
                          default_property(backend)))

    while !isempty(Q)
        ctx = pop(Q)
        box = backend_coord(ctx.canvas.box, ctx.parent_box, backend)

        property = isempty(ctx.canvas.property) ?
                ctx.parent_property : copy(ctx.parent_property)
        compose!(property, ctx.canvas.property)

        for f in form.specificts
            draw(backend, box, f)
        end

        for child in ctx.canvas.children
            enqueue(Q, DrawCanvasContext(child, box, property))
        end
    end
end


