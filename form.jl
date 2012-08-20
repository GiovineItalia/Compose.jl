
# Form: a thing that is visible once drawn.

require("backend.jl")
require("queue.jl")
require("property.jl")
require("measure.jl")

# A bare form.
abstract FormType

# A container for one or more forms with applied properties.
type Form <: FormType
    property::Property
    specifics::Vector{FormType}
end


# Package a Form with the information needed to draw it.
type DrawFormContext
    form::Form
    parent_property::Property
end


# Draw a form and all it contains on a backend within a bounding box.
function draw(backend::Backend, box::BoundingBox,
              root_property::Property, root_form::Form)
    Q = Queue()
    enqueue(Q,
        DrawFormContext(root_form,
                        root_property))

    while !isempty(Q)
        ctx = pop(Q)

        # if form is not a Form, it is not a container so we can draw in
        # directly
        if typeof(ctx.form) != Form
            draw(backend, ctx.parent_property)
            draw(backend, box, ctx.form)
        else
            property = compose(ctx.parent_property, ctx.form.property)
            for f in ctx.form.specifics
                enqueue(Q, DrawFormContext(f, property))
            end
        end
    end
end


# Each form translates into a series of lower-level drawing operations.
abstract DrawOp

type MoveTo <: DrawOp
    point::Point
end


type LineTo <: DrawOp
    point::Point
end


type FillStroke <: DrawOp end
type ClasePath  <: DrawOp end


# Specific forms

# Note that the constructors for these types are somewhat peculiar: they return
# an instance of the type they construct wrapped in a Form.


type Lines <: FormType
    points::Vector{Point}

    function Lines(points::XYTupleOrPoint...)
        Form(FormType[new([convert(Point, point) for point in points])])
    end
end


function draw(backend::Backend, box::BoundingBox, form::Lines)
    if isempty(form.points); return; end

    draw(backend, box, MoveTo(form.points[1]))
    for point in form.points[2:]
        draw(backend, box, LineTo(point))
    end
    draw(backend, box, ClosePath())
    draw(backend, box, FillStroke())
end


type Polygon <: FormType
    points::Vector{Point}

    function Polygon(points::XYTupleOrPoint...)
        Form(FormType[new([convert(Point, point) for point in points])])
    end
end


function draw(backend::Backend, box::BoundingBox, form::Polygon)
    if isempty(form.points); return; end

    draw(backend, box, MoveTo(form.points[1]))
    for point in form.points[2:]
        draw(backend, box, LineTo(point))
    end
    draw(backend, box, FillStroke())
end



