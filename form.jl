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

    function Form()
        new(Property(),
            FormType[])
    end

    function Form(property::Property, specifics::Vector{FormType})
        new(property, specifics)
    end
end


# Package a Form with the information needed to draw it.
type DrawFormContext
    form::FormType
    parent_property::Property
end


# Draw a form and all it contains on a backend within a bounding box.
function draw(backend::Backend, t::NativeTransform,
              unit_box::BoundingBox, box::NativeBoundingBox,
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
            draw(backend, t, unit_box, box, ctx.parent_property)
            draw(backend, t, unit_box, box, ctx.form)
        else
            if isempty(ctx.form.property)
                property = ctx.parent_property
            else
                property = copy(ctx.parent_property)
                append!(property.specifics, ctx.form.property.specifics)
            end

            for f in ctx.form.specifics
                enqueue(Q, DrawFormContext(f, property))
            end
        end
    end
end



# Specific forms

# Hmmm, it is a bit uglier without DrawOps bceause now we have to convert
# everything to native coordinates. Back to DrawOps then?



# Note that the constructors for these types are somewhat peculiar: they return
# an instance of the type they construct wrapped in a Form.


type LinesForm <: FormType
    points::Vector{Point}

end

function Lines(points::XYTupleOrPoint...)
    Form(Property(),
         FormType[LinesForm([convert(Point, point) for point in points])])
end


function draw(backend::Backend, t::NativeTransform, unit_box::BoundingBox,
              box::NativeBoundingBox, form::LinesForm)
    native_form = LinesForm()
    native_form.points = [native_measure(point, t, unit_box, box, backend)
                          for point in form.points]
    draw(backend, native_form)
end


type PolygonForm <: FormType
    points::Vector{Point}

end

function Polygon(points::XYTupleOrPoint...)
    Form(Property(),
         FormType[PolygonForm([convert(Point, point) for point in points])])
end


function draw(backend::Backend, t::NativeTransform, unit_box::BoundingBox,
              box::NativeBoundingBox, form::PolygonForm)
    native_form = PolygonForm()
    native_form.points = [native_measure(point, t, unit_box, box, backend)
                          for point in form.points]
    draw(backend, native_form)
end


type RectangleForm <: FormType
    xy0::Point
    xy1::Point
end


function Rectangle(x0::MeasureOrNumber, y0::MeasureOrNumber,
                   width::MeasureOrNumber, height::MeasureOrNumber)
    Form(Property(),
         FormType[RectangleForm(Point(x0, y0), Point(x0 + width, y0 + height))])
end


function Rectangle()
    Rectangle(0.0, 0.0, 1.0, 1.0)
end



function draw(backend::Backend, t::NativeTransform, unit_box::BoundingBox,
              box::NativeBoundingBox, form::RectangleForm)
    native_form = RectangleForm(
        native_measure(form.xy0, t, unit_box, box, backend),
        native_measure(form.xy1, t, unit_box, box, backend))
    draw(backend, native_form)
end


