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


# Ack! We have to do depth first traversal if we want to be able to push and pop properties
# as intended.


# Draw a form and all it contains on a backend within a bounding box.
function draw(backend::Backend, t::NativeTransform,
              unit_box::BoundingBox, box::NativeBoundingBox,
              form::Form)

    if !isempty(form.property)
        push_property(backend, form.property)
    end

    for specific in form.specifics
        draw(backend, t, unit_box, box, specific)
    end

    if !isempty(form.property)
        pop_property(backend)
    end
end


type LinesForm <: FormType
    points::Vector{Point}

end

function Lines(points::XYTupleOrPoint...)
    Form(Property(),
         FormType[LinesForm([convert(Point, point) for point in points])])
end


function draw(backend::Backend, t::NativeTransform, unit_box::BoundingBox,
              box::NativeBoundingBox, form::LinesForm)
    native_form = LinesForm([native_measure(point, t, unit_box, box, backend)
                             for point in form.points])
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
    native_form = PolygonForm([native_measure(point, t, unit_box, box, backend)
                               for point in form.points])
    draw(backend, native_form)
end


function Rectangle(x0::MeasureOrNumber, y0::MeasureOrNumber,
                   width::MeasureOrNumber, height::MeasureOrNumber)

    xy0 = Point(x0, y0)
    xy1 = Point(x0 + width, y0 + height)
    Polygon(xy0, Point(xy1.x, xy0.y), xy1, Point(xy0.x, xy1.y))
end


function Rectangle()
    Rectangle(0.0, 0.0, 1.0, 1.0)
end


type EllipseForm <: FormType
    center::Point
    x_point::Point
    y_point::Point
end


function Ellipse(x::MeasureOrNumber, y::MeasureOrNumber,
                 x_radius::MeasureOrNumber, y_radius::MeasureOrNumber)
    x = x_measure(x)
    y = y_measure(y)
    Form(Property(),
         FormType[EllipseForm(Point(x, y),
                              Point(x + x_measure(x_radius), y),
                              Point(x, y + y_measure(y_radius)))])
end


function Circle(x::MeasureOrNumber, y::MeasureOrNumber, radius::MeasureOrNumber)
    Ellipse(x, y, radius, radius)
end


function draw(backend::Backend, t::NativeTransform, unit_box::BoundingBox,
              box::NativeBoundingBox, form::EllipseForm)
    native_form = EllipseForm(
        native_measure(form.center, t, unit_box, box, backend),
        native_measure(form.x_point, t, unit_box, box, backend),
        native_measure(form.y_point, t, unit_box, box, backend))
    draw(backend, native_form)
end


