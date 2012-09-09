# Form: a thing that is visible once drawn.

require("backend.jl")
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

    # copy constructor
    function Form(form::Form)
        new(copy(form.property),
            copy(form.specifics))
    end
end


copy(form::Form) = Form(form)


# Draw a form and all it contains on a backend within a bounding box.
function draw(backend::Backend, t::NativeTransform,
              unit_box::BoundingBox, box::NativeBoundingBox,
              form::Form)

    if !isempty(form.property)
        push_property(backend,
                      native_measure(form.property, t, unit_box, box, backend))
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

copy(form::LinesForm) = LinesForm(copy(form.points))

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

copy(form::PolygonForm) = PolygonForm(copy(form.points))

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
    Rectangle(0.0w, 0.0h, 1.0w, 1.0h)
end


type EllipseForm <: FormType
    center::Point
    x_point::Point
    y_point::Point
end


function copy(form::EllipseForm)
    EllipseForm(copy(form.center),
                copy(form.x_point),
                copy(form.y_point))
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


function Ellipse()
    Ellipse(1/2w, 1/2h, 1/2w, 1/2h)
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


abstract HAlignment
type HLeft   <: HAlignment end
type HCenter <: HAlignment end
type HRight  <: HAlignment end

const hleft   = HLeft()
const hcenter = HCenter()
const hright  = HRight()

abstract VAlignment
type VTop    <: VAlignment end
type VCenter <: VAlignment end
type VBottom <: VAlignment end

const vtop    = VTop()
const vcenter = VCenter()
const vbottom = VBottom()

type TextForm <: FormType
    pos::Point
    value::String
    halign::HAlignment
    valign::VAlignment
end

function Text(x::MeasureOrNumber, y::MeasureOrNumber, value::String,
              halign::HAlignment, valign::VAlignment)
    Form(Property(),
         FormType[TextForm(Point(x_measure(x), y_measure(y)),
                           value, halign, valign)])
end


function Text(x::MeasureOrNumber, y::MeasureOrNumber, value::String)
    Text(x, y, value, hleft, hbottom)
end


function draw(backend::Backend, t::NativeTransform, unit_box::BoundingBox,
              box::NativeBoundingBox, form::TextForm)
    form = TextForm(native_measure(form.pos, t, unit_box, box, backend),
                    form.value, form.halign, form.valign)
    draw(backend, form)
end



