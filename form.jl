# Form: a thing that is visible once drawn.

require("backend.jl")
require("property.jl")
require("measure.jl")

# A primitive form: typcially a shape or sequence of drawing operations.
abstract FormPrimitive


# Form is a (possibly empty) binary tree. Each node is a form primitive
# with an associated property. When the form is rendered, the graph is
# traversed from the root. The property at a node is applied to all downstream
# nodes (children, grandchildren, etc).
abstract Form


# The empty form, which forms the identity element of the Form monoid.
type EmptyForm <: Form end
const empty_form = EmptyForm()


# A non-empty sequence of form primitives each with an associated (possibly
# empty) property.
type FormTree <: Form
    primitive::Union(Nothing, FormPrimitive)
    property::Property
    child::Form
    sibling::Form

    function FormTree(primitive::Union(Nothing, FormPrimitive),
                      property::Property,
                      child::Property,
                      sibling::Property)
        new(primitive, property, child, sibling)
    end

    function FormTree(primitive::FormPrimitive)
        new(primitive, empty_property, empty_form, empty_form)
    end
end


compose(a::EmptyForm, b::EmptyForm) = a
compose(a::FormTree, b::EmptyForm) = a
compose(a::EmptyForm, b::FormTree) = b


function compose(a::FormTree, b::FormTree)
    if a.primitive === nothing && a.property === empty_property
        if b.primitive === nothing && b.property === empty_property
            tmp = a.child
            b = copy(b)
            while !is(b.next, nothing)
                b.next = copy(b.next)
                b = b.next
            end
            b.next = tmp
            a = copy(a)
            a.child = b
        else
            b = copy(b)
            b.sibling = a.child
            a = copy(a)
            a.child = b
        end
        a
    else
        a = copy(a)
        b = copy(b)
        a.sibling = b
        b.sibling = empty_form
        FormTree(nothing, empty_property, a, empty_form)
    end
end


# Does a property in a node apply to it's siblings? No!
function draw(backend::Backend,
              t::NativeTransform,
              unit_box::BoundingBox,
              box::NativeBoundingBox,
              root_form::Form)
    S = {root_form}

    while !isempty(S)
        form = pop(S)

        if form === :POP_PROPERTY
            pop_property(backend)
        else
            u = form.sibling
            while !is(u, empty_form)
                push(S, u)
                u = u.sibling
            end

            if !is(form.property, empty_property)
                push(S, :POP_PROPERTY)
                push_property(backend,
                              native_measure(form.property, t, unit_box,
                                             box, backend))
            end

            if !is(form.child, empty_form)
                push(S, form.child)
            end

            draw(backend, t, unit_box, box, form.primitive)
        end
    end
end


type Lines <: FormPrimitive
    points::Vector{Point}
end


function lines(points::XYTupleOrPoint...)
    FormTree(LinesForm([convert(Point, point) for point in points]))
end


function draw(backend::Backend, t::NativeTransform, unit_box::BoundingBox,
              box::NativeBoundingBox, form::Lines)
    native_form = Lines([native_measure(point, t, unit_box, box, backend)
                         for point in form.points])
    draw(backend, native_form)
end


type Polygon <: FormPrimitive
    points::Vector{Point}

end


function polygon(points::XYTupleOrPoint...)
    FormTree(PolygonForm([convert(Point, point) for point in points]))
end


function draw(backend::Backend, t::NativeTransform, unit_box::BoundingBox,
              box::NativeBoundingBox, form::Polygon)
    native_form = Polygon([native_measure(point, t, unit_box, box, backend)
                           for point in form.points])
    draw(backend, native_form)
end


function cectangle(x0::MeasureOrNumber, y0::MeasureOrNumber,
                   width::MeasureOrNumber, height::MeasureOrNumber)

    xy0 = Point(x0, y0)
    xy1 = Point(x0 + width, y0 + height)
    polygon(xy0, Point(xy1.x, xy0.y), xy1, Point(xy0.x, xy1.y))
end


function rectangle()
    rectangle(0.0w, 0.0h, 1.0w, 1.0h)
end


type Ellipse <: FormPrimitive
    center::Point
    x_point::Point
    y_point::Point
end


function Ellipse(x::MeasureOrNumber, y::MeasureOrNumber,
                 x_radius::MeasureOrNumber, y_radius::MeasureOrNumber)
    x = x_measure(x)
    y = y_measure(y)
    FormTree(EllipseForm(Point(x, y),
                         Point(x + x_measure(x_radius), y),
                         Point(x, y + y_measure(y_radius))))
end


function ellipse()
    ellipse(1/2w, 1/2h, 1/2w, 1/2h)
end


function circle(x::MeasureOrNumber, y::MeasureOrNumber, radius::MeasureOrNumber)
    ellipse(x, y, radius, radius)
end


function draw(backend::Backend, t::NativeTransform, unit_box::BoundingBox,
              box::NativeBoundingBox, form::Ellipse)
    native_form = ellipse(
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

type Text <: FormPrimitive
    pos::Point
    value::String
    halign::HAlignment
    valign::VAlignment
end

function text(x::MeasureOrNumber, y::MeasureOrNumber, value::String,
              halign::HAlignment, valign::VAlignment)
    FormTree(Text(Point(x_measure(x), y_measure(y)),
                  value, halign, valign))
end


function text(x::MeasureOrNumber, y::MeasureOrNumber, value::String)
    Text(x, y, value, hleft, hbottom)
end


function draw(backend::Backend, t::NativeTransform, unit_box::BoundingBox,
              box::NativeBoundingBox, form::Text)
    form = Text(native_measure(form.pos, t, unit_box, box, backend),
                form.value, form.halign, form.valign)
    draw(backend, form)
end


