# Form: a thing that is visible once drawn.

export polygon, rectangle, lines, curve, arc, circle, ellipse, text,
       hcenter, hleft, hright, vcenter, vtop, vbottom, empty_form

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

contents(io, f::EmptyForm, n::Int, indent) = nothing
length(f::EmptyForm) = 0


# A non-empty sequence of form primitives each with an associated (possibly
# empty) property.
type FormTree <: Form
    primitive::Union(Nothing, FormPrimitive)
    property::Property
    children::List{Form}

    function FormTree(primitive::Union(Nothing, FormPrimitive),
                      property::Property,
                      children::List{Form})
        new(primitive, property, convert(List{Form}, children))
    end

    function FormTree(primitive::Union(Nothing, FormPrimitive),
                      property::Property)
        new(primitive, property, ListNil{Form}())
    end

    function FormTree(primitive::FormPrimitive)
        new(primitive, empty_property, ListNil{Form}())
    end

    # shallow copy constructor
    function FormTree(a::FormTree)
        new(a.primitive, a.property, a.children)
    end
end
length(f::FormTree) = length(f.children)

function contents(io, f::FormTree, n::Int, indent)
    if !(f.primitive === nothing)
        contents(io, f.primitive, 1, indent)
    end
    i = 1
    for fc in f.children
        contents(io, fc, n - 1, string(indent, "  "))
        if i > 10
            println(io, indent, "  ...")
            break
        end
        i += 1
    end
end

copy(a::FormTree) = FormTree(a)

children(a::FormTree) = a.children


function removable(a::FormTree)
    a.primitive === nothing && a.property === empty_property
end


# Conceptually, combining forms is tree joining by introducing a new root.
#
#   a      b               c
#  / \    / \    --->     / \
# X   Y  U   V           a   b
#                       / \ / \
#                      X  Y U  V
#
# There is a trick here to avoid an exceess of nop or "removable" nodes.
#
function combine(forms::Form...)
    children = ListNil{Form}()
    for form in forms
        if form === empty_form
            continue
        end
        if removable(form)
            for child in form.children
                children = cons(child, children)
            end
        else
            children = cons(form, children)
        end
    end

    FormTree(nothing, empty_property, children)
end


# Composition of properties into forms
function compose(a::FormTree, b::Property)
    FormTree(a.primitive, combine(a.property, b), a.children)
end


function compose(a::EmptyForm, b::Property)
    a
end


# Nop drawing of empty forms.
function draw(backend::Backend,
              t::Transform,
              unit_box::UnitBox,
              box::AbsoluteBoundingBox,
              root_form::EmptyForm)
end


# Does a property in a node apply to it's siblings? No!
function draw(backend::Backend,
              t::Transform,
              unit_box::UnitBox,
              box::AbsoluteBoundingBox,
              root_form::FormTree)

    S = {root_form}

    while !isempty(S)
        form = pop!(S)

        if form === :POP_PROPERTY
            pop_property(backend)
        elseif form === empty_form
            continue
        else
            if !is(form.property, empty_property)
                push!(S, :POP_PROPERTY)
                push_property(backend,
                              absolute_units(form.property, t, unit_box, box))
            end

            for child in children(form)
                push!(S, child)
            end

            if typeof(form) === FormTree
                if !(form.primitive === nothing)
                    draw(backend, t, unit_box, box, form.primitive)
                end
            else
                draw(backend, t, unit_box, box, form)
            end
        end
    end
end


immutable Lines <: FormPrimitive
    points::Vector{Point}

    function Lines(points::Vector{Point})
        new(points)
    end

    function Lines(points::Point...)
        new(Point[point for point in points])
    end
end


function contents(io, f::Lines, n::Int, indent)
    println(io, indent, "Line with ", length(f.points), " points")
end


function lines()
    empty_form
end


function lines(points::XYTupleOrPoint...)
    FormTree(Lines([convert(Point, point) for point in points]))
end


function draw(backend::Backend, t::Transform, unit_box::UnitBox,
              box::AbsoluteBoundingBox, form::Lines)
    native_form = Lines([absolute_units(point, t, unit_box, box)
                         for point in form.points])
    draw(backend, native_form)
end


immutable Curve <: FormPrimitive
    anchor0::Point
    ctrl0::Point
    ctrl1::Point
    anchor1::Point
end


function contents(io, f::Curve, n::Int, indent)
    println(io, indent, "Curve between ", f.anchor0, " and ", f.anchor1)
end


function curve(anchor0::XYTupleOrPoint, ctrl0::XYTupleOrPoint,
               ctrl1::XYTupleOrPoint, anchor1::XYTupleOrPoint)
    FormTree(Curve(convert(Point, anchor0), convert(Point, ctrl0),
                   convert(Point, ctrl1), convert(Point, anchor1)),
             fill(nothing))
end


function draw(backend::Backend, t::Transform, unit_box::UnitBox,
              box::AbsoluteBoundingBox, form::Curve)
    native_form = Curve(absolute_units(form.anchor0, t, unit_box, box),
                        absolute_units(form.ctrl0, t, unit_box, box),
                        absolute_units(form.ctrl1, t, unit_box, box),
                        absolute_units(form.anchor1, t, unit_box, box))
    draw(backend, native_form)
end


immutable Polygon <: FormPrimitive
    points::Vector{Point}
end


function contents(io, f::Polygon, n::Int, indent)
    println(io, indent, "Polygon with ", length(f.points), " points")
end


function polygon(points::XYTupleOrPoint...)
    FormTree(Polygon([convert(Point, point) for point in points]))
end


function draw(backend::Backend, t::Transform, unit_box::UnitBox,
              box::AbsoluteBoundingBox, form::Polygon)
    native_form = Polygon([absolute_units(point, t, unit_box, box)
                           for point in form.points])
    draw(backend, native_form)
end


function rectangle(x0, y0, width, height)
    x0 = x_measure(x0)
    y0 = y_measure(y0)
    width = x_measure(width)
    height = y_measure(height)
    xy0 = Point(x0, y0)
    xy1 = Point(x0 + width, y0 + height)
    polygon(xy0, Point(xy1.x, xy0.y), xy1, Point(xy0.x, xy1.y))
end


function rectangle()
    rectangle(0.0w, 0.0h, 1.0w, 1.0h)
end


immutable Ellipse <: FormPrimitive
    center::Point
    x_point::Point
    y_point::Point

    function Ellipse(center::Point, x_point::Point, y_point::Point)
        new(center, x_point, y_point)
    end

    function Ellipse(x, y, rx, ry)
        x = x_measure(x)
        y = y_measure(y)
        new(Point(x, y),
            Point(x + x_measure(rx), y),
            Point(x, y + y_measure(ry)))
    end
end


function contents(io, f::Ellipse, n::Int, indent)
    println(io, indent, "Ellipse centered at ", f.center)
end


function ellipse(x, y, x_radius, y_radius)
    x = x_measure(x)
    y = y_measure(y)
    FormTree(Ellipse(Point(x, y),
                     Point(x + x_measure(x_radius), y),
                     Point(x, y + y_measure(y_radius))))
end


function ellipse()
    ellipse(0.5w, 0.5h, 0.5w, 0.5h)
end


# Despite the innacuracy, this is convenient is a few places.
typealias Circle Ellipse


function circle(x, y, radius)
    ellipse(x, y, radius, radius)
end


function draw(backend::Backend, t::Transform, unit_box::UnitBox,
              box::AbsoluteBoundingBox, form::Ellipse)
    native_form = Ellipse(
        absolute_units(form.center, t, unit_box, box),
        absolute_units(form.x_point, t, unit_box, box),
        absolute_units(form.y_point, t, unit_box, box))
    draw(backend, native_form)
end


abstract HAlignment
immutable HLeft   <: HAlignment end
immutable HCenter <: HAlignment end
immutable HRight  <: HAlignment end

const hleft   = HLeft()
const hcenter = HCenter()
const hright  = HRight()

abstract VAlignment
immutable VTop    <: VAlignment end
immutable VCenter <: VAlignment end
immutable VBottom <: VAlignment end

const vtop    = VTop()
const vcenter = VCenter()
const vbottom = VBottom()

immutable Text <: FormPrimitive
    pos::Point
    value::String
    halign::HAlignment
    valign::VAlignment

    # Text forms need their own rotation field unfortunately, since there is no
    # way to give orientation with just a position point.
    t::Transform
end

function text(x, y, value::String,
              halign::HAlignment, valign::VAlignment)
    FormTree(Text(Point(x_measure(x), y_measure(y)),
                  value, halign, valign, Transform()))
end


function text(x, y, value::String)
    text(x, y, value, hleft, vbottom)
end


function draw(backend::Backend, t::Transform, unit_box::UnitBox,
              box::AbsoluteBoundingBox, form::Text)
    form = Text(absolute_units(form.pos, t, unit_box, box),
                form.value, form.halign, form.valign, t)

    draw(backend, form)
end

function contents(io, f::Text, n::Int, indent)
    println(io, indent, "Text: ", f.value)
end

