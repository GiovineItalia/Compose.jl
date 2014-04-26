
# A form is something that ends up as geometry in the graphic.

abstract FormPrimitive

immutable Form{P <: FormPrimitive} <: ComposeNode
    primitives::Vector{P}
end


function isempty(f::Form)
    return isempty(f.primitives)
end


function isscalar(f::Form)
    return length(f.primitives) == 1
end


function draw{P}(backend::Backend, t::Transform, units::UnitBox,
                 box::AbsoluteBoundingBox, form::Form{P})
    draw(backend, Form(P[absolute_units(primitive, t, units, box)
                         for primitive in form.primitives]))
end


# Polygon
# -------

immutable PolygonPrimitive <: FormPrimitive
    points::Vector{Point}
end

typealias Polygon Form{PolygonPrimitive}


function polygon(points::XYTupleOrPoint...)
    return Polygon([PolygonPrimitive([convert(Point, point) for point in points])])
end


function polygon(point_arrays::AbstractArray...)
    polys = Array(PolygonPrimitive, 0)
    for point_array in point_arrays
        push!(polys, PolygonPrimitive([convert(Point, point) for point in point_array]))
    end
    return Polygon(polys)
end


function absolute_units(p::PolygonPrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return PolygonPrimitive(Point[absolute_units(point, t, units, box)
                                  for point in p.points])
end


# Rectangle
# ---------

immutable RectanglePrimitive <: FormPrimitive
    corner::Point
    width::Measure
    height::Measure
end

typealias Rectangle Form{RectanglePrimitive}


function rectangle()
    return Rectangle([RectanglePrimitive(Point(0.0w, 0.0h), 1.0w, 1.0h)])
end


function rectangle(x0, y0, width, height)
    corner = Point(x0, y0)
    width = x_measure(width)
    height = y_measure(height)
    return Rectangle([RectanglePrimitive(corner, width, height)])
end


function rectangle(x0s::AbstractArray, y0s::AbstractArray,
                   widths::AbstractArray, heights::AbstractArray)
    return Rectangle([RectanglePrimitive(
                         Point(x0, y0),
                         x_measure(width), y_measure(height))
                      for (x0, y0, width, height) in cyclezip(x0s, y0s, widths, heights)])
end


function absolute_units(p::RectanglePrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return RectanglePrimitive(absolute_units(p.corner, t, units, box),
                              Measure(abs=absolute_units(p.width, t, units, box)),
                              Measure(abs=absolute_units(p.height, t, units, box)))
end


# Circle
# ------

immutable CirclePrimitive <: FormPrimitive
    center::Point
    radius::Measure
end


typealias Circle Form{CirclePrimitive}


function circle()
    return Circle([CirclePrimitive(Point(0.5w, 0.5h), 0.5w)])
end


function circle(x, y, r)
    return Circle([CirclePrimitive(Point(x, y), x_measure(r))])
end


function circle(xs::AbstractArray, ys::AbstractArray, rs::AbstractArray)
    return Circle([CirclePrimitive(Point(x, y), x_measure(r))
                   for (x, y, r) in cyclezip(xs, ys, rs)])
end


function absolute_units(p::CirclePrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return CirclePrimitive(absolute_units(p.center, t, units, box),
                           Measure(abs=absolute_units(p.radius, t, units, box)))
end


# Ellipse
# -------


immutable EllipsePrimitive <: FormPrimitive
    center::Point
    x_point::Point
    y_point::Point
end

typealias Ellipse Form{EllipsePrimitive}


function ellipse()
    return Ellipse([EllipsePrimitive(Point(0.5w, 0.5h),
                                     Point(1.0w, 0.5h),
                                     Point(0.5w, 1.0h))])
end


function ellipse(x, y, x_radius, y_radius)
    return Ellipse([EllipsePrimitive(Point(x, y),
                                     Point(x + x_measure(x_radius), y),
                                     Point(x, y + y_measure(y_radius)))])
end


function ellipse(xs::AbstractArray, ys::AbstractArray,
                 x_radiuses::AbstractArray, y_radiuses::AbstractArray)
    return Ellipse[EllipsePrimitive(Point(x, y),
                                    Point(x_measure(x) + x_measure(x_radius), y),
                                    Point(x, y_measure(y) + y_measure(y_radius)))
                   for (x, y, x_radius, y_radius) in cyclezip(xs, ys, x_radiuses, y_radiuses)]
end


function absolute_units(p::EllipsePrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return EllipsePrimitive(absolute_units(p.center, t, units, box),
                            absolute_units(p.x_point, t, units, box),
                            absolute_units(p.y_point, t, units, box))
end



# Text
# ----

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


immutable TextPrimitive <: FormPrimitive
    position::Point
    value::String
    halign::HAlignment
    valign::VAlignment

    # Text forms need their own rotation field unfortunately, since there is no
    # way to give orientation with just a position point.
    t::Transform
end

typealias Text Form{TextPrimitive}


function text(x, y, value::String,
              halign::HAlignment=hleft, valign::VAlignment=vbottom)
    return Text([TextPrimitive(Point(x, y), value, halign, valign, Transform())])
end


function text(xs::AbstractArray, ys::AbstractArray, values::AbstractArray,
              haligns::AbstractArray=[hleft], valigns::AbstractArray=[vbottom])
    return Text([TextPrimitive(Point(x, y), value, halign, valign, Transform())
                 for (x, y, value, halign, valign)
                 in cyclezip(xs, ys, values, halign, valigns)])
end


function absolute_units(p::TextPrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return TextPrimitive(absolute_units(p.position, t, units, box),
                         p.value, p.halign, p.valign, t)
end


# Lines
# -----

immutable LinesPrimitive <: FormPrimitive
    points::Vector{Point}
end

typealias Lines Form{LinesPrimitive}


function lines(points::XYTupleOrPoint...)
    return Lines([LinesPrimitive([convert(Point, point) for point in points])])
end


function polygon(point_arrays::AbstractArray...)
    linesprims = Array(LinesPrimitive, 0)
    for point_array in point_arrays
        push!(linesprims, LinesPrimitive([convert(Point, point) for point in point_array]))
    end
    return Lines(polys)
end


function absolute_units(p::LinesPrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return LinesPrimitive([absolute_units(point, t, units, box) for point in p.points])
end


# Curve
# -----

# Quadratic Bezier curve
immutable CurvePrimitive <: FormPrimitive
    anchor0::Point
    ctrl0::Point
    ctrl1::Point
    anchor1::Point
end

typealias Curve Form{CurvePrimitive}


function curve(anchor0::XYTupleOrPoint, ctrl0::XYTupleOrPoint,
               ctrl1::XYTupleOrPoint, anchor1::XYTupleOrPoint)
    return Curve([CurvePrimitive(convert(Point, ancho0), convert(Point, ctrl0),
                                 convert(Point, ctrl1), convert(Point, anchor1))])
end


function curve(anchor0s::AbstractArray, ctrl0s::AbstractArray,
               ctrl1s::AbstractArray, anchor1s::AbstractArray)
    return Curve([CurvePrimitive(convert(Point, anchor0), convert(Point, ctrl0),
                                 convert(Point, ctrl1), convert(Point, anchor1))
                  for (anchor0, ctrl0, ctrl1, anchor1)
                  in cyclezip(anchor0s, ctrl0s, ctrl1s, anchor1s)])
end


function absolute_units(p::CurvePrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return CurvePrimitive(absolute_units(p.anchor0, t, units, box),
                          absolute_units(p.ctrl0, t, units, box),
                          absolute_units(p.ctrl1, t, units, box),
                          absolute_units(p.anchor1, t, units, box))
end


