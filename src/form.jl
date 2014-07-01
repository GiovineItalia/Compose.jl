
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


function polygon()
    return PolygonPrimitive([PolygonPrimitive(Point[])])
end


function polygon{T <: XYTupleOrPoint}(points::AbstractArray{T})
    return Polygon([PolygonPrimitive([convert(Point, point)
                                      for point in points])])
end


function polygon(point_arrays::AbstractArray)
    polyprims = Array(PolygonPrimitive, length(point_arrays))
    for (i, point_array) in enumerate(point_arrays)
        polyprims[i] = PolygonPrimitive([convert(Point, point)
                                         for point in point_array])
    end
    return Polygon(polyprims)
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
    # SVG doesn't support negative width/height to indicate flipped axis,
    # so we have to adjust manually.
    corner = absolute_units(p.corner, t, units, box)
    width = absolute_units(p.width, t, units, box)
    height = absolute_units(p.height, t, units, box)

    return RectanglePrimitive(
        Point(Measure(abs=width < 0 ? corner.x.abs + width : corner.x.abs),
              Measure(abs=height < 0 ? corner.y.abs + height : corner.y.abs)),
        Measure(abs=abs(width)),
        Measure(abs=abs(height)))
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
    rot::Rotation
end

typealias Text Form{TextPrimitive}


function text(x, y, value::String,
              halign::HAlignment=hleft, valign::VAlignment=vbottom,
              rot=Rotation())
    return Text([TextPrimitive(Point(x, y), value, halign, valign, rot)])
end


function text(x, y, value,
              halign::HAlignment=hleft, valign::VAlignment=vbottom,
              rot=Rotation())
    return Text([TextPrimitive(Point(x, y), string(value), halign, valign, rot)])
end


function text(xs::AbstractArray, ys::AbstractArray, values::AbstractArray{String},
              haligns::AbstractArray=[hleft], valigns::AbstractArray=[vbottom],
              rots::AbstractArray=[Rotation()])
    return Text([TextPrimitive(Point(x, y), value, halign, valign, rot)
                 for (x, y, value, halign, valign, rot) in cyclezip(xs, ys, values, haligns, valigns, rots)])
end


function text(xs::AbstractArray, ys::AbstractArray, values::AbstractArray,
              haligns::AbstractArray=[hleft], valigns::AbstractArray=[vbottom],
              rots::AbstractArray=[Rotation()])
    return Text([TextPrimitive(Point(x, y), string(value), halign, valign, rot)
                 for (x, y, value, halign, valign, rot) in cyclezip(xs, ys, values, haligns, valigns, rots)])
end


function absolute_units(p::TextPrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return TextPrimitive(absolute_units(p.position, t, units, box),
                         p.value, p.halign, p.valign,
                         absolute_units(p.rot, t, units, box))
end


# Line
# ----

immutable LinePrimitive <: FormPrimitive
    points::Vector{Point}
end

typealias Line Form{LinePrimitive}


function line()
    return Line([LinePrimitive(Point[])])
end


function line{T <: XYTupleOrPoint}(points::AbstractArray{T})
    return Line([LinePrimitive([convert(Point, point) for point in points])])
end


function line(point_arrays::AbstractArray)
    lineprims = Array(LinePrimitive, length(point_arrays))
    for (i, point_array) in enumerate(point_arrays)
        lineprims[i] = LinePrimitive([convert(Point, point)
                                      for point in point_array])
    end
    return Line(lineprims)
end


function absolute_units(p::LinePrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return LinePrimitive([absolute_units(point, t, units, box) for point in p.points])
end


# Curve
# -----

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
                  for (anchor0, ctrl0, ctrl1, anchor1) in cyclezip(anchor0s, ctrl0s, ctrl1s, anchor1s)])
end


function absolute_units(p::CurvePrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return CurvePrimitive(absolute_units(p.anchor0, t, units, box),
                          absolute_units(p.ctrl0, t, units, box),
                          absolute_units(p.ctrl1, t, units, box),
                          absolute_units(p.anchor1, t, units, box))
end


# Bitmap
# ------

immutable BitmapPrimitive <: FormPrimitive
    mime::String
    data::Vector{Uint8}
    corner::Point
    width::Measure
    height::Measure
end

typealias Bitmap Form{BitmapPrimitive}


function bitmap(mime::String, data::Vector{Uint8}, x0, y0, width, height)
    corner = Point(x0, y0)
    width = x_measure(width)
    height = y_measure(height)
    return Bitmap([BitmapPrimitive(mime, data, corner, width, height)])
end


function bitmap(mimes::AbstractArray, datas::AbstractArray,
                x0s::AbstractArray, y0s::AbstractArray,
                widths::AbstractArray, heights::AbstractArray)
    return Bitmap([BitmapPrimitive(mime, data, x0, y0, x_measure(width), y_measure(height))
                   for (mime, data, x0, y0, width, height) in cyclezip(mimes, datas, x0s, y0s, widths, heights)])
end


function absolute_units(p::BitmapPrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return BitmapPrimitive(p.mime, p.data,
                           absolute_units(p.corner, t, units, box),
                           Measure(abs=absolute_units(p.width, t, units, box)),
                           Measure(abs=absolute_units(p.width, t, units, box)))
end


