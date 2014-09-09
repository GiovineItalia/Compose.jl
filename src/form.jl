
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

function boundingbox(form::PolygonPrimitive, linewidth::Measure,
                     font::String, fontsize::Measure)
    x0 = minimum([p.x for p in form.points])
    x1 = maximum([p.x for p in form.points])
    y0 = minimum([p.y for p in form.points])
    y1 = maximum([p.y for p in form.points])
    return BoundingBox(x0 - linewidth,
                       y0 - linewidth,
                       x1 - x0 + linewidth,
                       y1 - y0 + linewidth)
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
    primitives = @makeprimitives RectanglePrimitive,
                    (x0 in x0s, y0 in y0s, width in widths, height in heights),
                    RectanglePrimitive(Point(x0, y0),
                                       x_measure(width), y_measure(height))
    return Rectangle(primitives)
end


function absolute_units(p::RectanglePrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    # SVG doesn't support negative width/height to indicate flipped axis,
    # so we have to adjust manually.
    corner = absolute_units(p.corner, t, units, box)
    width = absolute_units(p.width, t, units, box)
    height = absolute_units(p.height, t, units, box)

    return RectanglePrimitive(
        Point(Measure(width < 0 ? corner.x.abs + width : corner.x.abs),
              Measure(height < 0 ? corner.y.abs + height : corner.y.abs)),
        Measure(abs(width)),
        Measure(abs(height)))
end


function boundingbox(form::RectanglePrimitive, linewidth::Measure,
                     font::String, fontsize::Measure)

    return BoundingBox(form.corner.x - linewidth,
                       form.corner.y - linewidth,
                       form.width + 2*linewidth,
                       form.height + 2*linewidth)
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
    return Circle(
        @makeprimitives CirclePrimitive,
            (x in xs, y in ys, r in rs),
            CirclePrimitive(Point(x, y), x_measure(r)))
end


function absolute_units(p::CirclePrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return CirclePrimitive(absolute_units(p.center, t, units, box),
                           Measure(absolute_units(p.radius, t, units, box)))
end

function boundingbox(form::CirclePrimitive, linewidth::Measure,
                     font::String, fontsize::Measure)
    return BoundingBox(form.center.x - form.radius - linewidth,
                       form.center.y - form.radius - linewidth,
                       2 * (form.radius + linewidth),
                       2 * (form.radius + linewidth))
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
    return Ellipse(
        @makeprimitives EllipsePrimitive,
            (x in xs, y in ys, x_radius in x_radiuses, y_radius in y_radiuses),
            EllipsePrimitive(Point(x, y),
                             Point(x_measure(x) + x_measure(x_radius), y),
                             Point(x, y_measure(y) + y_measure(y_radius))))
end


function absolute_units(p::EllipsePrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return EllipsePrimitive(absolute_units(p.center, t, units, box),
                            absolute_units(p.x_point, t, units, box),
                            absolute_units(p.y_point, t, units, box))
end

function boundingbox(form::EllipsePrimitive, linewidth::Measure,
                     font::String, fontsize::Measure)
    x0 = min(form.x_point.x, form.y_point.x)
    x1 = max(form.x_point.x, form.y_point.x)
    y0 = min(form.x_point.y, form.y_point.y)
    y1 = max(form.x_point.y, form.y_point.y)
    xr = x1 - x0
    yr = y1 - y0
    return BoundingBox(x0 - linewidth - xr,
                       y0 - linewidth - yr,
                       2 * (xr + linewidth),
                       2 * (yr + linewidth))
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
    return Text(
        @makeprimitives TextPrimitive,
            (x in xs, y in ys, value in values, halign in haligns, valign in valigns, rot in rots),
            TextPrimitive(Point(x, y), value, halign, valign, rot))
end


function text(xs::AbstractArray, ys::AbstractArray, values::AbstractArray,
              haligns::AbstractArray=[hleft], valigns::AbstractArray=[vbottom],
              rots::AbstractArray=[Rotation()])
    return Text(
        @makeprimitives TextPrimitive,
            (x in xs, y in ys, value in values, halign in haligns, valign in valigns, rot in rots),
            TextPrimitive(Point(x, y), string(value), halign, valign, rot))
end


function absolute_units(p::TextPrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return TextPrimitive(absolute_units(p.position, t, units, box),
                         p.value, p.halign, p.valign,
                         absolute_units(p.rot, t, units, box))
end

function boundingbox(form::TextPrimitive, linewidth::Measure,
                     font::String, fontsize::Measure)

    width, height = text_extents(font, fontsize, form.value)[1]

    if form.halign == hleft
        x0 = form.position.x
    elseif form.halign == hcenter
        x0 = form.position.x - width/2
    elseif form.halign == hright
        x0 = form.position.x - width
    end

    if form.valign == vbottom
        y0 = form.position.y - height
    elseif form.valign == vcenter
        y0 = form.position.y - height/2
    elseif form.valign == vtop
        y0 = form.position.y
    end

    return BoundingBox(x0 - linewidth,
                       y0 - linewidth,
                       width + linewidth,
                       height + linewidth)
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


function boundingbox(form::LinePrimitive, linewidth::Measure,
                     font::String, fontsize::Measure)
    x0 = minimum([p.x for p in form.points])
    x1 = maximum([p.x for p in form.points])
    y0 = minimum([p.y for p in form.points])
    y1 = maximum([p.y for p in form.points])
    return BoundingBox(x0 - linewidth,
                       y0 - linewidth,
                       x1 - x0 + linewidth,
                       y1 - y0 + linewidth)
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
    return Curve([CurvePrimitive(convert(Point, anchor0), convert(Point, ctrl0),
                                 convert(Point, ctrl1), convert(Point, anchor1))])
end


function curve(anchor0s::AbstractArray, ctrl0s::AbstractArray,
               ctrl1s::AbstractArray, anchor1s::AbstractArray)
    return Curve(
        @makeprimitives CurvePrimitive,
            (anchor0 in archor0s, ctrl0 in ctrl0s, ctrl1 in ctrl1s, anchor1 in anchor1s),
            CurvePrimitive(convert(Point, anchor0), convert(Point, ctrl0),
                           convert(Point, ctrl1), convert(Point, anchor1)))
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
    return Bitmap(
        @makeprimitives BitmapPrimitive,
            (mime in mimes, data in datas, x0 in x0s, y0 in y0s, width in widths, height in heigths),
            BitmapPrimitive(mime, data, x0, y0, x_measure(width), y_measure(height)))
end


function absolute_units(p::BitmapPrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return BitmapPrimitive(p.mime, p.data,
                           absolute_units(p.corner, t, units, box),
                           Measure(absolute_units(p.width, t, units, box)),
                           Measure(absolute_units(p.height, t, units, box)))
end


# Path
# ----

# An implementation of the SVG path mini-language.

abstract PathOp

immutable MoveAbsPathOp <: PathOp
    to::Point
end

function assert_pathop_tokens_len(op_type, tokens, i, needed)
    provided = length(tokens) - i + 1
    if provided < needed
        error("In path $(op_type) requires $(needed) argumens but only $(provided) provided.")
    end
end

function parsepathop(::Type{MoveAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(MoveAbsPathOp, tokens, i, 2)
    op = MoveAbsPathOp(Point(tokens[i], tokens[i + 1]))
    return (op, i + 2)
end

function absolute_units(p::MoveAbsPathOp, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return MoveAbsPathOp(absolute_units(p.to, t, units, box))
end

immutable MoveRelPathOp <: PathOp
    to::Point
end

function parsepathop(::Type{MoveRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(MoveRelPathOp, tokens, i, 2)
    op = MoveRelPathOp(Point(tokens[i], tokens[i + 1]))
    return (op, i + 2)
end

function absolute_units(p::MoveRelPathOp, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return MoveRelPathOp(Point(Measure(absolute_units(p.to.x, t, units, box)),
                               Measure(absolute_units(p.to.y, t, units, box))))
end


immutable ClosePathOp <: PathOp
end

function parsepathop(::Type{ClosePathOp}, tokens::AbstractArray, i)
    return (ClosePathOp(), i)
end

function absolute_units(p::ClosePathOp, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return p
end


immutable LineAbsPathOp <: PathOp
    to::Point
end

function parsepathop(::Type{LineAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(LineAbsPathOp, tokens, i, 2)
    op = LineAbsPathOp(Point(tokens[i], tokens[i + 1]))
    return (op, i + 2)
end

function absolute_units(p::LineAbsPathOp, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return LineAbsPathOp(absolute_units(p.to, t, units, box))
end


immutable LineRelPathOp <: PathOp
    to::Point
end

function parsepathop(::Type{LineRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(LineRelPathOp, tokens, i, 2)
    op = LineRelPathOp(Point(tokens[i], tokens[i + 1]))
    return (op, i + 2)
end

function absolute_units(p::LineRelPathOp, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return LineRelPathOp(Point(Measure(absolute_units(p.to.x, t, units, box)),
                               Measure(absolute_units(p.to.y, t, units, box))))
end


immutable HorLineAbsPathOp <: PathOp
    x::Measure
end

function parsepathop(::Type{HorLineAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(HorLineAbsPathOp, tokens, i, 1)
    op = HorLineAbsPathOp(x_measure(tokens[i]))
    return (op, i + 1)
end

function absolute_units(p::HorLineAbsPathOp, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return HorLineAbsPathOp(Measure(absolute_x_position(p.x, t, units, box)))
end


immutable HorLineRelPathOp <: PathOp
    Δx::Measure
end

function parsepathop(::Type{HorLineRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(HorLineRelPathOp, tokens, i, 1)
    op = HorLineRelPathOp(x_measure(tokens[i]))
    return (op, i + 1)
end

function absolute_units(p::HorLineRelPathOp, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return HorLineRelPathOp(Measure(absolute_units(p.Δx, t, units, box)))
end


immutable VertLineAbsPathOp <: PathOp
    y::Measure
end

function parsepathop(::Type{VertLineAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(VertLineAbsPathOp, tokens, i, 1)
    op = VertLineAbsPathOp(y_measure(tokens[i]))
    return (op, i + 1)
end

function absolute_units(p::VertLineAbsPathOp, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return VertLineAbsPathOp(Measure(absolute_y_position(p.x, t, units, box)))
end


immutable VertLineRelPathOp <: PathOp
    Δy::Measure
end

function parsepathop(::Type{VertLineRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(VertLineRelPathOp, tokens, i, 1)
    op = VertLineRelPathOp(y_measure(tokens[i]))
    return (op, i + 1)
end

function absolute_units(p::VertLineRelPathOp, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return VertLineRelPathOp(Measure(absolute_units(p.Δy, t, units, box)))
end


immutable CubicCurveAbsPathOp <: PathOp
    ctrl1::Point
    ctrl2::Point
    to::Point
end

function parsepathop(::Type{CubicCurveAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(CubicCurveAbsPathOp, tokens, i, 6)
    op = CubicCurveAbsPathOp(Point(tokens[i],     tokens[i + 1]),
                             Point(tokens[i + 2], tokens[i + 3]),
                             Point(tokens[i + 4], tokens[i + 5]))
    return (op, i + 6)
end

function absolute_units(p::CubicCurveAbsPathOp, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return CubicCurveAbsPathOp(
            absolute_units(p.ctrl1, t, units, box),
            absolute_units(p.ctrl2, t, units, box),
            absolute_units(p.to, t, units, box))
end


immutable CubicCurveRelPathOp <: PathOp
    ctrl1::Point
    ctrl2::Point
    to::Point
end

function parsepathop(::Type{CubicCurveRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(CubicCurveRelPathOp, tokens, i, 6)
    op = CubicCurveRelPathOp(Point(tokens[i],     tokens[i + 1]),
                             Point(tokens[i + 2], tokens[i + 3]),
                             Point(tokens[i + 4], tokens[i + 5]))
    return (op, i + 6)
end

function absolute_units(p::CubicCurveRelPathOp, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return CubicCurveRelPathOp(
            Point(Measure(absolute_units(p.ctrl1.x, t, units, box)),
                  Measure(absolute_units(p.ctrl1.y, t, units, box))),
            Point(Measure(absolute_units(p.ctrl2.x, t, units, box)),
                  Measure(absolute_units(p.ctrl2.y, t, units, box))),
            Point(Measure(absolute_units(p.to.x, t, units, box)),
                  Measure(absolute_units(p.to.y, t, units, box))))
end


immutable QuadCurveAbsPathOp <: PathOp
    ctrl1::Point
    to::Point
end

function parsepathop(::Type{QuadCurveAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(QuadCurveAbsPathOp, tokens, i, 4)
    op = QuadCurveAbsPathOp(Point(tokens[i],     tokens[i + 1]),
                            Point(tokens[i + 2], tokens[i + 3]))
    return (op, i + 4)
end

function absolute_units(p::QuadCurveAbsPathOp, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return QuadCurveAbsPathOp(
            absolute_units(p.ctrl1, t, units, box),
            absolute_units(p.to, t, units, box))
end


immutable QuadCurveRelPathOp <: PathOp
    ctrl1::Point
    to::Point
end

function parsepathop(::Type{QuadCurveRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(QuadCurveRelPathOp, tokens, i, 4)
    op = QuadCurveRelPathOp(Point(tokens[i],     tokens[i + 1]),
                            Point(tokens[i + 2], tokens[i + 3]))
    return (op, i + 4)
end

function absolute_units(p::QuadCurveRelPathOp, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return QuadCurveRelPathOp(
            Point(Measure(absolute_units(p.ctrl1.x, t, units, box)),
                  Measure(absolute_units(p.ctrl1.y, t, units, box))),
            Point(Measure(absolute_units(p.to.x, t, units, box)),
                  Measure(absolute_units(p.to.y, t, units, box))))
end


immutable QuadCurveShortAbsPathOp <: PathOp
    to::Point
end

function parsepathop(::Type{QuadCurveShortAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(QuadCurveShortAbsPathOp, tokens, i, 2)
    op = QuadCurveShortAbsPathOp(Point(tokens[i], tokens[i + 1]))
    return (op, i + 2)
end

function absolute_units(p::QuadCurveShortAbsPathOp, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return QuadCurveShortAbsPathOp(
            absolute_units(p.to, t, units, box))
end


immutable QuadCurveShortRelPathOp <: PathOp
    to::Point
end

function parsepathop(::Type{QuadCurveShortRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(QuadCurveShortRelPathOp, tokens, i, 2)
    op = QuadCurveShortRelPathOp(Point(tokens[i], tokens[i + 1]))
    return (op, i + 2)
end

function absolute_units(p::QuadCurveRelPathOp, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return QuadCurveRelPathOp(
            Point(Measure(absolute_units(p.to.x, t, units, box)),
                  Measure(absolute_units(p.to.y, t, units, box))))
end


immutable ArcAbsPathOp <: PathOp
    rx::Measure
    ry::Measure
    rotation::Float64
    largearc::Bool
    sweep::Bool
    to::Point
end

function absolute_units(p::ArcAbsPathOp, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return ArcAbsPathOp(
        Measure(absolute_units(p.rx, t, units, box)),
        Measure(absolute_units(p.ry, t, units, box)),
        p.rotation,
        p.largearc,
        p.sweep,
        absolute_units(p.to, t, units, box))
end


immutable ArcRelPathOp <: PathOp
    rx::Measure
    ry::Measure
    rotation::Float64
    largearc::Bool
    sweep::Bool
    to::Point
end

function parsepathop{T <: Union(ArcAbsPathOp, ArcRelPathOp)}(::Type{T}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(T, tokens, i, 7)

    if isa(tokens[i + 3], Bool)
        largearc = tokens[i + 3]
    elseif tokens[i + 3] == 0
        largearc = false
    elseif tokens[i + 3] == 1
        largearc = true
    else
        error("largearc argument to the arc path operation must be boolean")
    end

    if isa(tokens[i + 4], Bool)
        sweep = tokens[i + 4]
    elseif tokens[i + 4] == 0
        sweep = false
    elseif tokens[i + 4] == 1
        sweep = true
    else
        error("sweep argument to the arc path operation must be boolean")
    end

    if !isa(tokens[i + 2], Number)
        error("path arc operation requires a numerical rotation")
    end

    op = T(x_measure(tokens[i]),
           y_measure(tokens[i + 1]),
           convert(Float64, tokens[i + 2]),
           largearc, sweep,
           Point(tokens[i + 5], tokens[i + 6]))

    return (op, i + 7)
end

function absolute_units(p::ArcRelPathOp, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return ArcRelPathOp(
        Measure(absolute_units(p.rx, t, units, box)),
        Measure(absolute_units(p.ry, t, units, box)),
        p.rotation,
        p.largearc,
        p.sweep,
        Point(Measure(absolute_units(to.x, t, units, box)),
              Measure(absolute_units(to.y, t, units, box))))
end

const path_ops = [
     :M => MoveAbsPathOp,
     :m => MoveRelPathOp,
     :Z => ClosePathOp,
     :z => ClosePathOp,
     :L => LineAbsPathOp,
     :l => LineRelPathOp,
     :H => HorLineAbsPathOp,
     :h => HorLineAbsPathOp,
     :V => VertLineAbsPathOp,
     :v => VertLineRelPathOp,
     :C => CubicCurveAbsPathOp,
     :c => CubicCurveRelPathOp,
     :Q => QuadCurveAbsPathOp,
     :q => QuadCurveRelPathOp,
     :T => QuadCurveShortAbsPathOp,
     :t => QuadCurveShortRelPathOp,
     :A => ArcAbsPathOp,
     :a => ArcRelPathOp
]


# TODO: Actually, I don't think I'll need this.
const path_op_type_string = [
     MoveAbsPathOp           => "M",
     MoveRelPathOp           => "m",
     ClosePathOp             => "Z",
     ClosePathOp             => "z",
     LineAbsPathOp           => "L",
     LineRelPathOp           => "l",
     HorLineAbsPathOp        => "H",
     HorLineAbsPathOp        => "h",
     VertLineAbsPathOp       => "V",
     VertLineRelPathOp       => "v",
     CubicCurveAbsPathOp     => "C",
     CubicCurveRelPathOp     => "c",
     QuadCurveAbsPathOp      => "Q",
     QuadCurveRelPathOp      => "q",
     QuadCurveShortAbsPathOp => "T",
     QuadCurveShortRelPathOp => "t",
     ArcAbsPathOp            => "A",
     ArcRelPathOp            => "a"
]


# A path is an array of symbols, numbers, and measures following SVGs path
# mini-language.
function parsepath(tokens::AbstractArray)
    ops = PathOp[]
    last_op_type = nothing
    i = 1
    while i <= length(tokens)
        tok = tokens[i]
        if isa(tok, Symbol)
            if !haskey(path_ops, tok)
                error("$(tok) is not a valid path operation")
            else
                op_type = path_ops[tok]
                i += 1
                op, i = parsepathop(op_type, tokens, i)
                push!(ops, op)
                last_op_type
            end
        else
            i += 1
            op, i = parsepathop(last_op_type, tokens, i)
        end
    end

    return ops
end


immutable PathPrimitive <: FormPrimitive
    ops::Vector{PathOp}
end

typealias Path Form{PathPrimitive}


function path(tokens::AbstractArray)
    return Path([PathPrimitive(parsepath(tokens))])
end


function path{T <: AbstractArray}(tokens::AbstractArray{T})
    return Path([PathPrimitive(parsepath(ts)) for ts in tokens])
end


function absolute_units(p::PathPrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return PathPrimitive([absolute_units(op, t, units, box) for op in p.ops])
end

