
# A form is something that ends up as geometry in the graphic.

abstract FormPrimitive

const empty_tag = Symbol("")

immutable Form{P <: FormPrimitive} <: ComposeNode
    primitives::Vector{P}
    tag::Symbol

    function Form(prim, tag::Symbol=empty_tag)
        new(prim, tag)
    end
end

function Form{P<:FormPrimitive}(primitives::Vector{P}, tag::Symbol=empty_tag)
    Form{P}(primitives, tag)
end

function isempty(f::Form)
    return isempty(f.primitives)
end


function isscalar(f::Form)
    return length(f.primitives) == 1
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, form::Form)
    return Form([resolve(box, units, t, primitive)
                 for primitive in form.primitives])
end


function Base.similar{T}(f::Form{T})
    return Form{T}(T[])
end

form_string(::Form) = "FORM"  # fallback definition

# Polygon
# -------

immutable SimplePolygonPrimitive{P <: Vec} <: FormPrimitive
    points::Vector{P}
end

typealias SimplePolygon{P<:SimplePolygonPrimitive} Form{P}

typealias Polygon SimplePolygon
typealias PolygonPrimitive SimplePolygonPrimitive


function polygon()
    return Polygon([PolygonPrimitive(Vec[])])
end

"""
    polygon(points)

Define a polygon. `points` is an array of `(x,y)` tuples
that specify the corners of the polygon.
"""
function polygon{T <: XYTupleOrVec}(points::AbstractArray{T}, tag=empty_tag)
    XM, YM = narrow_polygon_point_types(Vector[points])
    if XM == Any
        XM = Length{:cx, Float64}
    end
    if YM == Any
        YM = Length{:cy, Float64}
    end
    VecType = Tuple{XM, YM}

    return Polygon([PolygonPrimitive(VecType[(x_measure(point[1]), y_measure(point[2]))
                    for point in points])], tag)
end


function polygon(point_arrays::AbstractArray, tag=empty_tag)
    XM, YM = narrow_polygon_point_types(point_arrays)
    VecType = XM == YM == Any ? Vec : Tuple{XM, YM}
    PrimType = XM == YM == Any ? PolygonPrimitive : PolygonPrimitive{VecType}

    polyprims = Array(PrimType, length(point_arrays))
    for (i, point_array) in enumerate(point_arrays)
        polyprims[i] = PrimType(VecType[(x_measure(point[1]), y_measure(point[2]))
                                        for point in point_array])
    end
    return Form{PrimType}(polyprims, tag)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::PolygonPrimitive)
    return PolygonPrimitive{AbsoluteVec2}(
                AbsoluteVec2[resolve(box, units, t, point) for point in p.points])
end


function boundingbox(form::PolygonPrimitive, linewidth::Measure,
                     font::AbstractString, fontsize::Measure)
    x0 = minimum([p[1] for p in form.points])
    x1 = maximum([p[1] for p in form.points])
    y0 = minimum([p[2] for p in form.points])
    y1 = maximum([p[2] for p in form.points])
    return BoundingBox(x0 - linewidth,
                       y0 - linewidth,
                       x1 - x0 + linewidth,
                       y1 - y0 + linewidth)
end


form_string(::SimplePolygon) = "SP"


immutable ComplexPolygonPrimitive{P <: Vec} <: FormPrimitive
    rings::Vector{Vector{P}}
end

typealias ComplexPolygon{P<:ComplexPolygonPrimitive} Form{P}


function complexpolygon()
    return ComplexPolygon([ComplexPolygonPrimitive(Vec[])])
end


function complexpolygon(rings::Vector{Vector}, tag=empty_tag)
    XM, YM = narrow_polygon_point_types(rings)
    if XM == Any
        XM = Length{:cx, Float64}
    end
    if YM == Any
        YM = Length{:cy, Float64}
    end
    VecType = Tuple{XM, YM}

    return ComplexPolygon([
        ComplexPolygonPrimitive([VecType[(x_measure(point[1]), y_measure(point[2]))
                                         for point in points]])], tag)
end


function complexpolygon(ring_arrays::Vector{Vector{Vector}}, tag=empty_tag)
    XM, YM = narrow_polygon_point_types(coords)
    VecType = XM == YM == Any ? Vec : Tuple{XM, YM}
    PrimType = XM == YM == Any ? ComplexPolygonPrimitive : ComplexPolygonPrimitive{VecType}

    ComplexPolygon([PrimType[[(x_measure(x), y_measure(y)) for (x, y) in ring]
                    for ring in ring_array] for ring_array in ring_arrays], tag)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::ComplexPolygonPrimitive)
    return ComplexPolygonPrimitive(
                [AbsoluteVec2[resolve(box, units, t, point, t) for point in ring]
                for ring in p.rings])
end

form_string(::ComplexPolygon) = "CP"



# Rectangle
# ---------

immutable RectanglePrimitive{P <: Vec, M1 <: Measure, M2 <: Measure} <: FormPrimitive
    corner::P
    width::M1
    height::M2
end

typealias Rectangle{P<:RectanglePrimitive} Form{P}

"""
    rectangle()

Define a rectangle that fills the current context completely.
"""
function rectangle()
    prim = RectanglePrimitive((0.0w, 0.0h), 1.0w, 1.0h)
    return Rectangle{typeof(prim)}([prim])
end


"""
    rectangle(x0, y0, width, height)

Define a rectangle of size `width`x`height` with its top left corner at the point (`x`, `y`).
"""
function rectangle(x0, y0, width, height, tag=empty_tag)
    corner = (x_measure(x0), y_measure(y0))
    width = x_measure(width)
    height = y_measure(height)
    prim = RectanglePrimitive(corner, width, height)
    return Rectangle{typeof(prim)}([prim], tag)
end

"""
    rectangle(x0s, y0s, widths, heights)

Arguments can be passed in arrays in order to perform multiple drawing operations at once.
"""
function rectangle(x0s::AbstractArray, y0s::AbstractArray,
                   widths::AbstractArray, heights::AbstractArray, tag=empty_tag)
    return @makeform (x0 in x0s, y0 in y0s, width in widths, height in heights),
        RectanglePrimitive{Vec2, Measure, Measure}((x_measure(x0), y_measure(y0)),
                                                    x_measure(width), y_measure(height)) tag
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::RectanglePrimitive)

    corner = resolve(box, units, t, p.corner)
    width = resolve(box, units, t, p.width)
    height = resolve(box, units, t, p.height)

    if isxflipped(units) && hasunits(Length{:cx}, p.corner[1])
        # if coordinates are flipped we end up with the corner on the other end
        # of the rectangle, which is fix here
        x = corner[1] - width
    else
        x = corner[1]
    end

    if isyflipped(units) && hasunits(Length{:cy}, p.corner[2])
        y = corner[2] - height
    else
        y = corner[2]
    end

    return RectanglePrimitive{AbsoluteVec2, AbsoluteLength, AbsoluteLength}(
        (x, y), width, height)
end


function boundingbox(form::RectanglePrimitive, linewidth::Measure,
                     font::AbstractString, fontsize::Measure)

    return BoundingBox(form.corner.x - linewidth,
                       form.corner.y - linewidth,
                       form.width + 2*linewidth,
                       form.height + 2*linewidth)
end

form_string(::Rectangle) = "R"

# Circle
# ------

immutable CirclePrimitive{P <: Vec, M <: Measure} <: FormPrimitive
    center::P
    radius::M
end


function CirclePrimitive{P, M}(center::P, radius::M)
    return CirclePrimitive{P, M}(center, radius)
end


function CirclePrimitive(x, y, r)
    return CirclePrimitive((x_measure(x), y_measure(y)), x_measure(r))
end


typealias Circle{P<:CirclePrimitive} Form{P}

"""
    circle()

Define a circle in the center of the current context with a diameter equal to the width of the context.
"""
function circle()
    prim = CirclePrimitive((0.5w, 0.5h), 0.5w)
    return Circle{typeof(prim)}([prim])
end

"""
    circle(x, y, r)

Define a circle with its center at (`x`,`y`) and a radius of `r`.
"""
function circle(x, y, r, tag=empty_tag)
    prim = CirclePrimitive(x, y, r)
    return Circle{typeof(prim)}([prim], tag)
end

"""
    circle(xs, ys, rs)

Arguments can be passed in arrays in order to perform multiple drawing operations.
"""
function circle(xs::AbstractArray, ys::AbstractArray, rs::AbstractArray, tag=empty_tag)
    if isempty(xs) || isempty(ys) || isempty(rs)
        prima = CirclePrimitive[]
        return Circle{eltype(prima)}(prima, tag)
    end

    return @makeform (x in xs, y in ys, r in rs),
        CirclePrimitive{Vec2, Measure}((x_measure(x), y_measure(y)), x_measure(r)) tag
end

function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::CirclePrimitive)
    return CirclePrimitive{AbsoluteVec2, AbsoluteLength}(
        resolve(box, units, t, p.center),
        resolve(box, units, t, p.radius))
end


function boundingbox(form::CirclePrimitive, linewidth::Measure,
                     font::AbstractString, fontsize::Measure)
    return BoundingBox(form.center[1] - form.radius - linewidth,
                       form.center[2] - form.radius - linewidth,
                       2 * (form.radius + linewidth),
                       2 * (form.radius + linewidth))
end

form_string(::Circle) = "C"

# Ellipse
# -------


immutable EllipsePrimitive{P1 <: Vec, P2 <: Vec, P3 <: Vec} <: FormPrimitive
    center::P1
    x_point::P2
    y_point::P3
end

typealias Ellipse{P<:EllipsePrimitive} Form{P}


function ellipse()
    prim = EllipsePrimitive((0.5w, 0.5h),
                            (1.0w, 0.5h),
                            (0.5w, 1.0h))
    return Ellipse{typeof(prim)}([prim])
end


function ellipse(x, y, x_radius, y_radius, tag=empty_tag)
    xm = x_measure(x)
    ym = y_measure(y)
    prim = EllipsePrimitive((xm, ym),
                            (xm + x_measure(x_radius), ym),
                            (xm, ym + y_measure(y_radius)))
    return Ellipse{typeof(prim)}([prim], tag)
end


function ellipse(xs::AbstractArray, ys::AbstractArray,
                 x_radiuses::AbstractArray, y_radiuses::AbstractArray, tag=empty_tag)
    return @makeform (x in xs, y in ys, x_radius in x_radiuses, y_radius in y_radiuses),
    EllipsePrimitive((x_measure(x), y_measure(y)),
                     (x_measure(x) + x_measure(x_radius), y_measure(y)),
                     (x_measure(x), y_measure(y) + y_measure(y_radius))) tag
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::EllipsePrimitive)
    return EllipsePrimitive{AbsoluteVec2, AbsoluteVec2, AbsoluteVec2}(
                   resolve(box, units, t, p.center),
                   resolve(box, units, t, p.x_point),
                   resolve(box, units, t, p.y_point))
end


function boundingbox(form::EllipsePrimitive, linewidth::Measure,
                     font::AbstractString, fontsize::Measure)
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

form_string(::Ellipse) = "E"

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


immutable TextPrimitive{P <: Vec, R <: Rotation} <: FormPrimitive
    position::P
    value::AbstractString
    halign::HAlignment
    valign::VAlignment

    # Text forms need their own rotation field unfortunately, since there is no
    # way to give orientation with just a position point.
    rot::R
end

typealias Text{P<:TextPrimitive} Form{P}



"""
    text(x, y, value [,halgin::HAlignment [,valgin::VAlignment [,rot::Rotation]]])

Draw the text `value` at the position (`x`,`y`) relative to the current context.

The default alignment of the text is `hleft` `vbottom`. The vertical and horizontal
alignment is specified by passing `hleft`, `hcenter` or `hright` and `vtop`,
`vcenter` or `vbottom` as values for `halgin` and `valgin` respectively.
"""
function text(x, y, value::AbstractString,
              halign::HAlignment=hleft, valign::VAlignment=vbottom,
              rot=Rotation(); tag::Symbol=empty_tag)
    prim = TextPrimitive((x_measure(x), y_measure(y)), value, halign, valign, rot)
    return Text{typeof(prim)}([prim], tag)
end


function text(x, y, value,
              halign::HAlignment=hleft, valign::VAlignment=vbottom,
              rot=Rotation(); tag::Symbol=empty_tag)
    prim = TextPrimitive((x_measure(x), y_measure(y)), string(value), halign, valign, rot)
    return Text{typeof(prim)}([prim], tag)
end


"""
    text(xs, ys, values [,halgins::HAlignment [,valgins::VAlignment [,rots::Rotation]]])

Arguments can be passed in arrays in order to perform multiple drawing operations at once.
"""
function text(xs::AbstractArray, ys::AbstractArray, values::AbstractArray{AbstractString},
              haligns::AbstractArray=[hleft], valigns::AbstractArray=[vbottom],
              rots::AbstractArray=[Rotation()]; tag::Symbol=empty_tag)
    return @makeform (x in xs, y in ys, value in values, halign in haligns, valign in valigns, rot in rots),
            TextPrimitive((x_measure(x), y_measure(y)), value, halign, valign, rot) tag
end


function text(xs::AbstractArray, ys::AbstractArray, values::AbstractArray,
              haligns::AbstractArray=[hleft], valigns::AbstractArray=[vbottom],
              rots::AbstractArray=[Rotation()]; tag::Symbol=empty_tag)
    return @makeform (x in xs, y in ys, value in values, halign in haligns, valign in valigns, rot in rots),
            TextPrimitive((x_measure(x), y_measure(y)), value, halign, valign, rot) tag
end


function resolve{P, R}(box::AbsoluteBox, units::UnitBox, t::Transform,
                       p::TextPrimitive{P, R})
    rot = resolve(box, units, t, p.rot)
    return TextPrimitive{AbsoluteVec2, typeof(rot)}(
                resolve(box, units, t, p.position),
                p.value, p.halign, p.valign, rot)
end

function boundingbox(form::TextPrimitive, linewidth::Measure,
                     font::AbstractString, fontsize::Measure)

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

form_string(::Text) = "T"

# Line
# ----

immutable LinePrimitive{P <: Vec} <: FormPrimitive
    points::Vector{P}
end

typealias Line{P<:LinePrimitive} Form{P}


function line()
    prim = LinePrimitive(Vec[])
    return Line{typeof(prim)}([prim])
end


function line{T <: XYTupleOrVec}(points::AbstractArray{T}, tag=empty_tag)
    XM, YM = narrow_polygon_point_types(Vector[points])
    VecType = XM == YM == Any ? Vec2 : Tuple{XM, YM}
    prim = LinePrimitive(VecType[(x_measure(point[1]), y_measure(point[2])) for point in points])
    return Line{typeof(prim)}([prim], tag)
end


function line(point_arrays::AbstractArray, tag=empty_tag)
    XM, YM = narrow_polygon_point_types(point_arrays)
    VecType = XM == YM == Any ? Vec2 : Tuple{XM, YM}
    PrimType = XM == YM == Any ? LinePrimitive : LinePrimitive{VecType}

    lineprims = Array(PrimType, length(point_arrays))
    for (i, point_array) in enumerate(point_arrays)
        p = PrimType(VecType[(x_measure(point[1]), y_measure(point[2]))
                             for point in point_array])
        lineprims[i] = p
    end
    return Form{PrimType}(lineprims, tag)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::LinePrimitive)
    return LinePrimitive{AbsoluteVec2}(
                AbsoluteVec2[resolve(box, units, t, point) for point in p.points])
end


function boundingbox(form::LinePrimitive, linewidth::Measure,
                     font::AbstractString, fontsize::Measure)
    x0 = minimum([p.x for p in form.points])
    x1 = maximum([p.x for p in form.points])
    y0 = minimum([p.y for p in form.points])
    y1 = maximum([p.y for p in form.points])
    return BoundingBox(x0 - linewidth,
                       y0 - linewidth,
                       x1 - x0 + linewidth,
                       y1 - y0 + linewidth)
end

form_string(::Line) = "L"

# Curve
# -----

immutable CurvePrimitive{P1 <: Vec, P2 <: Vec, P3 <: Vec, P4 <: Vec} <: FormPrimitive
    anchor0::P1
    ctrl0::P2
    ctrl1::P3
    anchor1::P4
end

typealias Curve{P<:CurvePrimitive} Form{P}


function curve(anchor0::XYTupleOrVec, ctrl0::XYTupleOrVec,
               ctrl1::XYTupleOrVec, anchor1::XYTupleOrVec, tag=empty_tag)
    return Curve([CurvePrimitive((x_measure(anchor0[1]), y_measure(anchor0[2])),
                                 (x_measure(ctrl0[1]), y_measure(ctrl0[2])),
                                 (x_measure(ctrl1[1]), y_measure(ctrl1[2])),
                                 (x_measure(anchor1[1]), y_measure(anchor1[2])))], tag)
end


function curve(anchor0s::AbstractArray, ctrl0s::AbstractArray,
               ctrl1s::AbstractArray, anchor1s::AbstractArray, tag=empty_tag)
    return @makeform (anchor0 in anchor0s, ctrl0 in ctrl0s, ctrl1 in ctrl1s, anchor1 in anchor1s),
    CurvePrimitive((x_measure(anchor0[1]), y_measure(anchor0[2])),
                   (x_measure(ctrl0[1]), y_measure(ctrl0[2])),
                   (x_measure(ctrl1[1]), y_measure(ctrl1[2])),
                   (x_measure(anchor1[1]), y_measure(anchor1[2]))) tag
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::CurvePrimitive)
    return CurvePrimitive{AbsoluteVec2, AbsoluteVec2, AbsoluteVec2, AbsoluteVec2}(
                resolve(box, units, t, p.anchor0),
                resolve(box, units, t, p.ctrl0),
                resolve(box, units, t, p.ctrl1),
                resolve(box, units, t, p.anchor1))
end

form_string(::Curve) = "CV"

# Bitmap
# ------

immutable BitmapPrimitive{P <: Vec, XM <: Measure, YM <: Measure} <: FormPrimitive
    mime::AbstractString
    data::Vector{UInt8}
    corner::P
    width::XM
    height::YM
end

typealias Bitmap{P<:BitmapPrimitive} Form{P}


function bitmap(mime::AbstractString, data::Vector{UInt8}, x0, y0, width, height, tag=empty_tag)
    corner = (x_measure(x0), y_measure(y0))
    width = x_measure(width)
    height = y_measure(height)
    prim = BitmapPrimitive(mime, data, corner, width, height)
    return Bitmap{typeof(prim)}([prim], tag)
end


function bitmap(mimes::AbstractArray, datas::AbstractArray,
                x0s::AbstractArray, y0s::AbstractArray,
                widths::AbstractArray, heights::AbstractArray, tag=empty_tag)
    return @makeform (mime in mimes, data in datas, x0 in x0s, y0 in y0s, width in widths, height in heigths),
            BitmapPrimitive(mime, data, x0, y0, x_measure(width), y_measure(height)) tag
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::BitmapPrimitive)
    return BitmapPrimitive{AbsoluteVec2, AbsoluteLength, AbsoluteLength}(
                p.mime, p.data,
                resolve(box, units, t, p.corner),
                resolve(box, units, t, p.width),
                resolve(box, units, t, p.height))
end


function boundingbox(form::BitmapPrimitive, linewidth::Measure,
                     font::AbstractString, fontsize::Measure)
    return BoundingBox(form.corner.x, form.corner.y, form.width, form.height)
end

form_string(::Bitmap) = "B"

# Path
# ----

# An implementation of the SVG path mini-language.

abstract PathOp

immutable MoveAbsPathOp <: PathOp
    to::Vec
end


function assert_pathop_tokens_len(op_type, tokens, i, needed)
    provided = length(tokens) - i + 1
    if provided < needed
        error("In path $(op_type) requires $(needed) argumens but only $(provided) provided.")
    end
end


function parsepathop(::Type{MoveAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(MoveAbsPathOp, tokens, i, 2)
    op = MoveAbsPathOp((x_measure(tokens[i]), y_measure(tokens[i + 1])))
    return (op, i + 2)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::MoveAbsPathOp)
    return MoveAbsPathOp(resolve(box, units, t, p.to))
end


immutable MoveRelPathOp <: PathOp
    to::Vec
end


function parsepathop(::Type{MoveRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(MoveRelPathOp, tokens, i, 2)
    op = MoveRelPathOp((tokens[i], tokens[i + 1]))
    return (op, i + 2)
end


function resolve_offset(box::AbsoluteBox, units::UnitBox, t::Transform,
                        p::Vec)

    absp = resolve(box, units, t, p)
    zer0 = resolve(box, units, t, (0w, 0h))
    return (absp[1] - zer0[1], absp[2] - zer0[2])
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::MoveRelPathOp)
    return MoveRelPathOp(resolve_offset(box, units, t, p.to))
end


immutable ClosePathOp <: PathOp
end


function parsepathop(::Type{ClosePathOp}, tokens::AbstractArray, i)
    return (ClosePathOp(), i)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::ClosePathOp)
    return p
end


immutable LineAbsPathOp <: PathOp
    to::Vec
end


function parsepathop(::Type{LineAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(LineAbsPathOp, tokens, i, 2)
    op = LineAbsPathOp((x_measure(tokens[i]), y_measure(tokens[i + 1])))
    return (op, i + 2)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::LineAbsPathOp)
    return LineAbsPathOp(resolve(box, units, t, p.to))
end


immutable LineRelPathOp <: PathOp
    to::Vec
end


function parsepathop(::Type{LineRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(LineRelPathOp, tokens, i, 2)
    op = LineRelPathOp((x_measure(tokens[i]), y_measure(tokens[i + 1])))
    return (op, i + 2)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::LineRelPathOp)
    return LineRelPathOp(resolve(box, units, t, p.to))
end


immutable HorLineAbsPathOp <: PathOp
    x::Measure
end


function parsepathop(::Type{HorLineAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(HorLineAbsPathOp, tokens, i, 1)
    op = HorLineAbsPathOp(x_measure(tokens[i]))
    return (op, i + 1)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::HorLineAbsPathOp)
    return HorLineAbsPathOp(resolve(box, units, t, (p.x, 0mm))[1])
end


immutable HorLineRelPathOp <: PathOp
    Δx::Measure
end


function parsepathop(::Type{HorLineRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(HorLineRelPathOp, tokens, i, 1)
    op = HorLineRelPathOp(x_measure(tokens[i]))
    return (op, i + 1)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::HorLineRelPathOp)
    return HorLineRelPathOp(resolve(box, units, t, p.Δx))
end


immutable VertLineAbsPathOp <: PathOp
    y::Measure
end


function parsepathop(::Type{VertLineAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(VertLineAbsPathOp, tokens, i, 1)
    op = VertLineAbsPathOp(y_measure(tokens[i]))
    return (op, i + 1)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::VertLineAbsPathOp)
    return VertLineAbsPathOp(resolve(box, units, t, (0mm, p.y))[2])
end


immutable VertLineRelPathOp <: PathOp
    Δy::Measure
end


function parsepathop(::Type{VertLineRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(VertLineRelPathOp, tokens, i, 1)
    op = VertLineRelPathOp(y_measure(tokens[i]))
    return (op, i + 1)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::VertLineRelPathOp)
    return VertLineAbsPathOp(resolve(box, units, t, (0mmm, p.Δy))[2])
end


immutable CubicCurveAbsPathOp <: PathOp
    ctrl1::Vec
    ctrl2::Vec
    to::Vec
end


function parsepathop(::Type{CubicCurveAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(CubicCurveAbsPathOp, tokens, i, 6)
    op = CubicCurveAbsPathOp((tokens[i],     tokens[i + 1]),
                             (tokens[i + 2], tokens[i + 3]),
                             (tokens[i + 4], tokens[i + 5]))
    return (op, i + 6)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::CubicCurveAbsPathOp)
    return CubicCurveAbsPathOp(
            resolve(box, units, t, p.ctrl1),
            resolve(box, units, t, p.ctrl2),
            resolve(box, units, t, p.to))
end


immutable CubicCurveRelPathOp <: PathOp
    ctrl1::Vec
    ctrl2::Vec
    to::Vec
end


function parsepathop(::Type{CubicCurveRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(CubicCurveRelPathOp, tokens, i, 6)
    op = CubicCurveRelPathOp((tokens[i],     tokens[i + 1]),
                             (tokens[i + 2], tokens[i + 3]),
                             (tokens[i + 4], tokens[i + 5]))
    return (op, i + 6)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::CubicCurveRelPathOp)
    return CubicCurveRelPathOp(
            resolve(box, units, t, p.ctrl1),
            resolve(box, units, t, p.ctrl2),
            resolve(box, units, t, p.to))
end


immutable CubicCurveShortAbsPathOp <: PathOp
    ctrl2::Vec
    to::Vec
end


function parsepathop(::Type{CubicCurveShortAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(CubicCurveShortAbsPathOp, tokens, i, 4)
    op = CubicCurveShortAbsPathOp((x_measure(tokens[i]),     y_measure(tokens[i + 1])),
                                  (x_measure(tokens[i + 2]), y_measure(tokens[i + 3])))
    return (op, i + 4)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::CubicCurveShortAbsPathOp)
    return CubicCurveShortAbsPathOp(
            resolve_offset(box, units, t, p.ctrl2),
            resolve_offset(box, units, t, p.to))
end


immutable CubicCurveShortRelPathOp <: PathOp
    ctrl2::Vec
    to::Vec
end


function parsepathop(::Type{CubicCurveShortRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(CubicCurveShortRelPathOp, tokens, i, 4)
    op = CubicCurveShortRelPathOp((x_measure(tokens[i]),     y_measure(tokens[i + 1])),
                                  (x_measure(tokens[i + 2]), y_measure(tokens[i + 3])))
    return (op, i + 4)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::CubicCurveShortRelPathOp)
    return CubicCurveShortRelPathOp(
            resolve(box, units, t, p.ctrl2),
            resolve(box, units, t, p.to))
end


immutable QuadCurveAbsPathOp <: PathOp
    ctrl1::Vec
    to::Vec
end


function parsepathop(::Type{QuadCurveAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(QuadCurveAbsPathOp, tokens, i, 4)
    op = QuadCurveAbsPathOp((tokens[i],     tokens[i + 1]),
                            (tokens[i + 2], tokens[i + 3]))
    return (op, i + 4)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::QuadCurveAbsPathOp)
    return QuadCurveAbsPathOp(
            resolve(box, units, t, p.ctrl1),
            resolve(box, units, t, p.to))
end


immutable QuadCurveRelPathOp <: PathOp
    ctrl1::Vec
    to::Vec
end


function parsepathop(::Type{QuadCurveRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(QuadCurveRelPathOp, tokens, i, 4)
    op = QuadCurveRelPathOp((tokens[i],     tokens[i + 1]),
                            (tokens[i + 2], tokens[i + 3]))
    return (op, i + 4)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::QuadCurveRelPathOp)
    return QuadCurveRelPathOp(
            (resolve(box, units, t, p.ctrl1[1]),
             resolve(box, units, t, p.ctrl1[2])),
            (resolve(box, units, t, p.to[1]),
             resolve(box, units, t, p.to[2])))
end


immutable QuadCurveShortAbsPathOp <: PathOp
    to::Vec
end


function parsepathop(::Type{QuadCurveShortAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(QuadCurveShortAbsPathOp, tokens, i, 2)
    op = QuadCurveShortAbsPathOp((tokens[i], tokens[i + 1]))
    return (op, i + 2)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::QuadCurveShortAbsPathOp)
    return QuadCurveShortAbsPathOp(resolve(box, units, t, p.to))
end


immutable QuadCurveShortRelPathOp <: PathOp
    to::Vec
end


function parsepathop(::Type{QuadCurveShortRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(QuadCurveShortRelPathOp, tokens, i, 2)
    op = QuadCurveShortRelPathOp((tokens[i], tokens[i + 1]))
    return (op, i + 2)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::QuadCurveShortRelPathOp)
    return QuadCurveShortRelPathOp(
            (resolve(box, units, t, p.to[1]),
             resolve(box, units, t, p.to[2])))
end


immutable ArcAbsPathOp <: PathOp
    rx::Measure
    ry::Measure
    rotation::Float64
    largearc::Bool
    sweep::Bool
    to::Vec
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::ArcAbsPathOp)
    return ArcAbsPathOp(
        resolve(box, units, t, p.rx),
        resolve(box, units, t, p.ry),
        p.rotation,
        p.largearc,
        p.sweep,
        resolve(box, units, t, p.to))
end


immutable ArcRelPathOp <: PathOp
    rx::Measure
    ry::Measure
    rotation::Float64
    largearc::Bool
    sweep::Bool
    to::Vec
end


function parsepathop{T <: Union{ArcAbsPathOp, ArcRelPathOp}}(::Type{T}, tokens::AbstractArray, i)
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
           (x_measure(tokens[i + 5]), y_measure(tokens[i + 6])))

    return (op, i + 7)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::ArcRelPathOp)
    return ArcRelPathOp(
        resolve(box, units, t, p.rx),
        resolve(box, units, t, p.ry),
        p.rotation,
        p.largearc,
        p.sweep,
        (resolve(box, units, t, p.to[1]),
         resolve(box, units, t, p.to[2])))
end


const path_ops = @compat Dict(
     :M => MoveAbsPathOp,
     :m => MoveRelPathOp,
     :Z => ClosePathOp,
     :z => ClosePathOp,
     :L => LineAbsPathOp,
     :l => LineRelPathOp,
     :H => HorLineAbsPathOp,
     :h => HorLineRelPathOp,
     :V => VertLineAbsPathOp,
     :v => VertLineRelPathOp,
     :C => CubicCurveAbsPathOp,
     :c => CubicCurveRelPathOp,
     :S => CubicCurveShortAbsPathOp,
     :s => CubicCurveShortRelPathOp,
     :Q => QuadCurveAbsPathOp,
     :q => QuadCurveRelPathOp,
     :T => QuadCurveShortAbsPathOp,
     :t => QuadCurveShortRelPathOp,
     :A => ArcAbsPathOp,
     :a => ArcRelPathOp
)


# A path is an array of symbols, numbers, and measures following SVGs path
# mini-language.
function parsepath(tokens::AbstractArray)
    ops = PathOp[]
    last_op_type = nothing
    i = 1
    while i <= length(tokens)
        tok = tokens[i]
        strt = i
        if isa(tok, Symbol)
            if !haskey(path_ops, tok)
                error("$(tok) is not a valid path operation")
            else
                op_type = path_ops[tok]
                i += 1
                op, i = parsepathop(op_type, tokens, i)
                push!(ops, op)
                last_op_type = op_type
            end
        else
            op, i = parsepathop(last_op_type, tokens, i)
            push!(ops, op)
        end
    end

    return ops
end


immutable PathPrimitive <: FormPrimitive
    ops::Vector{PathOp}
end

typealias Path Form{PathPrimitive}


function path(tokens::AbstractArray, tag=empty_tag)
    return Path([PathPrimitive(parsepath(tokens))], tag)
end


function path{T <: AbstractArray}(tokens::AbstractArray{T}, tag=empty_tag)
    return Path([PathPrimitive(parsepath(ts)) for ts in tokens], tag)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::PathPrimitive)
    return PathPrimitive([resolve(box, units, t, op) for op in p.ops])
end




# TODO: boundingbox
