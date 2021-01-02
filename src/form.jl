# A form is something that ends up as geometry in the graphic.

abstract type FormPrimitive end

const empty_tag = Symbol("")

struct Form{P <: FormPrimitive} <: ComposeNode
    primitives::Vector{P}
    tag::Symbol

    Form{P}(prim, tag::Symbol=empty_tag) where P = new{P}(prim, tag)
end

Form(primitives::Vector{P}, tag::Symbol=empty_tag) where P <: FormPrimitive =
        Form{P}(primitives, tag)

isempty(f::Form) = isempty(f.primitives)

isscalar(f::Form) =
        length(f.primitives) == 1

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, form::Form) =
        Form([resolve(box, units, t, primitive) for primitive in form.primitives])

Base.similar(f::Form{T}) where T = Form{T}(T[])

form_string(::Form) = "FORM"  # fallback definition

# Polygon
# -------

struct SimplePolygonPrimitive{P <: Vec} <: FormPrimitive
    points::Vector{P}
end

const SimplePolygon{P<:SimplePolygonPrimitive} = Form{P}

const Polygon = SimplePolygon
const PolygonPrimitive = SimplePolygonPrimitive

polygon() = Form([PolygonPrimitive(Vec[])])

"""
    polygon(points)

Define a polygon. `points` is an array of `(x,y)` tuples
that specify the corners of the polygon.
"""
function polygon(points::AbstractArray{T}, tag=empty_tag) where T <: XYTupleOrVec
    XM, YM = narrow_polygon_point_types(Vector[points])
    if XM == Any
        XM = Measure
    end
    if YM == Any
        YM = Measure
    end
    VecType = Tuple{XM, YM}

    return Form([PolygonPrimitive(VecType[(x_measure(point[1]), y_measure(point[2]))
                    for point in points])], tag)
end

"""
    polygon(point_arrays::AbstractArray)

Arguments can be passed in arrays in order to perform multiple drawing operations at once.
"""
function polygon(point_arrays::AbstractArray, tag=empty_tag)
    XM, YM = narrow_polygon_point_types(point_arrays)
    VecType = XM == YM == Any ? Vec : Tuple{XM, YM}
    PrimType = XM == YM == Any ? PolygonPrimitive : PolygonPrimitive{VecType}

    polyprims = Array{PrimType}(undef, length(point_arrays))
    for (i, point_array) in enumerate(point_arrays)
        polyprims[i] = PrimType(VecType[(x_measure(point[1]), y_measure(point[2]))
                                        for point in point_array])
    end
    return Form{PrimType}(polyprims, tag)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::PolygonPrimitive) =
        PolygonPrimitive{AbsoluteVec2}( AbsoluteVec2[
            resolve(box, units, t, point) for point in p.points])

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

struct ComplexPolygonPrimitive{P <: Vec} <: FormPrimitive
    rings::Vector{Vector{P}}
end

const ComplexPolygon{P<:ComplexPolygonPrimitive} = Form{P}

complexpolygon() = ComplexPolygon([ComplexPolygonPrimitive(Vec[])])

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

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::ComplexPolygonPrimitive) =
        ComplexPolygonPrimitive( [AbsoluteVec2[
            resolve(box, units, t, point, t) for point in ring] for ring in p.rings])

form_string(::ComplexPolygon) = "CP"


# Rectangle
# ---------

struct RectanglePrimitive{P <: Vec, M1 <: Measure, M2 <: Measure} <: FormPrimitive
    corner::P
    width::M1
    height::M2
end

const Rectangle{P<:RectanglePrimitive} = Form{P}

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
    rectangle(x0s::AbstractArray, y0s::AbstractArray, widths::AbstractArray, heights::AbstractArray)

Arguments can be passed in arrays in order to perform multiple drawing operations at once.
"""
rectangle(x0s::AbstractArray, y0s::AbstractArray,
                   widths::AbstractArray, heights::AbstractArray, tag=empty_tag) =
        @makeform (x0 in x0s, y0 in y0s, width in widths, height in heights),
            RectanglePrimitive{Vec2, Measure, Measure}((x_measure(x0), y_measure(y0)),
                                                    x_measure(width), y_measure(height)) tag


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

boundingbox(form::RectanglePrimitive, linewidth::Measure,
                     font::AbstractString, fontsize::Measure) =
        BoundingBox(form.corner.x - linewidth,
                    form.corner.y - linewidth,
                    form.width + 2*linewidth,
                    form.height + 2*linewidth)

form_string(::Rectangle) = "R"

# Circle
# ------

struct CirclePrimitive{P <: Vec, M <: Measure} <: FormPrimitive
    center::P
    radius::M
end

CirclePrimitive(center::P, radius::M) where {P, M} = CirclePrimitive{P, M}(center, radius)
CirclePrimitive(x, y, r) = CirclePrimitive((x_measure(x), y_measure(y)), x_measure(r))

const Circle{P<:CirclePrimitive} = Form{P}

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
    circle(xs::AbstractArray, ys::AbstractArray, rs::AbstractArray)

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

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::CirclePrimitive) =
        CirclePrimitive{AbsoluteVec2, AbsoluteLength}(
            resolve(box, units, t, p.center),
            resolve(box, units, t, p.radius))

boundingbox(form::CirclePrimitive, linewidth::Measure, font::AbstractString, fontsize::Measure) =
        BoundingBox(form.center[1] - form.radius - linewidth,
            form.center[2] - form.radius - linewidth,
            2 * (form.radius + linewidth),
            2 * (form.radius + linewidth))

form_string(::Circle) = "C"

# Ellipse
# -------

struct EllipsePrimitive{P1<:Vec, P2<:Vec, P3<:Vec} <: FormPrimitive
    center::P1
    x_point::P2
    y_point::P3
end

const Ellipse{P<:EllipsePrimitive} = Form{P}

"""
    ellipse()

Define an ellipse in the center of the current context with `x_radius=0.5w` and `y_radius=0.5h`.
"""
function ellipse()
    prim = EllipsePrimitive((0.5w, 0.5h),
                            (1.0w, 0.5h),
                            (0.5w, 1.0h))
    return Ellipse{typeof(prim)}([prim])
end

"""
    ellipse(x, y, x_radius, y_radius)

Define an ellipse with its center at (`x`,`y`) with radii `x_radius` and `y_radius`.
"""
function ellipse(x, y, x_radius, y_radius, tag=empty_tag)
    xm = x_measure(x)
    ym = y_measure(y)
    prim = EllipsePrimitive((xm, ym),
                            (xm + x_measure(x_radius), ym),
                            (xm, ym + y_measure(y_radius)))
    return Ellipse{typeof(prim)}([prim], tag)
end

"""
    ellipse(xs::AbstractArray, ys::AbstractArray, x_radiuses::AbstractArray, y_radiuses::AbstractArray)

Arguments can be passed in arrays in order to perform multiple drawing operations.
"""
function ellipse(xs::AbstractArray, ys::AbstractArray,
                 x_radiuses::AbstractArray, y_radiuses::AbstractArray, tag=empty_tag)
        return @makeform (x in xs, y in ys, x_radius in x_radiuses, y_radius in y_radiuses),
            EllipsePrimitive((x_measure(x), y_measure(y)),
                     (x_measure(x) + x_measure(x_radius), y_measure(y)),
                     (x_measure(x), y_measure(y) + y_measure(y_radius))) tag
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::EllipsePrimitive) =
        EllipsePrimitive{AbsoluteVec2, AbsoluteVec2, AbsoluteVec2}(
            resolve(box, units, t, p.center),
            resolve(box, units, t, p.x_point),
            resolve(box, units, t, p.y_point))

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

abstract type HAlignment end
struct HLeft   <: HAlignment end
struct HCenter <: HAlignment end
struct HRight  <: HAlignment end

const hleft   = HLeft()
const hcenter = HCenter()
const hright  = HRight()

abstract type VAlignment end
struct VTop    <: VAlignment end
struct VCenter <: VAlignment end
struct VBottom <: VAlignment end

const vtop    = VTop()
const vcenter = VCenter()
const vbottom = VBottom()

struct TextPrimitive{P<:Vec, R<:Rotation, O<:Vec} <: FormPrimitive
    position::P
    value::AbstractString
    halign::HAlignment
    valign::VAlignment

    # Text forms need their own rotation field unfortunately, since there is no
    # way to give orientation with just a position point.
    rot::R

    offset::O
end

const Text{P<:TextPrimitive} = Form{P}

"""
    text(x, y, value [,halign::HAlignment [,valign::VAlignment [,rot::Rotation]]])

Draw the text `value` at the position (`x`,`y`) relative to the current context.

The default alignment of the text is `hleft` `vbottom`. The vertical and horizontal
alignment is specified by passing `hleft`, `hcenter` or `hright` and `vtop`,
`vcenter` or `vbottom` as values for `halign` and `valign` respectively.
"""
function text(x, y, value,
              halign::HAlignment=hleft, valign::VAlignment=vbottom,
              rot=Rotation(), offset::Vec2=(0mm,0mm);
              tag::Symbol=empty_tag)
    moffset = (x_measure(offset[1]), y_measure(offset[2]))
    prim = TextPrimitive((x_measure(x), y_measure(y)), string(value), halign, valign, rot, moffset)
    Text{typeof(prim)}([prim], tag)
end

"""
    text(xs::AbstractArray, ys::AbstractArray, values::AbstractArray [,haligns::HAlignment [,valigns::VAlignment [,rots::Rotation]]])

Arguments can be passed in arrays in order to perform multiple drawing operations at once.
"""
text(xs::AbstractArray, ys::AbstractArray, values::AbstractArray{AbstractString},
              haligns::AbstractArray=[hleft], valigns::AbstractArray=[vbottom],
              rots::AbstractArray=[Rotation()], offsets::AbstractArray=[(0mm,0mm)];
              tag::Symbol=empty_tag) =
    @makeform (x in xs, y in ys, value in values, halign in haligns, valign in valigns, rot in rots, offset in offsets),
        TextPrimitive((x_measure(x), y_measure(y)), value, halign, valign, rot, (x_measure(offset[1]), y_measure(offset[2]))) tag

text(xs::AbstractArray, ys::AbstractArray, values::AbstractArray,
              haligns::AbstractArray=[hleft], valigns::AbstractArray=[vbottom],
              rots::AbstractArray=[Rotation()], offsets::AbstractArray=[(0mm,0mm)];
              tag::Symbol=empty_tag) =
    @makeform (x in xs, y in ys, value in values, halign in haligns, valign in valigns, rot in rots, offset in offsets),
        TextPrimitive((x_measure(x), y_measure(y)), value, halign, valign, rot, (x_measure(offset[1]), y_measure(offset[2]))) tag

function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::TextPrimitive{P,R,O}) where {P,R,O}
    rot = resolve(box, units, t, p.rot)
    return TextPrimitive{AbsoluteVec2, typeof(rot), O}(
                resolve(box, units, t, p.position),
                p.value, p.halign, p.valign, rot, p.offset)
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

    return BoundingBox(x0 - linewidth + form.offset[1],
                       y0 - linewidth + form.offset[2],
                       width + linewidth,
                       height + linewidth)
end

form_string(::Text) = "T"

# Line
# ----

struct LinePrimitive{P<:Vec} <: FormPrimitive
    points::Vector{P}
end

const Line{P<:LinePrimitive} = Form{P}

function line()
    prim = LinePrimitive(Vec[])
    return Line{typeof(prim)}([prim])
end

"""
    line(points)
    
Define a line. `points` is an array of `(x,y)` tuples.
"""
function line(points::AbstractArray{T}, tag=empty_tag) where T <: XYTupleOrVec
    XM, YM = narrow_polygon_point_types(Vector[points])
    VecType = XM == YM == Any ? Vec2 : Tuple{XM, YM}
    prim = LinePrimitive(VecType[(x_measure(point[1]), y_measure(point[2])) for point in points])
    return Line{typeof(prim)}([prim], tag)
end

"""
    line(point_arrays::AbstractArray)

Arguments can be passed in arrays in order to perform multiple drawing operations at once.
"""
function line(point_arrays::AbstractArray, tag=empty_tag)
    XM, YM = narrow_polygon_point_types(point_arrays)
    VecType = XM == YM == Any ? Vec2 : Tuple{XM, YM}
    PrimType = XM == YM == Any ? LinePrimitive : LinePrimitive{VecType}

    lineprims = Array{PrimType}(undef, length(point_arrays))
    for (i, point_array) in enumerate(point_arrays)
        p = PrimType(VecType[(x_measure(point[1]), y_measure(point[2]))
                             for point in point_array])
        lineprims[i] = p
    end
    return Form{PrimType}(lineprims, tag)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::LinePrimitive) =
        LinePrimitive{AbsoluteVec2}(
            AbsoluteVec2[resolve(box, units, t, point) for point in p.points])

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

struct CurvePrimitive{P1<:Vec, P2<:Vec, P3<:Vec, P4<:Vec} <: FormPrimitive
    anchor0::P1
    ctrl0::P2
    ctrl1::P3
    anchor1::P4
end

const Curve{P<:CurvePrimitive} = Form{P}


"""
    curve(anchor0, ctrl0, ctrl1, anchor1)

Define a bezier curve between `anchor0` and `anchor1` with control points `ctrl0` and `ctrl1`.
"""
function curve(anchor0::XYTupleOrVec, ctrl0::XYTupleOrVec,
               ctrl1::XYTupleOrVec, anchor1::XYTupleOrVec, tag=empty_tag)
    prim = CurvePrimitive((x_measure(anchor0[1]), y_measure(anchor0[2])),
                                 (x_measure(ctrl0[1]), y_measure(ctrl0[2])),
                                 (x_measure(ctrl1[1]), y_measure(ctrl1[2])),
                                 (x_measure(anchor1[1]), y_measure(anchor1[2])))
    return Curve{typeof(prim)}([prim], tag)
end

"""
    curve(anchor0s::AbstractArray, ctrl0s::AbstractArray, ctrl1s::AbstractArray, anchor1s::AbstractArray)

Arguments can be passed in arrays in order to perform multiple drawing operations.
"""
curve(anchor0s::AbstractArray, ctrl0s::AbstractArray,
               ctrl1s::AbstractArray, anchor1s::AbstractArray, tag=empty_tag) =
        @makeform (anchor0 in anchor0s, ctrl0 in ctrl0s, ctrl1 in ctrl1s, anchor1 in anchor1s),
            CurvePrimitive((x_measure(anchor0[1]), y_measure(anchor0[2])),
                (x_measure(ctrl0[1]), y_measure(ctrl0[2])),
                (x_measure(ctrl1[1]), y_measure(ctrl1[2])),
                (x_measure(anchor1[1]), y_measure(anchor1[2]))) tag

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::CurvePrimitive) =
        CurvePrimitive{AbsoluteVec2, AbsoluteVec2, AbsoluteVec2, AbsoluteVec2}(
            resolve(box, units, t, p.anchor0),
            resolve(box, units, t, p.ctrl0),
            resolve(box, units, t, p.ctrl1),
            resolve(box, units, t, p.anchor1))

form_string(::Curve) = "CV"

# Bitmap
# ------

struct BitmapPrimitive{P <: Vec, XM <: Measure, YM <: Measure} <: FormPrimitive
    mime::AbstractString
    data::Vector{UInt8}
    corner::P
    width::XM
    height::YM
end

const Bitmap{P<:BitmapPrimitive} = Form{P}

"""
    bitmap(mime, data, x0, y0, width, height)

Define a bitmap of size `width`x`height` with its top left corner at the point (`x`, `y`).
"""
function bitmap(mime::AbstractString, data::Vector{UInt8}, x0, y0, width, height, tag=empty_tag)
    corner = (x_measure(x0), y_measure(y0))
    width = x_measure(width)
    height = y_measure(height)
    prim = BitmapPrimitive(mime, data, corner, width, height)
    return Bitmap{typeof(prim)}([prim], tag)
end

"""
    bitmap(mimes::AbstractArray, datas::AbstractArray, x0s::AbstractArray, y0s::AbstractArray, widths::AbstractArray, heights::AbstractArray)

Arguments can be passed in arrays in order to perform multiple drawing operations.
"""
bitmap(mimes::AbstractArray, datas::AbstractArray,
                x0s::AbstractArray, y0s::AbstractArray,
                widths::AbstractArray, heights::AbstractArray, tag=empty_tag) =
        @makeform (mime in mimes, data in datas, x0 in x0s, y0 in y0s, width in widths, height in heights),
            BitmapPrimitive(mime, data, (x_measure(x0), y_measure(y0)), x_measure(width), y_measure(height)) tag

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::BitmapPrimitive) =
        BitmapPrimitive{AbsoluteVec2, AbsoluteLength, AbsoluteLength}(
            p.mime, p.data,
            resolve(box, units, t, p.corner),
            resolve(box, units, t, p.width),
            resolve(box, units, t, p.height))

boundingbox(form::BitmapPrimitive, linewidth::Measure, font::AbstractString, fontsize::Measure) =
        BoundingBox(form.corner.x, form.corner.y, form.width, form.height)

form_string(::Bitmap) = "B"




# Arc
# -------
struct ArcPrimitive{P<:Vec, M<:Measure} <: Compose.FormPrimitive
    center::P
    radius::M
    angle1::Float64
    angle2::Float64
    sector::Bool
end

ArcPrimitive(center::P, radius::M, angle1, angle2, sector) where {P, M} = ArcPrimitive{P, M}(center, radius, angle1, angle2, sector)
ArcPrimitive(x, y, r, θ1, θ2, sector) = ArcPrimitive((x_measure(x), y_measure(y)), x_measure(r), θ1, θ2, sector)

 Arc{P<:ArcPrimitive} = Compose.Form{P}


"""
    arc(x, y, r, θ1, θ2, sector)

Define an arc with its center at (`x`,`y`), radius of `r`, between `θ1` and `θ2`.  
`sector` (optional) is true or false, true for a pie sector, false for an arc.
Arcs are drawn clockwise from θ1 to θ2.    
"""
function arc(x, y, r, θ1, θ2, sector=false, tag=empty_tag)
    prim = ArcPrimitive(x, y, r, θ1, θ2, sector)
    return Arc{typeof(prim)}([prim], tag)
end

"""
    sector(x, y, r, θ1, θ2)

Define a pie sector with its center at (`x`,`y`), radius of `r`, between `θ1` and `θ2`.  
"""
sector(x, y, r, θ1, θ2) = arc(x,y,r,θ1,θ2,true)

"""
    arc(xs::AbstractVector, ys::AbstractVector, rs::AbstractVector, θ1s::AbstractVector, θ2s::AbstractVector, sectors::AbstractVector)

Arguments can be passed in arrays in order to perform multiple drawing operations.
"""
function arc(xs::AbstractVector, ys::AbstractVector, rs::AbstractVector, θ1s::AbstractVector, θ2s::AbstractVector, sectors::AbstractVector=[false], tag=empty_tag)
        return @makeform (x in xs, y in ys, r in rs, θ1 in θ1s, θ2 in θ2s, sector in sectors), 
            ArcPrimitive((x_measure(x), y_measure(y)), x_measure(r), θ1, θ2, sector) tag
end

"""
    sector(xs::AbstractVector, ys::AbstractVector, rs::AbstractVector, θ1s::AbstractVector, θ2s::AbstractVector)

Arguments can be passed in arrays in order to perform multiple drawing operations.
"""
sector(xs::AbstractVector, ys::AbstractVector, rs::AbstractVector, θ1s::AbstractVector, θ2s::AbstractVector) = 
    arc(xs, ys, rs, θ1s, θ2s, [true])


resolve(box::AbsoluteBox, units::UnitBox, t::Compose.Transform, p::ArcPrimitive) =
        ArcPrimitive{AbsoluteVec2, AbsoluteLength}(
            resolve(box, units, t, p.center),
            resolve(box, units, t, p.radius), p.angle1, p.angle2, p.sector)

boundingbox(form::ArcPrimitive, linewidth::Measure, font::AbstractString, fontsize::Measure) =
        BoundingBox(form.center[1] - form.radius - linewidth,
            form.center[2] - form.radius - linewidth,
            2 * (form.radius + linewidth),
            2 * (form.radius + linewidth))

form_string(::Arc) = "A"

@deprecate slice(x,y,r,θ1,θ2) sector(x,y,r,θ1,θ2)



### Polygon primitive forms

"""
    ngon(x, y, r, n::Int)

Define a `n`-sided polygon with its center at (`x`,`y`), and radius of `r`.  For an upside-down ngon, use `-r`.  
"""
function ngon(x, y, r, n::Int, tag=empty_tag)
    θ = range(-π/2, stop=1.5π, length=n+1)
    x1 = x_measure(x) .+ size_x_measure(r).*cos.(θ)
    y1 = y_measure(y) .+ size_x_measure(r).*sin.(θ)
    points = collect(Tuple{Measure, Measure}, zip(x1, y1))
    return Form([PolygonPrimitive(points)], tag)
end


"""
    ngon(xs::AbstractVector, ys::AbstractVector, rs::AbstractVector, ns::AbstractVector{Int})

Arguments can be passed in arrays in order to perform multiple drawing operations at once.
"""
function ngon(xs::AbstractVector, ys::AbstractVector, rs::AbstractVector, ns::AbstractVector{Int}, tag=empty_tag)
    VecType = Tuple{Measure, Measure}
    PrimType = PolygonPrimitive{VecType}
    polyprims = PrimType[]
    for (x, y, r, n) in Compose.cyclezip(xs, ys, rs, ns)
        p = ngon( x, y, r, n)
        push!(polyprims, PrimType(p.primitives[1].points))
    end
    return Form{PrimType}(polyprims, tag)
end


"""
    star(x, y, r, n::Int, ratio)

Define a `n`-pointed star with its center at (`x`,`y`), outer radius of `r`, and inner radius equal to `r*ratio`. For an upside-down star, use `-r`.
"""
function star(x, y, r, n::Int, ratio::Float64=0.3, tag=empty_tag)
    θ = range(-π/2, stop=1.5π, length=2*n+1)[1:end-1]
    r1 = repeat([r, r*ratio], outer=n)
    x1 = x_measure(x) .+ size_x_measure(r1).*cos.(θ)
    y1 = y_measure(y) .+ size_x_measure(r1).*sin.(θ)
    points = collect(Tuple{Measure, Measure}, zip(x1, y1))
    return Form([PolygonPrimitive(points)], tag)
end


"""
    star(xs::AbstractVector, ys::AbstractVector, rs::AbstractVector, ns::AbstractVector{Int}, ratios::AbstractVector{Float64})

Arguments can be passed in arrays in order to perform multiple drawing operations at once.
"""
function star(xs::AbstractVector, ys::AbstractVector, rs::AbstractVector, ns::AbstractVector{Int}, ratios::AbstractVector{Float64}=[0.3], tag=empty_tag)
    VecType = Tuple{Measure, Measure}
    PrimType = PolygonPrimitive{VecType}
    polyprims = PrimType[]
    for (x, y, r, n, ratio) in Compose.cyclezip(xs, ys, rs, ns, ratios)
        p = star( x, y, r, n, ratio)
        push!(polyprims, PrimType(p.primitives[1].points))
    end
    return Form{PrimType}(polyprims, tag)
end


"""
    xgon(x, y, r, n::Int, ratio)

Define a cross with `n` arms with its center at (`x`,`y`), outer radius of `r`, and inner radius equal to `r*ratio`. For an upside-down xgon, use `-r`.
"""
function xgon(x, y, r, n::Int, ratio::Float64=0.1, tag=empty_tag)
    θ₁ = range(-0.75π, stop=1.25π, length=n+1)[1:end-1]
    w = 2*r*ratio*sin(π/n)
    dₒ = abs(asin(0.5*w/r))
    dᵢ = abs(asin(0.5*w/(r*ratio)))   
    r₂ = repeat([r*ratio,r,r], outer=n)
    θ₂ = vec([θ+x  for x in [-dᵢ, -dₒ, dₒ], θ in θ₁])
    x1 = x_measure(x) .+ size_x_measure(r₂).*cos.(θ₂)
    y1 = y_measure(y) .+ size_x_measure(r₂).*sin.(θ₂)
    points = collect(Tuple{Measure, Measure}, zip(x1, y1))
    return Form([PolygonPrimitive(points)], tag)
end

"""
    xgon(xs::AbstractVector, ys::AbstractVector, rs::AbstractVector, ns::AbstractVector{Int}, ratios::AbstractVector{Float64})

Arguments can be passed in arrays in order to perform multiple drawing operations at once.
"""
function xgon(xs::AbstractVector, ys::AbstractVector, rs::AbstractVector, ns::AbstractVector{Int}, ratios::AbstractVector{Float64}=[0.3], tag=empty_tag)
    VecType = Tuple{Measure, Measure}
    PrimType = PolygonPrimitive{VecType}
    polyprims = PrimType[]
    for (x, y, r, n, ratio) in Compose.cyclezip(xs, ys, rs, ns, ratios)
        p = xgon(x, y, r, n, ratio)
        push!(polyprims, PrimType(p.primitives[1].points))
    end
    return Form{PrimType}(polyprims, tag)
end

"""
    points(x::Compose.Form)

Extract points from a Compose.Form
"""
points(x::Compose.Form) = x.primitives[1].points


# Bezigon
# -------

struct BezierPolygonPrimitive{P<:Vec} <: FormPrimitive
    anchor::P
    sides::Vector{Vector{P}}
end

Bezigon{P<:BezierPolygonPrimitive} = Form{P}


"""
     bezigon(anchor0::Tuple, sides::Vector{<:Vector{<:Tuple}})
 
 Define a bezier polygon. `anchor0` is the starting point as an `(x,y)` tuple.
 `sides` contains Vectors of side points (tuples): each vector has the control point(s) and end point for each side 
 (the end point forms the next starting point).
 The sides can be linear (1 point), quadratic (2 points) or cubic (3 points).
 """
function bezigon(anchor0::XYTupleOrVec, sides::Vector{T}, tag=empty_tag) where T<:Vector{<:XYTupleOrVec}
    anchor = (x_measure(anchor0[1]), y_measure(anchor0[2]))
    sv = Vector{Vec2}[]
    for side in sides
        s = collect(Vec2, zip(x_measure.(first.(side)), y_measure.(last.(side))))
        push!(sv, s)
    end
    Form([BezierPolygonPrimitive(anchor, sv)], tag)
end

 
"""
    bezigon(anchors::Vector{Tuple}, polysides=Vector{<:Vector{<:Vector{<:Tuple}}})

Arguments can be passed in arrays in order to perform multiple drawing operations at once.
"""
function bezigon(anchors::Vector, polysides::Vector{T}, tag=empty_tag) where T<:Vector{<:Vector}
    polyprims = BezierPolygonPrimitive[]
    for (anchor0, sides) in cyclezip(anchors, polysides)
        anchor = (x_measure(anchor0[1]), y_measure(anchor0[2]))
        sv = Vector{Vec2}[]
        for side in sides
            s = collect(Vec2, zip(x_measure.(first.(side)), y_measure.(last.(side))))
            push!(sv, s)
        end
        push!(polyprims, BezierPolygonPrimitive(anchor, sv))
    end
    Form(polyprims, tag)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::BezierPolygonPrimitive)
    anchor = resolve(box, units, t, p.anchor)
    sv = Vector{Vec}[]
    for side in p.sides
        push!(sv, [resolve(box, units, t, point) for point in side])
    end
    return BezierPolygonPrimitive(anchor, sv)
end


function boundingbox(prim::BezierPolygonPrimitive, linewidth::Measure,
                      font::AbstractString, fontsize::Measure)
    points = [prim.anchor; reduce(vcat, prim.sides)]
    x, y = first.(points), last.(points)
    x0, x1 = extrema(x)
    y0, y1 = extrema(y)
    return BoundingBox(x0-linewidth, y0-linewidth, x1-x0+linewidth, y1-y0+linewidth)
end
 
 
form_string(::Bezigon) = "BP"



