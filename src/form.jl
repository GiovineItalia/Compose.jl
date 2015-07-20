
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


function draw(backend::Backend, box::AbsoluteBox, units::UnitBox, t::Transform,
              form::Form)
    draw(backend, Form([resolve(box, units, t, primitive)
                        for primitive in form.primitives]))
end


# Polygon
# -------

immutable SimplePolygonPrimitive{P <: Vec} <: FormPrimitive
    points::Vector{P}
end

typealias SimplePolygon Form{SimplePolygonPrimitive}

typealias Polygon SimplePolygon
typealias PolygonPrimitive SimplePolygonPrimitive


function polygon()
    return Polygon([PolygonPrimitive(Vec[])])
end


function polygon{T <: XYTupleOrVec}(points::AbstractArray{T})
    XM, YM = narrow_polygon_point_types(Vector[points])
    if XM == Any
        XM = Length{:cx, Float64}
    end
    if YM == Any
        YM = Length{:cy, Float64}
    end
    VecType = Tuple{XM, YM}

    return Polygon([PolygonPrimitive(VecType[(XM(point[1]), YM(point[2]))
                    for point in points])])
end


function narrow_polygon_point_types{XM, YM}(
            point_arrays::AbstractArray{Vector{Tuple{XM, YM}}})
    return (XM, YM)
end


function narrow_polygon_point_types(point_arrays::AbstractArray)
    type_params{XM, YM}(p::Tuple{XM, YM}) = (XM, YM)

    if !isempty(point_arrays) && all([eltype(arr) <: Vec for arr in point_arrays])
        xm, ym = type_params(eltype(point_arrays[1]))
        for i in 2:length(point_arrays)
            if type_params(eltype(point_arrays[i])) != (xm, ym)
                return Any, Any
            end
        end
        return xm, ym
    else
        return Any, Any
    end
end


function polygon(point_arrays::AbstractArray)
    XM, YM = narrow_polygon_point_types(point_arrays)
    VecType = XM == YM == Any ? Vec : Vec{XM, YM}
    PrimType = XM == YM == Any ? PolygonPrimitive : PolygonPrimitive{VecType}

    polyprims = Array(PrimType, length(point_arrays))
    for (i, point_array) in enumerate(point_arrays)
        polyprims[i] = PrimType(VecType[convert(Vec, point)
                                          for point in point_array])
    end
    return Form{PrimType}(polyprims)
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::PolygonPrimitive)
    return PolygonPrimitive{AbsoluteVec}(
                [resolve(box, units, t, point) for point in p.points])
end


function boundingbox(form::PolygonPrimitive, linewidth::Measure,
                     font::String, fontsize::Measure)
    x0 = minimum([p[1] for p in form.points])
    x1 = maximum([p[1] for p in form.points])
    y0 = minimum([p[2] for p in form.points])
    y1 = maximum([p[2] for p in form.points])
    return BoundingBox(x0 - linewidth,
                       y0 - linewidth,
                       x1 - x0 + linewidth,
                       y1 - y0 + linewidth)
end

immutable ComplexPolygonPrimitive{P <: Point} <: FormPrimitive
    rings::Vector{Vector{P}}
end

typealias ComplexPolygon Form{ComplexPolygonPrimitive}

function complexpolygon()
    return ComplexPolygon([ComplexPolygonPrimitive(Point[])])
end

function complexpolygon{T <: Real}(coords::Vector{Vector{Vector{T}}})
    return ComplexPolygon([ComplexPolygonPrimitive(Vector{Point}[Point[Point(i,j) for (i,j) in ring] for ring in coords])])
end

function complexpolygon{P <: Point}(rings::Vector{Vector{P}})
    return ComplexPolygon([ComplexPolygonPrimitive(rings)])
end

function absolute_units(p::ComplexPolygonPrimitive, t::Transform, units::UnitBox,
                        box::AbsoluteBoundingBox)
    return ComplexPolygonPrimitive{SimplePoint}(
                [SimplePoint[absolute_units(point, t, units, box) for point in ring]
                for ring in p.rings])
end


# Rectangle
# ---------

immutable RectanglePrimitive{P <: Vec, M1 <: Measure, M2 <: Measure} <: FormPrimitive
    corner::P
    width::M1
    height::M2
end

typealias Rectangle Form{RectanglePrimitive}


function rectangle()
    return Rectangle([RectanglePrimitive((0.0w, 0.0h), 1.0w, 1.0h)])
end


function rectangle(x0, y0, width, height)
    corner = (x0, y0)
    width = x_measure(width)
    height = y_measure(height)
    return Rectangle([RectanglePrimitive(corner, width, height)])
end


function rectangle(x0s::AbstractArray, y0s::AbstractArray,
                   widths::AbstractArray, heights::AbstractArray)
    return @makeform (x0 in x0s, y0 in y0s, width in widths, height in heights),
                     RectanglePrimitive((x0, y0),
                                        x_measure(width), y_measure(height))
end


function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::RectanglePrimitive)

    corner = resolve(box, units, t, p.corner)
    width = resolve(box, units, t, p.width)
    height = resolve(box, units, t, p.height)

    if isa(p.corner.x, AbsoluteLength) && units.width < zero(typeof(units.width))
        x = corner.x.abs - width
    else
        x = corner.x.abs
    end

    if iisa(p.corner.y, AbsoluteLength) && units.height < zero(typeof(units.height))
        y = corner.y.abs - height
    else
        y = corner.y.abs
    end

    return RectanglePrimitive{AbsoluteVec, AbsoluteLength, AbsoluteLength}(
        (x, y), width, height)
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

immutable CirclePrimitive{P <: Vec, M <: Measure} <: FormPrimitive
    center::P
    radius::M
end


function CirclePrimitive{P, M}(center::P, radius::M)
    return CirclePrimitive{P, M}(center, radius)
end


function CirclePrimitive(x, y, r)
    return CirclePrimitive((x, y), x_measure(r))
end


typealias Circle Form{CirclePrimitive}


function circle()
    return Circle([CirclePrimitive((0.5w, 0.5h), 0.5w)])
end


function circle(x, y, r)
    return Circle([CirclePrimitive(x, y, r)])
end


function circle(xs::AbstractArray, ys::AbstractArray, rs::AbstractArray)
    if isempty(xs) || isempty(ys) || isempty(rs)
        return Circle(CirclePrimitive[])
    end

    return @makeform (x in xs, y in ys, r in rs), CirclePrimitive(x, y, r)
end

function resolve(box::AbsoluteBox, units::UnitBox, t::Transform,
                 p::CirclePrimitive)
    return CirclePrimitive{AbsoluteVec{2}, AbsoluteLength}(
        resolve(box, units, t, p.center),
        resolve(box, units, t, p.radius))
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


#immutable EllipsePrimitive{P1 <: Vec, P2 <: Vec, P3 <: Vec} <: FormPrimitive
    #center::P1
    #x_point::P2
    #y_point::P3
#end

#typealias Ellipse Form{EllipsePrimitive}


#function ellipse()
    #return Ellipse([EllipsePrimitive(Vec(0.5w, 0.5h),
                                     #Vec(1.0w, 0.5h),
                                     #Vec(0.5w, 1.0h))])
#end


#function ellipse(x, y, x_radius, y_radius)
    #return Ellipse([EllipsePrimitive(Vec(x, y),
                                     #Vec(x_measure(x) + x_measure(x_radius), y),
                                     #Vec(x, y_measure(y) + y_measure(y_radius)))])
#end


#function ellipse(xs::AbstractArray, ys::AbstractArray,
                 #x_radiuses::AbstractArray, y_radiuses::AbstractArray)
    #return @makeform (x in xs, y in ys, x_radius in x_radiuses, y_radius in y_radiuses),
            #EllipsePrimitive(Vec(x, y),
                             #Vec(x_measure(x) + x_measure(x_radius), y),
                             #Vec(x, y_measure(y) + y_measure(y_radius)))
#end


#function absolute_units(p::EllipsePrimitive, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return EllipsePrimitive{SimpleVec, SimpleVec, SimpleVec}(
                        #absolute_units(p.center, t, units, box),
                        #absolute_units(p.x_point, t, units, box),
                        #absolute_units(p.y_point, t, units, box))
#end

#function boundingbox(form::EllipsePrimitive, linewidth::Measure,
                     #font::String, fontsize::Measure)
    #x0 = min(form.x_point.x, form.y_point.x)
    #x1 = max(form.x_point.x, form.y_point.x)
    #y0 = min(form.x_point.y, form.y_point.y)
    #y1 = max(form.x_point.y, form.y_point.y)
    #xr = x1 - x0
    #yr = y1 - y0
    #return BoundingBox(x0 - linewidth - xr,
                       #y0 - linewidth - yr,
                       #2 * (xr + linewidth),
                       #2 * (yr + linewidth))
#end



## Text
## ----

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


#immutable TextPrimitive{P <: Vec, R <: Rotation} <: FormPrimitive
    #position::P
    #value::String
    #halign::HAlignment
    #valign::VAlignment

    ## Text forms need their own rotation field unfortunately, since there is no
    ## way to give orientation with just a position point.
    #rot::R
#end

#typealias Text Form{TextPrimitive}


#function text(x, y, value::String,
              #halign::HAlignment=hleft, valign::VAlignment=vbottom,
              #rot=Rotation())
    #return Text([TextPrimitive(Vec(x, y), value, halign, valign, rot)])
#end


#function text(x, y, value,
              #halign::HAlignment=hleft, valign::VAlignment=vbottom,
              #rot=Rotation())
    #return Text([TextPrimitive(Vec(x, y), string(value), halign, valign, rot)])
#end


#function text(xs::AbstractArray, ys::AbstractArray, values::AbstractArray{String},
              #haligns::AbstractArray=[hleft], valigns::AbstractArray=[vbottom],
              #rots::AbstractArray=[Rotation()])
    #return @makeform (x in xs, y in ys, value in values, halign in haligns, valign in valigns, rot in rots),
            #TextPrimitive(Vec(x, y), value, halign, valign, rot)
#end


#function text(xs::AbstractArray, ys::AbstractArray, values::AbstractArray,
              #haligns::AbstractArray=[hleft], valigns::AbstractArray=[vbottom],
              #rots::AbstractArray=[Rotation()])
    #return @makeform (x in xs, y in ys, value in values, halign in haligns, valign in valigns, rot in rots),
            #TextPrimitive(Vec(x, y), value, halign, valign, rot)
#end


#function absolute_units(p::TextPrimitive, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return TextPrimitive{SimpleVec, Rotation{SimpleVec}}(
                     #absolute_units(p.position, t, units, box),
                     #p.value, p.halign, p.valign,
                     #absolute_units(p.rot, t, units, box))
#end

#function boundingbox(form::TextPrimitive, linewidth::Measure,
                     #font::String, fontsize::Measure)

    #width, height = text_extents(font, fontsize, form.value)[1]

    #if form.halign == hleft
        #x0 = form.position.x
    #elseif form.halign == hcenter
        #x0 = form.position.x - width/2
    #elseif form.halign == hright
        #x0 = form.position.x - width
    #end

    #if form.valign == vbottom
        #y0 = form.position.y - height
    #elseif form.valign == vcenter
        #y0 = form.position.y - height/2
    #elseif form.valign == vtop
        #y0 = form.position.y
    #end

    #return BoundingBox(x0 - linewidth,
                       #y0 - linewidth,
                       #width + linewidth,
                       #height + linewidth)
#end

## Line
## ----

#immutable LinePrimitive{P <: Vec} <: FormPrimitive
    #points::Vector{P}
#end

#typealias Line Form{LinePrimitive}


#function line()
    #return Line([LinePrimitive(Vec[])])
#end


#function line{T <: XYTupleOrVec}(points::AbstractArray{T})
    #XM, YM = narrow_polygon_point_types(Vector[points])
    #VecType = XM == YM == Any ? Vec : Vec{XM, YM}
    #return Line([LinePrimitive(VecType[convert(VecType, point) for point in points])])
#end


#function line(point_arrays::AbstractArray)
    #XM, YM = narrow_polygon_point_types(point_arrays)
    #VecType = XM == YM == Any ? Vec : Vec{XM, YM}
    #PrimType = XM == YM == Any ? LinePrimitive : LinePrimitive{VecType}

    #lineprims = Array(PrimType, length(point_arrays))
    #for (i, point_array) in enumerate(point_arrays)
        #p =  PrimType(VecType[convert(Vec, point) for point in point_array])
        #lineprims[i] = PrimType(VecType[convert(Vec, point)
                                          #for point in point_array])
    #end
    #return Form{PrimType}(lineprims)
#end


#function absolute_units(p::LinePrimitive, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return LinePrimitive{SimpleVec}(
                #[absolute_units(point, t, units, box) for point in p.points])
#end


#function boundingbox(form::LinePrimitive, linewidth::Measure,
                     #font::String, fontsize::Measure)
    #x0 = minimum([p.x for p in form.points])
    #x1 = maximum([p.x for p in form.points])
    #y0 = minimum([p.y for p in form.points])
    #y1 = maximum([p.y for p in form.points])
    #return BoundingBox(x0 - linewidth,
                       #y0 - linewidth,
                       #x1 - x0 + linewidth,
                       #y1 - y0 + linewidth)
#end

## Curve
## -----

#immutable CurvePrimitive{P1 <: Vec, P2 <: Vec, P3 <: Vec, P4 <: Vec} <: FormPrimitive
    #anchor0::P1
    #ctrl0::P2
    #ctrl1::P3
    #anchor1::P4
#end

#typealias Curve Form{CurvePrimitive}


#function curve(anchor0::XYTupleOrVec, ctrl0::XYTupleOrVec,
               #ctrl1::XYTupleOrVec, anchor1::XYTupleOrVec)
    #return Curve([CurvePrimitive(convert(Vec, anchor0), convert(Vec, ctrl0),
                                 #convert(Vec, ctrl1), convert(Vec, anchor1))])
#end


#function curve(anchor0s::AbstractArray, ctrl0s::AbstractArray,
               #ctrl1s::AbstractArray, anchor1s::AbstractArray)
    #return @makeform (anchor0 in anchor0s, ctrl0 in ctrl0s, ctrl1 in ctrl1s, anchor1 in anchor1s),
            #CurvePrimitive(convert(Vec, anchor0), convert(Vec, ctrl0),
                           #convert(Vec, ctrl1), convert(Vec, anchor1))
#end


#function absolute_units(p::CurvePrimitive, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return CurvePrimitive{SimpleVec, SimpleVec, SimpleVec, SimpleVec}(
                      #absolute_units(p.anchor0, t, units, box),
                      #absolute_units(p.ctrl0, t, units, box),
                      #absolute_units(p.ctrl1, t, units, box),
                      #absolute_units(p.anchor1, t, units, box))
#end


## Bitmap
## ------

#immutable BitmapPrimitive{P <: Vec, XM <: Measure, YM <: Measure} <: FormPrimitive
    #mime::String
    #data::Vector{Uint8}
    #corner::P
    #width::XM
    #height::YM
#end

#typealias Bitmap Form{BitmapPrimitive}


#function bitmap(mime::String, data::Vector{Uint8}, x0, y0, width, height)
    #corner = Vec(x0, y0)
    #width = x_measure(width)
    #height = y_measure(height)
    #return Bitmap([BitmapPrimitive(mime, data, corner, width, height)])
#end


#function bitmap(mimes::AbstractArray, datas::AbstractArray,
                #x0s::AbstractArray, y0s::AbstractArray,
                #widths::AbstractArray, heights::AbstractArray)
    #return @makeform (mime in mimes, data in datas, x0 in x0s, y0 in y0s, width in widths, height in heigths),
            #BitmapPrimitive(mime, data, x0, y0, x_measure(width), y_measure(height))
#end


#function absolute_units(p::BitmapPrimitive, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return BitmapPrimitive{SimpleVec, SimpleMeasure, SimpleMeasure}(
                       #p.mime, p.data,
                       #absolute_units(p.corner, t, units, box),
                       #Measure(absolute_units(p.width, t, units, box)),
                       #Measure(absolute_units(p.height, t, units, box)))
#end


#function boundingbox(form::BitmapPrimitive, linewidth::Measure,
                     #font::String, fontsize::Measure)
    #return BoundingBox(form.corner.x, form.corner.y, form.width, form.height)
#end

## Path
## ----

## An implementation of the SVG path mini-language.

#abstract PathOp

#immutable MoveAbsPathOp <: PathOp
    #to::Vec
#end

#function assert_pathop_tokens_len(op_type, tokens, i, needed)
    #provided = length(tokens) - i + 1
    #if provided < needed
        #error("In path $(op_type) requires $(needed) argumens but only $(provided) provided.")
    #end
#end

#function parsepathop(::Type{MoveAbsPathOp}, tokens::AbstractArray, i)
    #assert_pathop_tokens_len(MoveAbsPathOp, tokens, i, 2)
    #op = MoveAbsPathOp(Vec(tokens[i], tokens[i + 1]))
    #return (op, i + 2)
#end

#function absolute_units(p::MoveAbsPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return MoveAbsPathOp(absolute_units(p.to, t, units, box))
#end

#immutable MoveRelPathOp <: PathOp
    #to::Vec
#end

#function parsepathop(::Type{MoveRelPathOp}, tokens::AbstractArray, i)
    #assert_pathop_tokens_len(MoveRelPathOp, tokens, i, 2)
    #op = MoveRelPathOp(Vec(tokens[i], tokens[i + 1]))
    #return (op, i + 2)
#end

#function absolute_offset(p::Vec, t::Transform, units::UnitBox, box::AbsoluteBoundingBox)

    #absp = absolute_units(p, t, units, box)
    #zer0 = absolute_units(Vec(0w, 0h), t, units, box)

    #return Vec(Measure(absp.x.abs - zer0.x.abs),
                               #Measure(absp.y.abs - zer0.y.abs))
#end

#function absolute_units(p::MoveRelPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return MoveRelPathOp(absolute_offset(p.to, t, units, box))
#end


#immutable ClosePathOp <: PathOp
#end

#function parsepathop(::Type{ClosePathOp}, tokens::AbstractArray, i)
    #return (ClosePathOp(), i)
#end

#function absolute_units(p::ClosePathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return p
#end


#immutable LineAbsPathOp <: PathOp
    #to::Vec
#end

#function parsepathop(::Type{LineAbsPathOp}, tokens::AbstractArray, i)
    #assert_pathop_tokens_len(LineAbsPathOp, tokens, i, 2)
    #op = LineAbsPathOp(Vec(tokens[i], tokens[i + 1]))
    #return (op, i + 2)
#end

#function absolute_units(p::LineAbsPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return LineAbsPathOp(absolute_units(p.to, t, units, box))
#end


#immutable LineRelPathOp <: PathOp
    #to::Vec
#end

#function parsepathop(::Type{LineRelPathOp}, tokens::AbstractArray, i)
    #assert_pathop_tokens_len(LineRelPathOp, tokens, i, 2)
    #op = LineRelPathOp(Vec(tokens[i], tokens[i + 1]))
    #return (op, i + 2)
#end

#function absolute_units(p::LineRelPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return LineRelPathOp(absolute_offset(p.to, t, units, box))
#end


#immutable HorLineAbsPathOp <: PathOp
    #x::Measure
#end

#function parsepathop(::Type{HorLineAbsPathOp}, tokens::AbstractArray, i)
    #assert_pathop_tokens_len(HorLineAbsPathOp, tokens, i, 1)
    #op = HorLineAbsPathOp(x_measure(tokens[i]))
    #return (op, i + 1)
#end

#function absolute_units(p::HorLineAbsPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return HorLineAbsPathOp(Measure(absolute_x_position(p.x, t, units, box)))
#end


#immutable HorLineRelPathOp <: PathOp
    #Δx::Measure
#end

#function parsepathop(::Type{HorLineRelPathOp}, tokens::AbstractArray, i)
    #assert_pathop_tokens_len(HorLineRelPathOp, tokens, i, 1)
    #op = HorLineRelPathOp(x_measure(tokens[i]))
    #return (op, i + 1)
#end

#function absolute_units(p::HorLineRelPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return HorLineRelPathOp(Measure(absolute_units(p.Δx, t, units, box)))
#end


#immutable VertLineAbsPathOp <: PathOp
    #y::Measure
#end

#function parsepathop(::Type{VertLineAbsPathOp}, tokens::AbstractArray, i)
    #assert_pathop_tokens_len(VertLineAbsPathOp, tokens, i, 1)
    #op = VertLineAbsPathOp(y_measure(tokens[i]))
    #return (op, i + 1)
#end

#function absolute_units(p::VertLineAbsPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return VertLineAbsPathOp(Measure(absolute_y_position(p.x, t, units, box)))
#end


#immutable VertLineRelPathOp <: PathOp
    #Δy::Measure
#end

#function parsepathop(::Type{VertLineRelPathOp}, tokens::AbstractArray, i)
    #assert_pathop_tokens_len(VertLineRelPathOp, tokens, i, 1)
    #op = VertLineRelPathOp(y_measure(tokens[i]))
    #return (op, i + 1)
#end

#function absolute_units(p::VertLineRelPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return VertLineRelPathOp(Measure(absolute_units(p.Δy, t, units, box)))
#end


#immutable CubicCurveAbsPathOp <: PathOp
    #ctrl1::Vec
    #ctrl2::Vec
    #to::Vec
#end

#function parsepathop(::Type{CubicCurveAbsPathOp}, tokens::AbstractArray, i)
    #assert_pathop_tokens_len(CubicCurveAbsPathOp, tokens, i, 6)
    #op = CubicCurveAbsPathOp(Vec(tokens[i],     tokens[i + 1]),
                             #Vec(tokens[i + 2], tokens[i + 3]),
                             #Vec(tokens[i + 4], tokens[i + 5]))
    #return (op, i + 6)
#end

#function absolute_units(p::CubicCurveAbsPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return CubicCurveAbsPathOp(
            #absolute_units(p.ctrl1, t, units, box),
            #absolute_units(p.ctrl2, t, units, box),
            #absolute_units(p.to, t, units, box))
#end


#immutable CubicCurveRelPathOp <: PathOp
    #ctrl1::Vec
    #ctrl2::Vec
    #to::Vec
#end

#function parsepathop(::Type{CubicCurveRelPathOp}, tokens::AbstractArray, i)
    #assert_pathop_tokens_len(CubicCurveRelPathOp, tokens, i, 6)
    #op = CubicCurveRelPathOp(Vec(tokens[i],     tokens[i + 1]),
                             #Vec(tokens[i + 2], tokens[i + 3]),
                             #Vec(tokens[i + 4], tokens[i + 5]))
    #return (op, i + 6)
#end

#function absolute_units(p::CubicCurveRelPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return CubicCurveRelPathOp(
            #absolute_offset(p.ctrl1, t, units, box),
            #absolute_offset(p.ctrl2, t, units, box),
            #absolute_offset(p.to, t, units, box))
#end


#immutable CubicCurveShortAbsPathOp <: PathOp
    #ctrl2::Vec
    #to::Vec
#end

#function parsepathop(::Type{CubicCurveShortAbsPathOp}, tokens::AbstractArray, i)
    #assert_pathop_tokens_len(CubicCurveShortAbsPathOp, tokens, i, 4)
    #op = CubicCurveShortAbsPathOp(Vec(tokens[i],     tokens[i + 1]),
                                  #Vec(tokens[i + 2], tokens[i + 3]))
    #return (op, i + 4)
#end

#function absolute_units(p::CubicCurveShortAbsPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return CubicCurveShortAbsPathOp(
            #absolute_offset(p.ctrl2, t, units, box),
            #absolute_offset(p.to, t, units, box))
#end


#immutable CubicCurveShortRelPathOp <: PathOp
    #ctrl2::Vec
    #to::Vec
#end

#function parsepathop(::Type{CubicCurveShortRelPathOp}, tokens::AbstractArray, i)
    #assert_pathop_tokens_len(CubicCurveShortRelPathOp, tokens, i, 4)
    #op = CubicCurveShortRelPathOp(Vec(tokens[i],     tokens[i + 1]),
                                  #Vec(tokens[i + 2], tokens[i + 3]))
    #return (op, i + 4)
#end

#function absolute_units(p::CubicCurveShortRelPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return CubicCurveShortRelPathOp(
            #absolute_offset(p.ctrl2, t, units, box),
            #absolute_offset(p.to, t, units, box))
#end


#immutable QuadCurveAbsPathOp <: PathOp
    #ctrl1::Vec
    #to::Vec
#end

#function parsepathop(::Type{QuadCurveAbsPathOp}, tokens::AbstractArray, i)
    #assert_pathop_tokens_len(QuadCurveAbsPathOp, tokens, i, 4)
    #op = QuadCurveAbsPathOp(Vec(tokens[i],     tokens[i + 1]),
                            #Vec(tokens[i + 2], tokens[i + 3]))
    #return (op, i + 4)
#end

#function absolute_units(p::QuadCurveAbsPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return QuadCurveAbsPathOp(
            #absolute_units(p.ctrl1, t, units, box),
            #absolute_units(p.to, t, units, box))
#end


#immutable QuadCurveRelPathOp <: PathOp
    #ctrl1::Vec
    #to::Vec
#end

#function parsepathop(::Type{QuadCurveRelPathOp}, tokens::AbstractArray, i)
    #assert_pathop_tokens_len(QuadCurveRelPathOp, tokens, i, 4)
    #op = QuadCurveRelPathOp(Vec(tokens[i],     tokens[i + 1]),
                            #Vec(tokens[i + 2], tokens[i + 3]))
    #return (op, i + 4)
#end

#function absolute_units(p::QuadCurveRelPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return QuadCurveRelPathOp(
            #Vec(Measure(absolute_units(p.ctrl1.x, t, units, box)),
                  #Measure(absolute_units(p.ctrl1.y, t, units, box))),
            #Vec(Measure(absolute_units(p.to.x, t, units, box)),
                  #Measure(absolute_units(p.to.y, t, units, box))))
#end


#immutable QuadCurveShortAbsPathOp <: PathOp
    #to::Vec
#end

#function parsepathop(::Type{QuadCurveShortAbsPathOp}, tokens::AbstractArray, i)
    #assert_pathop_tokens_len(QuadCurveShortAbsPathOp, tokens, i, 2)
    #op = QuadCurveShortAbsPathOp(Vec(tokens[i], tokens[i + 1]))
    #return (op, i + 2)
#end

#function absolute_units(p::QuadCurveShortAbsPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return QuadCurveShortAbsPathOp(
            #absolute_units(p.to, t, units, box))
#end


#immutable QuadCurveShortRelPathOp <: PathOp
    #to::Vec
#end

#function parsepathop(::Type{QuadCurveShortRelPathOp}, tokens::AbstractArray, i)
    #assert_pathop_tokens_len(QuadCurveShortRelPathOp, tokens, i, 2)
    #op = QuadCurveShortRelPathOp(Vec(tokens[i], tokens[i + 1]))
    #return (op, i + 2)
#end

#function absolute_units(p::QuadCurveRelPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return QuadCurveRelPathOp(
            #Vec(Measure(absolute_units(p.to.x, t, units, box)),
                  #Measure(absolute_units(p.to.y, t, units, box))))
#end


#immutable ArcAbsPathOp <: PathOp
    #rx::Measure
    #ry::Measure
    #rotation::Float64
    #largearc::Bool
    #sweep::Bool
    #to::Vec
#end

#function absolute_units(p::ArcAbsPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return ArcAbsPathOp(
        #Measure(absolute_units(p.rx, t, units, box)),
        #Measure(absolute_units(p.ry, t, units, box)),
        #p.rotation,
        #p.largearc,
        #p.sweep,
        #absolute_units(p.to, t, units, box))
#end


#immutable ArcRelPathOp <: PathOp
    #rx::Measure
    #ry::Measure
    #rotation::Float64
    #largearc::Bool
    #sweep::Bool
    #to::Vec
#end

#function parsepathop{T <: Union(ArcAbsPathOp, ArcRelPathOp)}(::Type{T}, tokens::AbstractArray, i)
    #assert_pathop_tokens_len(T, tokens, i, 7)

    #if isa(tokens[i + 3], Bool)
        #largearc = tokens[i + 3]
    #elseif tokens[i + 3] == 0
        #largearc = false
    #elseif tokens[i + 3] == 1
        #largearc = true
    #else
        #error("largearc argument to the arc path operation must be boolean")
    #end

    #if isa(tokens[i + 4], Bool)
        #sweep = tokens[i + 4]
    #elseif tokens[i + 4] == 0
        #sweep = false
    #elseif tokens[i + 4] == 1
        #sweep = true
    #else
        #error("sweep argument to the arc path operation must be boolean")
    #end

    #if !isa(tokens[i + 2], Number)
        #error("path arc operation requires a numerical rotation")
    #end

    #op = T(x_measure(tokens[i]),
           #y_measure(tokens[i + 1]),
           #convert(Float64, tokens[i + 2]),
           #largearc, sweep,
           #Vec(tokens[i + 5], tokens[i + 6]))

    #return (op, i + 7)
#end

#function absolute_units(p::ArcRelPathOp, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return ArcRelPathOp(
        #Measure(absolute_units(p.rx, t, units, box)),
        #Measure(absolute_units(p.ry, t, units, box)),
        #p.rotation,
        #p.largearc,
        #p.sweep,
        #Vec(Measure(absolute_units(p.to.x, t, units, box)),
              #Measure(absolute_units(p.to.y, t, units, box))))
#end

#const path_ops = @compat Dict(
     #:M => MoveAbsPathOp,
     #:m => MoveRelPathOp,
     #:Z => ClosePathOp,
     #:z => ClosePathOp,
     #:L => LineAbsPathOp,
     #:l => LineRelPathOp,
     #:H => HorLineAbsPathOp,
     #:h => HorLineRelPathOp,
     #:V => VertLineAbsPathOp,
     #:v => VertLineRelPathOp,
     #:C => CubicCurveAbsPathOp,
     #:c => CubicCurveRelPathOp,
     #:S => CubicCurveShortAbsPathOp,
     #:s => CubicCurveShortRelPathOp,
     #:Q => QuadCurveAbsPathOp,
     #:q => QuadCurveRelPathOp,
     #:T => QuadCurveShortAbsPathOp,
     #:t => QuadCurveShortRelPathOp,
     #:A => ArcAbsPathOp,
     #:a => ArcRelPathOp
#)


## A path is an array of symbols, numbers, and measures following SVGs path
## mini-language.
#function parsepath(tokens::AbstractArray)
    #ops = PathOp[]
    #last_op_type = nothing
    #i = 1
    #while i <= length(tokens)
        #tok = tokens[i]
        #strt = i
        #if isa(tok, Symbol)
            #if !haskey(path_ops, tok)
                #error("$(tok) is not a valid path operation")
            #else
                #op_type = path_ops[tok]
                #i += 1
                #op, i = parsepathop(op_type, tokens, i)
                #push!(ops, op)
                #last_op_type = op_type
            #end
        #else
            #op, i = parsepathop(last_op_type, tokens, i)
            #push!(ops, op)
        #end
    #end

    #return ops
#end


#immutable PathPrimitive <: FormPrimitive
    #ops::Vector{PathOp}
#end

#typealias Path Form{PathPrimitive}


#function path(tokens::AbstractArray)
    #return Path([PathPrimitive(parsepath(tokens))])
#end


#function path{T <: AbstractArray}(tokens::AbstractArray{T})
    #return Path([PathPrimitive(parsepath(ts)) for ts in tokens])
#end


#function absolute_units(p::PathPrimitive, t::Transform, units::UnitBox,
                        #box::AbsoluteBoundingBox)
    #return PathPrimitive([absolute_units(op, t, units, box) for op in p.ops])
#end


# TODO: boundingbox

