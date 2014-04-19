
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
    return Polygon(Array(PolygonPrimitive, 0))
end


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

