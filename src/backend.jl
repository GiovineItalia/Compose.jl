
# Backend: a thing upon with other things are drawn.

export finish, absolute_measure


abstract Backend


# Conversion to SimpleMeasure{MillimeterUnit} given the parent canvases
# bounding box in absolute units and its unit box.
function absolute_measure(u::SimpleMeasure{CanvasXUnit},
                          unit_box::BoundingBox, parent_box::BoundingBox)
    ((u.value - unit_box.x0.value) / unit_box.width.value) * parent_box.width
end

function absolute_measure(u::SimpleMeasure{CanvasYUnit},
                          unit_box::BoundingBox, parent_box::BoundingBox)
    ((u.value - unit_box.y0.value) / unit_box.height.value) * parent_box.height
end

function absolute_measure(u::SimpleMeasure{WidthUnit},
                          unit_box::BoundingBox, parent_box::BoundingBox)
    u.value * parent_box.width
end

function absolute_measure(u::SimpleMeasure{HeightUnit},
                          unit_box::BoundingBox, parent_box::BoundingBox)
    u.value * parent_box.height
end

function absolute_measure(u::CompoundMeasure,
                          unit_box::BoundingBox,
                          parent_box::BoundingBox)
    v = 0mm
    for (U, value) in u.values
        v += adsolute_measure(SimpleMeasure{U}(value), unit_box, parent_box)
    end
    v
end

function absolute_measure(point::Point,
                          unit_box::BoundingBox,
                          parent_box::BoundingBox)

    x = parent_box.x0 + absolute_measure(point.x, unit_box, parent_box)
    y = parent_box.y0 + absolute_measure(point.y, unit_box, parent_box)
    Point(x, y)
end

function absolute_measure(a::BoundingBox,
                          unit_box::BoundingBox,
                          parent_box::BoundingBox)
    BoundingBox(absolute_measure(unit_box, parent_box, a.x0),
                absolute_measure(unit_box, parent_box, a.y0),
                absolute_measure(unit_box, parent_box, a.width),
                absolute_measure(unit_box, parent_box, a.height))
end


# Conversion to native units
# The backend is responsible for defining a
# `native_measure{T <: AbsoluteUnit}(u::SimpleMeasure{T}, backend::Backend)`
# function to convert any SimpleMeasure in absolute units to a Measure of its
# particular subtype of NativeUnit. Everything else is handled here.


#function natie_measureu


function native_measure(u::SimpleMeasure{CanvasXUnit},
                        unit_box::BoundingBox,
                        parent_box::NativeBoundingBox,
                        backend::Backend)
    ((u.value - unit_box.x0.value) / unit_box.width.value) * parent_box.width
end


function native_measure(u::SimpleMeasure{CanvasYUnit},
                        unit_box::BoundingBox,
                        parent_box::NativeBoundingBox,
                        backend::Backend)
    ((u.value - unit_box.y0.value) / unit_box.height.value) * parent_box.height
end


function native_measure(u::SimpleMeasure{WidthUnit},
                        unit_box::BoundingBox,
                        parent_box::NativeBoundingBox,
                        backend::Backend)
    u.value * parent_box.width
end


function native_measure(u::SimpleMeasure{HeightUnit},
                        unit_box::BoundingBox,
                        parent_box::NativeBoundingBox,
                        backend::Backend)
    u.value * parent_box.height
end


function native_measure(u::SimpleMeasure,
                        rot::Rotation,
                        unit_box::BoundingBox,
                        parent_box::NativeBoundingBox,
                        backend::Backend)
    native_measure(u, unit_box, parent_box, backenD)
end


function native_measure(u::SimpleMeasure,
                        unit_box::BoundingBox,
                        parent_box::NativeBoundingBox,
                        backend::Backend)
    native_measure(u, backend)
end


function native_measure(u::SimpleMeasure,
                        t::NativeTransform,
                        unit_box::BoundingBox,
                        parent_box::NativeBoundingBox,
                        backend::Backend)
    native_measure(u, unit_box, parent_box, backend)
end


function native_measure(u::CompoundMeasure,
                        t::NativeTransform,
                        unit_box::BoundingBox,
                        parent_box::NativeBoundingBox,
                        backend::Backend)
    v = native_zero(backend)
    for (U, value) in u.values
        v += native_measure(SimpleMeasure{U}(value), t, unit_box,
                            parent_box, backend)
    end
    v
end


function native_measure(point::Point,
                        t::NativeTransform,
                        unit_box::BoundingBox,
                        parent_box::NativeBoundingBox,
                        backend::Backend)

    x = parent_box.x0 + native_measure(point.x, t, unit_box,
                                       parent_box, backend)
    y = parent_box.y0 + native_measure(point.y, t, unit_box,
                                       parent_box, backend)

    xy = [convert(Float64, x), convert(Float64, y), 1.0]
    xyt = t.M * xy

    Point(convert(typeof(x), xyt[1]),
          convert(typeof(y), xyt[2]))
end


function native_measure(rot::Rotation,
                        t::NativeTransform,
                        unit_box::BoundingBox,
                        parent_box::NativeBoundingBox,
                        backend::Backend)
    off = native_measure(rot.offset, t, unit_box, parent_box, backend)
    ct = cos(rot.theta)
    st = sin(rot.theta)
    x0 = off.x - (ct * off.x - st * off.y)
    y0 = off.y - (st * off.x + ct * off.y)
    NativeTransform([ct -st convert(Float64, x0)
                     st  ct convert(Float64, y0)
                     0.0 0.0 1.0])
end


function native_measure(box::BoundingBox,
                        t::NativeTransform,
                        unit_box::BoundingBox,
                        parent_box::NativeBoundingBox,
                        backend::Backend)
    NativeBoundingBox(
          parent_box.x0 + native_measure(box.x0, t, unit_box,
                                         parent_box, backend),
          parent_box.y0 + native_measure(box.y0, t, unit_box,
                                         parent_box, backend),
          native_measure(box.width,  t, unit_box, parent_box, backend),
          native_measure(box.height, t, unit_box, parent_box, backend))
end


