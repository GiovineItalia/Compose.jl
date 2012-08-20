
# Backend: a thing upon with other things are drawn.

require("measure.jl")

abstract Backend


# Conversion to native units
# The backend is responsible for defining a
# `native_measure{T <: AbsoluteUnit}(u::SimpleMeasure{T}, backend::Backend)`
# function to convert any SimpleMeasure in absolute units to a Measure of its
# particular subtype of NativeUnit. Everything else is handled here.

function native_measure(u::SimpleMeasure{WidthUnit},
                        parent_box::NativeBoundingBox,
                        backend::Backend)
    u.value * parent_box.width
end


function native_measure(u::SimpleMeasure{HeightUnit},
                        parent_box::NativeBoundingBox,
                        backend::Backend)
    u.value * parent_box.height
end


function native_measure(u::CompoundMeasure,
                        parent_box::NativeBoundingBox,
                        backend::Backend)

    conv{U}(v) = native_measure(SimpleMeasure{U}(v), parent_box, backend)
    sum([conv{U}(value) for (U, value) in u.values])
end


function native_measure(point::Point,
                        parent_box::NativeBoundingBox,
                        backend::Backend)
    Point(parent_box.x0 + native_measure(point.x, parent_box, backend),
          parent_box.y0 + native_measure(point.y, parent_box, backend))
end


function native_measure(box::BoundingBox,
                        parent_box::NativeBoundingBox,
                        backend::Backend)
    Point(parent_box.x0 + native_measure(box.x0, parent_box, backend),
          parent_box.y0 + native_measure(box.y0, parent_box, backend),
          native_measure(box.width,  parent_box, backend),
          native_measure(box.height, parent_box, backend))
end


