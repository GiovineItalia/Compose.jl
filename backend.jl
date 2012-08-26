
# Backend: a thing upon with other things are drawn.

require("measure.jl")

abstract Backend


# Conversion to native units
# The backend is responsible for defining a
# `native_measure{T <: AbsoluteUnit}(u::SimpleMeasure{T}, backend::Backend)`
# function to convert any SimpleMeasure in absolute units to a Measure of its
# particular subtype of NativeUnit. Everything else is handled here.

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
                        unit_box::BoundingBox,
                        parent_box::NativeBoundingBox,
                        backend::Backend)
    native_measure(u, backend)
end


function native_measure(u::CompoundMeasure,
                        unit_box::BoundingBox,
                        parent_box::NativeBoundingBox,
                        backend::Backend)

    conv{U}(v) = native_measure(SimpleMeasure{U}(v), parent_box, backend)
    sum([conv{U}(value) for (U, value) in u.values])
end


function native_measure(point::Point,
                        unit_box::BoundingBox,
                        parent_box::NativeBoundingBox,
                        backend::Backend)
    Point(parent_box.x0 + native_measure(point.x, unit_box,
                                         parent_box, backend),
          parent_box.y0 + native_measure(point.y, unit_box,
                                         parent_box, backend))
end


function native_measure(box::BoundingBox,
                        unit_box::BoundingBox,
                        parent_box::NativeBoundingBox,
                        backend::Backend)
    NativeBoundingBox(
          parent_box.x0 + native_measure(box.x0, unit_box, parent_box, backend),
          parent_box.y0 + native_measure(box.y0, unit_box, parent_box, backend),
          native_measure(box.width,  unit_box, parent_box, backend),
          native_measure(box.height, unit_box, parent_box, backend))
end


# The 'upon' macro: slightly more conscise calls to draw.

macro upon(backend_expr::Expr, expr::Expr)

    backend = eval(backend_expr)

    function splice_args(ex::Expr)
        if ex.head == :call && !isempty(ex.args) && ex.args[1] == :draw
            insert(ex.args, 2, backend)
        end

        for arg in ex.args
            if typeof(arg) == Expr
                splice_args(arg)
            end
        end
    end

    splice_args(expr)

    quote
        $(expr)
        finish($(backend))
    end
end

