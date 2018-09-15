
# Path Primitives


# From form.jl:
# Path
# ----

# An implementation of the SVG path mini-language.

abstract type PathOp end

struct MoveAbsPathOp <: PathOp
    to::Vec
end

function assert_pathop_tokens_len(op_type, tokens, i, needed)
    provided = length(tokens) - i + 1
    provided < needed &&
            error("In path $(op_type) requires $(needed) argumens but only $(provided) provided.")
end

function parsepathop(::Type{MoveAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(MoveAbsPathOp, tokens, i, 2)
    op = MoveAbsPathOp((x_measure(tokens[i]), y_measure(tokens[i + 1])))
    return (op, i + 2)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::MoveAbsPathOp) =
        MoveAbsPathOp(resolve(box, units, t, p.to))

struct MoveRelPathOp <: PathOp
    to::Vec
end

function parsepathop(::Type{MoveRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(MoveRelPathOp, tokens, i, 2)
    op = MoveRelPathOp((tokens[i], tokens[i + 1]))
    return (op, i + 2)
end

function resolve_offset(box::AbsoluteBox, units::UnitBox, t::Transform, p::Vec)
    absp = resolve(box, units, t, p)
    zer0 = resolve(box, units, t, (0w, 0h))
    return (absp[1] - zer0[1], absp[2] - zer0[2])
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::MoveRelPathOp) =
        MoveRelPathOp(resolve_offset(box, units, t, p.to))

struct ClosePathOp <: PathOp
end

parsepathop(::Type{ClosePathOp}, tokens::AbstractArray, i) = (ClosePathOp(), i)

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::ClosePathOp) = p

struct LineAbsPathOp <: PathOp
    to::Vec
end

function parsepathop(::Type{LineAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(LineAbsPathOp, tokens, i, 2)
    op = LineAbsPathOp((x_measure(tokens[i]), y_measure(tokens[i + 1])))
    return (op, i + 2)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::LineAbsPathOp) =
        LineAbsPathOp(resolve(box, units, t, p.to))

struct LineRelPathOp <: PathOp
    to::Vec
end

function parsepathop(::Type{LineRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(LineRelPathOp, tokens, i, 2)
    op = LineRelPathOp((x_measure(tokens[i]), y_measure(tokens[i + 1])))
    return (op, i + 2)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::LineRelPathOp) =
        LineRelPathOp(resolve(box, units, t, p.to))

struct HorLineAbsPathOp <: PathOp
    x::Measure
end

function parsepathop(::Type{HorLineAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(HorLineAbsPathOp, tokens, i, 1)
    op = HorLineAbsPathOp(x_measure(tokens[i]))
    return (op, i + 1)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::HorLineAbsPathOp) =
        HorLineAbsPathOp(resolve(box, units, t, (p.x, 0mm))[1])

struct HorLineRelPathOp <: PathOp
    Δx::Measure
end

function parsepathop(::Type{HorLineRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(HorLineRelPathOp, tokens, i, 1)
    op = HorLineRelPathOp(x_measure(tokens[i]))
    return (op, i + 1)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::HorLineRelPathOp) =
        HorLineRelPathOp(resolve(box, units, t, p.Δx))

struct VertLineAbsPathOp <: PathOp
    y::Measure
end

function parsepathop(::Type{VertLineAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(VertLineAbsPathOp, tokens, i, 1)
    op = VertLineAbsPathOp(y_measure(tokens[i]))
    return (op, i + 1)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::VertLineAbsPathOp) =
        VertLineAbsPathOp(resolve(box, units, t, (0mm, p.y))[2])

struct VertLineRelPathOp <: PathOp
    Δy::Measure
end

function parsepathop(::Type{VertLineRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(VertLineRelPathOp, tokens, i, 1)
    op = VertLineRelPathOp(y_measure(tokens[i]))
    return (op, i + 1)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::VertLineRelPathOp) =
        VertLineAbsPathOp(resolve(box, units, t, (0mmm, p.Δy))[2])

struct CubicCurveAbsPathOp <: PathOp
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

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::CubicCurveAbsPathOp) =
        CubicCurveAbsPathOp(
            resolve(box, units, t, p.ctrl1),
            resolve(box, units, t, p.ctrl2),
            resolve(box, units, t, p.to))

struct CubicCurveRelPathOp <: PathOp
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

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::CubicCurveRelPathOp) =
        CubicCurveRelPathOp(
            resolve(box, units, t, p.ctrl1),
            resolve(box, units, t, p.ctrl2),
            resolve(box, units, t, p.to))

struct CubicCurveShortAbsPathOp <: PathOp
    ctrl2::Vec
    to::Vec
end

function parsepathop(::Type{CubicCurveShortAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(CubicCurveShortAbsPathOp, tokens, i, 4)
    op = CubicCurveShortAbsPathOp((x_measure(tokens[i]),     y_measure(tokens[i + 1])),
                                  (x_measure(tokens[i + 2]), y_measure(tokens[i + 3])))
    return (op, i + 4)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::CubicCurveShortAbsPathOp) =
        CubicCurveShortAbsPathOp(
            resolve_offset(box, units, t, p.ctrl2),
            resolve_offset(box, units, t, p.to))

struct CubicCurveShortRelPathOp <: PathOp
    ctrl2::Vec
    to::Vec
end

function parsepathop(::Type{CubicCurveShortRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(CubicCurveShortRelPathOp, tokens, i, 4)
    op = CubicCurveShortRelPathOp((x_measure(tokens[i]),     y_measure(tokens[i + 1])),
                                  (x_measure(tokens[i + 2]), y_measure(tokens[i + 3])))
    return (op, i + 4)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::CubicCurveShortRelPathOp) =
        CubicCurveShortRelPathOp(
            resolve(box, units, t, p.ctrl2),
            resolve(box, units, t, p.to))

struct QuadCurveAbsPathOp <: PathOp
    ctrl1::Vec
    to::Vec
end

function parsepathop(::Type{QuadCurveAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(QuadCurveAbsPathOp, tokens, i, 4)
    op = QuadCurveAbsPathOp((tokens[i],     tokens[i + 1]),
                            (tokens[i + 2], tokens[i + 3]))
    return (op, i + 4)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::QuadCurveAbsPathOp) =
        QuadCurveAbsPathOp(
            resolve(box, units, t, p.ctrl1),
            resolve(box, units, t, p.to))

struct QuadCurveRelPathOp <: PathOp
    ctrl1::Vec
    to::Vec
end

function parsepathop(::Type{QuadCurveRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(QuadCurveRelPathOp, tokens, i, 4)
    op = QuadCurveRelPathOp((tokens[i],     tokens[i + 1]),
                            (tokens[i + 2], tokens[i + 3]))
    return (op, i + 4)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::QuadCurveRelPathOp) =
        QuadCurveRelPathOp(
            (resolve(box, units, t, p.ctrl1[1]),
             resolve(box, units, t, p.ctrl1[2])),
            (resolve(box, units, t, p.to[1]),
             resolve(box, units, t, p.to[2])))

struct QuadCurveShortAbsPathOp <: PathOp
    to::Vec
end

function parsepathop(::Type{QuadCurveShortAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(QuadCurveShortAbsPathOp, tokens, i, 2)
    op = QuadCurveShortAbsPathOp((tokens[i], tokens[i + 1]))
    return (op, i + 2)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::QuadCurveShortAbsPathOp) =
        QuadCurveShortAbsPathOp(resolve(box, units, t, p.to))

struct QuadCurveShortRelPathOp <: PathOp
    to::Vec
end

function parsepathop(::Type{QuadCurveShortRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(QuadCurveShortRelPathOp, tokens, i, 2)
    op = QuadCurveShortRelPathOp((tokens[i], tokens[i + 1]))
    return (op, i + 2)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::QuadCurveShortRelPathOp) =
        QuadCurveShortRelPathOp(
            (resolve(box, units, t, p.to[1]),
             resolve(box, units, t, p.to[2])))

struct ArcAbsPathOp <: PathOp
    rx::Measure
    ry::Measure
    rotation::Float64
    largearc::Bool
    sweep::Bool
    to::Vec
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::ArcAbsPathOp) =
        ArcAbsPathOp(
            resolve(box, units, t, p.rx),
            resolve(box, units, t, p.ry),
            p.rotation,
            p.largearc,
            p.sweep,
            resolve(box, units, t, p.to))

struct ArcRelPathOp <: PathOp
    rx::Measure
    ry::Measure
    rotation::Float64
    largearc::Bool
    sweep::Bool
    to::Vec
end

function parsepathop(::Type{T}, tokens::AbstractArray, i) where T <: Union{ArcAbsPathOp, ArcRelPathOp}
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

    isa(tokens[i + 2], Number) || error("path arc operation requires a numerical rotation")

    op = T(x_measure(tokens[i]),
           y_measure(tokens[i + 1]),
           convert(Float64, tokens[i + 2]),
           largearc, sweep,
           (x_measure(tokens[i + 5]), y_measure(tokens[i + 6])))

    return (op, i + 7)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::ArcRelPathOp) =
        ArcRelPathOp(
            resolve(box, units, t, p.rx),
            resolve(box, units, t, p.ry),
            p.rotation,
            p.largearc,
            p.sweep,
            (resolve(box, units, t, p.to[1]),
             resolve(box, units, t, p.to[2])))

const path_ops = Dict(
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

struct PathPrimitive <: FormPrimitive
    ops::Vector{PathOp}
end

const Path = Form{PathPrimitive}

path(tokens::AbstractArray, tag=empty_tag) = Path([PathPrimitive(parsepath(tokens))], tag)

path(tokens::AbstractArray{T}, tag=empty_tag) where T <: AbstractArray =
        Path([PathPrimitive(parsepath(ts)) for ts in tokens], tag)

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::PathPrimitive) =
        PathPrimitive([resolve(box, units, t, op) for op in p.ops])

# TODO: boundingbox



# From cairo_backends.jl:

function draw(img::Image, prim::PathPrimitive)
    for op in prim.ops
        draw_path_op(img, op)
    end
    fillstroke(img)
end

draw_path_op(img::Image, op::MoveAbsPathOp) = move_to(img, op.to)
draw_path_op(img::Image, op::MoveRelPathOp) = rel_move_to(img, op.to)
draw_path_op(img::Image, op::ClosePathOp)   = close_path(img)
draw_path_op(img::Image, op::LineAbsPathOp) = line_to(img, op.to)
draw_path_op(img::Image, op::LineRelPathOp) = rel_line_to(img, op.to)

function draw_path_op(img::Image, op::HorLineAbsPathOp)
    pos = current_point(img)
    line_to(img, (op.x, pos.y))
end

draw_path_op(img::Image, op::HorLineRelPathOp) = rel_line_to(img, (op.Δx, 0.0mm))

function draw_path_op(img::Image, op::VertLineAbsPathOp)
    pos = current_point(img)
    line_to(img, (pos.x, op.y))
end

draw_path_op(img::Image, op::VertLineRelPathOp) = rel_line_to(img, (0.0mm, op.Δy))

function draw_path_op(img::Image, op::CubicCurveAbsPathOp)
    curve_to(img, op.ctrl1, op.ctrl2, op.to)
    img.last_ctrl2_point = op.ctrl2
end

function draw_path_op(img::Image, op::CubicCurveRelPathOp)
    xy = current_point(img)
    rel_curve_to(img, op.ctrl1, op.ctrl2, op.to)
    img.last_ctrl2_point = (op.ctrl2[1] + xy[1], op.ctrl2[2] + xy[2])
end

function draw_path_op(img::Image, op::CubicCurveShortAbsPathOp)
    xy = current_point(img)
    ctrl1 = img.last_ctrl2_point
    if ctrl1 === nothing
        ctrl1 = xy
    else
        ctrl1 = (2*xy[1] - ctrl1[1], 2*xy[2] - ctrl1[2])
    end
    curve_to(img, ctrl1, op.ctrl2, op.to)
    img.last_ctrl2_point = op.ctrl2
end

function draw_path_op(img::Image, op::CubicCurveShortRelPathOp)
    xy = current_point(img)
    x1, y1 = xy[1].value, xy[2].value
    x2, y2 = op.to[1].value, op.to[2].value

    ctrl1 = img.last_ctrl2_point
    if ctrl1 === nothing
        ctrl1 = xy
    else
        ctrl1 = (Measure(abs=(2*x1 - ctrl1[1].value) - x1),
                 Measure(abs=(2*y1 - ctrl1[2].value) - y1))
    end
    cx, cy = ctrl1[1].value, ctrl1[2].value

    rel_curve_to(img, ctrl1, op.ctrl2, op.to)
    img.last_ctrl2_point =
        (Measure(abs=op.ctrl2[1].value + xy.x.abs),
         Measure(abs=op.ctrl2[2].value + xy.y.abs))
end

function draw_path_op(img::Image, op::QuadCurveAbsPathOp)
    xy = current_point(img)
    x1, y1 = xy[1].value, xy[2].value
    x2, y2 = op.to[1].value, op.to[2].value
    cx, cy = op.ctrl1[1].value, op.ctrl1[2].value
    curve_to(img,
             (Measure(abs=(x1 + 2*cx)/3),
              Measure(abs=(y1 + 2*cy)/3)),
             (Measure(abs=(x2 + 2*cx)/3),
              Measure(abs=(y2 + 2*cy)/3)),
             op.to)
    img.last_ctrl1_point = op.ctrl1
end

function draw_path_op(img::Image, op::QuadCurveRelPathOp)
    xy = current_point(img)
    x1, y1 = xy[1].value, xy[2].value
    x2, y2 = op.to[1].value, op.to[2].value
    cx, cy = op.ctrl1[1].value, op.ctrl1[2].value
    rel_curve_to(img,
                 (Measure(abs=(x1 + 2*cx)/3),
                  Measure(abs=(y1 + 2*cy)/3)),
                 (Measure(abs=(x2 + 2*cx)/3),
                  Measure(abs=(y2 + 2*cy)/3)),
             op.to)
    img.last_ctrl1_point =
        (Measure(abs=op.ctrl1[1].value + xy.x.abs),
         Measure(abs=op.ctrl1[2].value + xy.y.abs))
end

function draw_path_op(img::Image, op::QuadCurveShortAbsPathOp)
    xy = current_point(img)
    x1, y1 = xy[1].value, xy[2].value
    x2, y2 = op.to[1].value, op.to[2].value

    ctrl1 = img.last_ctrl1_point
    if img.last_ctrl1_point === nothing
        ctrl1 = xy
    else
        ctrl1 = (Measure(abs=2*x1 - ctrl1[1].value),
                 Measure(abs=2*y1 - ctrl1[2].value))
    end
    cx, cy = ctrl1[1].value, ctrl1[2].value

    curve_to(img,
             (Measure(abs=(x1 + 2*cx)/3),
              Measure(abs=(y1 + 2*cy)/3)),
             (Measure(abs=(x2 + 2*cx)/3),
              Measure(abs=(y2 + 2*cy)/3)),
             (Measure(abs=x2), Measure(abs=y2)))
    img.last_ctrl1_point = ctrl1
end

function draw_path_op(img::Image, op::QuadCurveShortRelPathOp)
    xy = current_point(img)
    x1, y1 = xy[1].value, xy[2].value
    x2, y2 = x1 + op.to[1].value, y1 + op.to[2].value

    ctrl1 = img.last_ctrl1_point
    if ctrl1 === nothing
        ctrl1 = xy
    else
        ctrl1 = (Measure(abs=(2*x1 - ctrl1[1].value) - x1),
                 Measure(abs=(2*y1 - ctrl1[2].value) - y1))
    end
    cx, cy = ctrl1[1].value, ctrl1[2].value

    rel_curve_to(img,
                 (Measure(abs=(x1 + 2*cx)/3),
                  Measure(abs=(y1 + 2*cy)/3)),
                 (Measure(abs=(x2 + 2*cx)/3),
                  Measure(abs=(y2 + 2*cy)/3)),
                 (Measure(abs=x2), Measure(abs=y2)))
    img.last_ctrl1_point =
        (Measure(abs=op.ctrl1[1].value + x1),
         Measure(abs=op.ctrl1[2].value + y1))
end

function draw_path_op(img::Image, op::ArcAbsPathOp)
    xy = current_point(img)
    x1, y1 = xy[1].value, xy[2].value
    x2, y2 = op.to[1].value, op.to[2].value
    rx, ry = op.rx.abs, op.ry.abs
    φ = deg2rad(op.rotation)
    draw_endpoint_arc(img, rx, ry, φ, op.largearc, op.sweep, x1, y1, x2, y2)
end

function draw_path_op(img::Image, op::ArcRelPathOp)
    xy = current_point(img)
    x1, y1 = xy[1].value, xy[2].value
    x2, y2 = x1 + op.to[1].value, y1 + op.to[2].value
    rx, ry = op.rx.abs, op.ry.abs
    φ = deg2rad(op.rotation)
    draw_endpoint_arc(img, rx, ry, φ, op.largearc, op.sweep, x1, y1, x2, y2)
end

# Draw an SVG style elliptical arc
function draw_endpoint_arc(img::Image, rx::Float64, ry::Float64, φ::Float64,
                           largearc::Bool, sweep::Bool,
                           x1::Float64, y1::Float64,
                           x2::Float64, y2::Float64)
    function uvangle(ux, uy, vx, vy)
        t = (ux * vx + uy * vy) / (sqrt(ux^2 + uy^2) * sqrt(vx^2 + vy^2))
        t = max(min(t, 1.0), -1.0)
        return (ux * vy - uy * vx < 0.0 ? -1 : 1.0) * acos(t)
    end

    # From: http://www.w3.org/TR/SVG/implnote.html#ArcConversionEndpointToCenter
    xm, ym = (x1 - x2)/2, (y1 - y2)/2
    x1p =  cos(φ) * xm + sin(φ) * ym
    y1p = -sin(φ) * xm + cos(φ) * ym

    u = (rx^2 * ry^2 - rx^2 * y1p^2 - ry^2 * x1p^2) / (rx^2 * y1p^2 + ry^2 * x1p^2)
    u = u >= 0.0 ? sqrt(u) : 0.0

    cxp =  u * (rx * y1p) / ry
    cyp = -u * (ry * x1p) / rx
    if sweep == largearc
        cxp = -cxp
        cyp = -cyp
    end
    cx = (x1 + x2)/2 + cos(φ) * cxp - sin(φ) * cyp
    cy = (y1 + y2)/2 + sin(φ) * cxp + cos(φ) * cyp

    θ1 = uvangle(1.0, 0.0, (x1p - cxp) / rx, (y1p - cyp) / ry)
    Δθ = uvangle((x1p - cxp) / rx, (y1p - cyp) / ry,
                 (-x1p - cxp) / rx, (-y1p - cyp) / ry) % (2.0*π)
    if Δθ > 0.0 && !sweep
        Δθ -= 2*π
    elseif Δθ < 0.0 && sweep
        Δθ += 2*π
    end

    Cairo.save(img.ctx)
    Cairo.translate(img.ctx,
                    absolute_native_units(img, cx),
                    absolute_native_units(img, cy))
    Cairo.rotate(img.ctx, φ)
    Cairo.scale(img.ctx, rx, ry)
    if sweep
        arc(img, 0.0, 0.0, 1.0, θ1, θ1 + Δθ)
    else
        arc_negative(img, 0.0, 0.0, 1.0, θ1, θ1 + Δθ)
    end
    Cairo.restore(img.ctx)
end


# From svg.jl:

function svg_print_path_op(io::IO, op::MoveAbsPathOp)
    print(io, 'M')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::MoveRelPathOp)
    print(io, 'm')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

svg_print_path_op(io::IO, op::ClosePathOp) = print(io, 'z')

function svg_print_path_op(io::IO, op::LineAbsPathOp)
    print(io, 'L')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::LineRelPathOp)
    print(io, 'l')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::HorLineAbsPathOp)
    print(io, 'H')
    svg_print_float(io, op.x.value)
end

function svg_print_path_op(io::IO, op::HorLineRelPathOp)
    print(io, 'h')
    svg_print_float(io, op.Δx.value)
end

function svg_print_path_op(io::IO, op::VertLineAbsPathOp)
    print(io, 'V')
    svg_print_float(io, op.y.value)
end

function svg_print_path_op(io::IO, op::VertLineRelPathOp)
    print(io, 'v')
    svg_print_float(io, op.Δy.value)
end

function svg_print_path_op(io::IO, op::CubicCurveAbsPathOp)
    print(io, 'C')
    svg_print_float(io, op.ctrl1[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl1[2].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::CubicCurveRelPathOp)
    print(io, 'c')
    svg_print_float(io, op.ctrl1[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl1[2].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::CubicCurveShortAbsPathOp)
    print(io, 'S')
    svg_print_float(io, op.ctrl2[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::CubicCurveShortRelPathOp)
    print(io, 's')
    svg_print_float(io, op.ctrl2[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::QuadCurveAbsPathOp)
    print(io, 'Q')
    svg_print_float(io, op.ctrl1[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl1[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::QuadCurveRelPathOp)
    print(io, 'q')
    svg_print_float(io, op.ctrl1[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl1[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::QuadCurveShortAbsPathOp)
    print(io, 'T')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::QuadCurveShortRelPathOp)
    print(io, 't')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::ArcAbsPathOp)
    print(io, 'A')
    svg_print_float(io, op.rx.value)
    print(io, ' ')
    svg_print_float(io, op.ry.value)
    print(io, ' ')
    svg_print_float(io, op.rotation)
    print(io, ' ',
          op.largearc ? 1 : 0, ' ',
          op.sweep ? 1 : 0, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::ArcRelPathOp)
    print(io, 'a')
    svg_print_float(io, op.rx.value)
    print(io, ' ')
    svg_print_float(io, op.ry.value)
    print(io, ' ')
    svg_print_float(io, op.rotation)
    print(io, ' ',
          op.largearc ? 1 : 0, ' ',
          op.sweep ? 1 : 0, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function draw(img::SVG, prim::PathPrimitive, idx::Int)
    indent(img)
    print(img.out, "<path d=\"")
    for op in prim.ops
        svg_print_path_op(img.out, op)
    end
    print(img.out, '"')
    print_vector_properties(img, idx)
    print(img.out, "/>\n")
end



