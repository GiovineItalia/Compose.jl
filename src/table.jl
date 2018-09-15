# A special kind of container promise that performs table layout optimization.

mutable struct Table <: ContainerPromise
    # Direct children must be Contexts, and not just Containers. If
    # children[i,j] has a vector with multiple children it indicates multiple
    # possible layouts for that cell in the table.
    children::Matrix{Vector{Context}}

    # In the formulation of the table layout problem used here, we are trying
    # find a feasible solution in which the width + height of a particular
    # group of cells in the table is maximized.
    x_focus::UnitRange{Int}
    y_focus::UnitRange{Int}

    # If non-nothing, constrain the focused cells to have a proportional
    # relationship.
    x_prop::Union{Vector{Float64}, Nothing}
    y_prop::Union{Vector{Float64}, Nothing}

    # If non-nothing, constrain the focused cells to have a fixed aspect ratio.
    aspect_ratio::Union{Float64, Nothing}

    # fixed configuration
    fixed_configs::Vector

    # Coordinate system used for children
    units::Union{UnitBox, Nothing}

    # Z-order of this context relative to its siblings.
    order::Int

    # Ignore this context and everything under it if we are
    # not drawing to the javascript backend.
    withjs::Bool

    # Ignore this context if we are drawing to the SVGJS backend.
    withoutjs::Bool
end

function Table(m::Integer, n::Integer, y_focus::UnitRange{Int}, x_focus::UnitRange{Int};
               y_prop=nothing, x_prop=nothing,
               aspect_ratio=nothing,
               units=nothing, order=0, withjs=false, withoutjs=false,
               fixed_configs=Any[])

    if x_prop != nothing
        @assert length(x_prop) == length(x_focus)
        x_prop ./= sum(filter(x -> !isnan(x), x_prop))
    end

    if y_prop != nothing
        @assert length(y_prop) == length(y_focus)
        y_prop ./= sum(filter(x -> !isnan(x), y_prop))
    end

    tbl = Table(Array{Vector{Context}}(undef, (m, n)),
              x_focus, y_focus,
              x_prop, y_prop,
              aspect_ratio,
              fixed_configs,
              units, order, withjs, withoutjs)
    for i in 1:m, j in 1:n
        tbl.children[i, j] = Array{Context}(undef, 0)
    end
    return tbl
end

const table = Table

getindex(t::Table, i::Integer, j::Integer) = t.children[i, j]
setindex!(t::Table, child, i::Integer, j::Integer) = t.children[i, j] = child
size(t::Table, i::Integer) = size(t.children, i)
size(t::Table) = size(t.children)

# Adjust a table solution so that the aspect ratio matches tbl.aspect_ratio
#
# Returns:
#   true if the solution is feasibly, false if not
#
# Modifies:
#   x_solution, y_solution, w_solution, h_solution
#
function force_aspect_ratio!(tbl::Table,
                             x_solution::Vector, y_solution::Vector,
                             w_solution::Vector, h_solution::Vector)

    w0 = sum(w_solution[tbl.x_focus])
    h0 = sum(h_solution[tbl.y_focus])
    adj = (w0 / h0) / tbl.aspect_ratio

    # we can't expand either dimension (since it's presumably of maximum size)
    # so shrink one or the other.
    if adj > 1.0
        w = w0 / adj
        delta = w0 - w
        x_solution[1:tbl.x_focus.stop] .+= delta/2
        x_solution[tbl.x_focus.stop+1:end] .-= delta/2
        w_solution[tbl.x_focus] ./= adj
    else
        w = w0
        h = h0 * adj
        delta = h0 - h
        y_solution[1:tbl.y_focus.stop] .+= delta/2
        y_solution[tbl.y_focus.stop+1:end] .-= delta/2
        h_solution[tbl.y_focus] *= adj
    end
end

# Return true if the minwidth and minheight constraints on ctx are satisfied
# by the given width/height
function issatisfied(ctx::Context, width, height)
    eps = 1e-4
    return (minwidth(ctx) == nothing || width + eps >= minwidth(ctx)) &&
           (minheight(ctx) == nothing || height + eps >= minheight(ctx))
end

# Solve the table layout using a brute force approach, when a MILP isn't
# available.
function realize_brute_force(tbl::Table, drawctx::ParentDrawContext)
    m, n = size(tbl.children)

    maxobjective = 0.0
    optimal_choice = nothing
    feasible = false # is the current optimal_choice feasible

    # if the current solution is infeasible, we try to minimize badness,
    # which is basically "size needed" - "size available".
    minbadness = Inf

    focused_col_widths = Array{Float64}(undef, length(tbl.x_focus))
    focused_row_heights = Array{Float64}(undef, length(tbl.y_focus))

    # minimum sizes for each column and row
    minrowheights = Array{Float64}(undef, m)
    mincolwidths = Array{Float64}(undef, n)

    # convert tbl.fixed_configs to linear indexing
    fixed_configs = Any[
        Set([(j-1)*m + i for (i, j) in fixed_config])
        for fixed_config in tbl.fixed_configs
    ]

    # build equilavence classes of configurations basen on fixed_configs
    constrained_cells = Set()
    num_choices = [length(child) for child in tbl.children]
    num_group_choices = Any[]
    for fixed_config in fixed_configs
        push!(num_group_choices, num_choices[first(fixed_config)])
        for idx in fixed_config
            push!(constrained_cells, idx)
        end
    end

    for (idx, child) in enumerate(tbl.children)
        if length(child) > 1
            if !in(idx, constrained_cells)
                push!(num_group_choices, length(child))
                push!(fixed_configs, Set([idx]))
            end
        end
    end

    # compute the optimal column widths/row heights for fixed choice and
    # pre-computed minrowheight/mincolwidths.
    function update_focused_col_widths!(focused_col_widths)
        minwidth = sum(mincolwidths)
        total_focus_width =
            drawctx.box.a[1].value - minwidth + sum(mincolwidths[tbl.x_focus])

        if tbl.x_prop != nothing
            for k in 1:length(tbl.x_focus)
                if isnan(tbl.x_prop[k])
                    total_focus_width -= mincolwidths[tbl.x_focus[k]]
                end
            end

            for k in 1:length(tbl.x_focus)
                if !isnan(tbl.x_prop[k])
                    focused_col_widths[k] = tbl.x_prop[k] * total_focus_width
                else
                    focused_col_widths[k] = mincolwidths[tbl.x_focus[k]]
                end
            end
        else
            extra_width = total_focus_width - sum(mincolwidths[tbl.x_focus])
            for k in 1:length(tbl.x_focus)
                focused_col_widths[k] =
                        mincolwidths[tbl.x_focus[k]] + extra_width / length(tbl.x_focus)
            end
        end
    end

    function update_focused_row_heights!(focused_row_heights)
        total_focus_height =
                drawctx.box.a[2].value - sum(minrowheights) + sum(minrowheights[tbl.y_focus])

        if tbl.y_prop != nothing
            for k in 1:length(tbl.y_focus)
                if isnan(tbl.y_prop[k])
                    total_focus_height -= minrowheights(tbl.y_focus[k])
                end
            end

            for k in 1:length(tbl.y_focus)
                if !isnan(tbl.y_prop[k])
                    focused_row_heights[k] = tbl.y_prop[k] * total_focus_height
                else
                    focused_row_heights[k] = minrowheights[tbl.y_focus[k]]
                end
            end
        else
            extra_height = total_focus_height - sum(minrowheights[tbl.y_focus])
            for k in 1:length(tbl.y_focus)
                focused_row_heights[k] =
                        minrowheights[tbl.y_focus[k]] + extra_height / length(tbl.y_focus)
            end
        end
    end

    # for a given configuration, compute the minimum width for every column and
    # minimum height for every row. Return the penalty for choice.
    function update_mincolrow_sizes!(choice, minrowheights, mincolwidths)
        fill!(minrowheights, -Inf)
        fill!(mincolwidths, -Inf)
        penalty = 0.0
        for i in 1:m, j in 1:n
            isempty(tbl.children[i, j]) && continue

            choice_ij = choice[(j-1)*m + i]
            child = tbl.children[i, j][(choice_ij == 0 ? 1 : choice_ij)]
            penalty += child.penalty
            mw, mh = minwidth(child), minheight(child)
            if mw != nothing && mw > mincolwidths[j]
                mincolwidths[j] = mw
            end
            if mh != nothing && mh > minrowheights[i]
                minrowheights[i] = mh
            end
        end

        minrowheights[isfinite.(minrowheights) .== false] .= 0.0
        mincolwidths[isfinite.(mincolwidths) .== false] .= 0.0

        return penalty
    end

    it_count = 0
    group_choices = [l == 0 ? (0:0) : (1:l) for l in num_group_choices]
    choice = zeros(Int, m * n)
    optimal_choice = nothing
    if !isempty(group_choices)
        for group_choice in Iterators.product(group_choices...)
            it_count += 1
            for (l, k) in enumerate(group_choice)
                for p in fixed_configs[l]
                    choice[p] = k
                end
            end

            penalty = update_mincolrow_sizes!(choice, minrowheights, mincolwidths)

            minheightval = sum(minrowheights)
            minwidthval = sum(mincolwidths)

            update_focused_col_widths!(focused_col_widths)
            update_focused_row_heights!(focused_row_heights)

            objective = sum(focused_col_widths) + sum(focused_row_heights) - penalty

            # feasible?
            if minwidthval < drawctx.box.a[1].value && minheightval < drawctx.box.a[2].value &&
               all(focused_col_widths .>= mincolwidths[tbl.x_focus]) &&
               all(focused_row_heights .>= minrowheights[tbl.y_focus])
                if objective > maxobjective || !feasible
                    maxobjective = objective
                    minbadness = 0.0
                    optimal_choice = copy(choice)
                end
                feasible = true
            else
                badness = max(minwidthval - drawctx.box.a[1].value, 0.0) +
                          max(minheightval - drawctx.box.a[2].value, 0.0)
                if badness < minbadness && !feasible
                    minbadness = badness
                    optimal_choice = copy(choice)
                end
            end

            if optimal_choice === nothing
                optimal_choice = copy(choice)
            end
        end
    end

    if optimal_choice === nothing
        optimal_choice = zeros(Int, m * n)
    end

    update_mincolrow_sizes!(optimal_choice, minrowheights, mincolwidths)
    update_focused_col_widths!(focused_col_widths)
    update_focused_row_heights!(focused_row_heights)

    w_solution = mincolwidths
    h_solution = minrowheights

    for k in 1:length(tbl.x_focus)
        w_solution[tbl.x_focus[k]] = focused_col_widths[k]
    end

    for k in 1:length(tbl.y_focus)
        h_solution[tbl.y_focus[k]] = focused_row_heights[k]
    end

    x_solution = cumsum(mincolwidths) .- w_solution
    y_solution = cumsum(minrowheights) .- h_solution

    if tbl.aspect_ratio != nothing
        force_aspect_ratio!(tbl, x_solution, y_solution, w_solution, h_solution)
    end

    root = context(units=tbl.units, order=tbl.order)

    feasible = true
    for i in 1:m, j in 1:n
        if isempty(tbl.children[i, j])
            continue
        elseif length(tbl.children[i, j]) == 1
            ctx = copy(tbl.children[i, j][1])
        elseif length(tbl.children[i, j]) > 1
            idx = optimal_choice[(j-1)*m + i]
            ctx = copy(tbl.children[i, j][idx])
        end
        feasible == feasible && issatisfied(ctx, w_solution[j], h_solution[i])
        ctx.box = BoundingBox(
            x_solution[j]*mm, y_solution[i]*mm,
            w_solution[j]*mm, h_solution[i]*mm)
        compose!(root, ctx)
    end

    feasible || warn("Graphic may not be drawn correctly at the given size.")

    return root
end

# TODO: Enable this when we have a mechanism for making it optional
#if isinstalled("JuMP") &&
    #(isinstalled("GLPKMathProgInterface") ||
     #isinstalled("Cbc"))
    #include("table-jump.jl")
#else
realize(tbl::Table, drawctx::ParentDrawContext) = realize_brute_force(tbl, drawctx)
#end

function show(io::IO, t::Table)
    if get(io, :compact, false)
        println(io,"$(size(t.children,1))x$(size(t.children,2)) Table:")
        for i = 1:size(t.children,1)
            print(io, "  ")
            first = true
            for j = 1:size(t.children,2)
                first || print(io, ",")
                first = false
                show(io, t.children[i,j])
            end
            println(io)
        end
    else
        invoke(show, Tuple{IO, Any}, io, t)
    end
end
