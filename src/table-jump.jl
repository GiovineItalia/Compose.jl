
using JuMP

function is_approx_integer(x::Float64)
    return abs(x - round(x)) < 1e-8
end

function realize(tbl::Table, drawctx::ParentDrawContext)
    model = Model()

    m, n = size(tbl.children)

    abswidth = drawctx.box.width
    absheight = drawctx.box.height

    c_indexes = @compat Tuple{Int, Int, Int}[]
    idx_cs = Dict{(@compat Tuple{Int, Int}), Vector{Int}}()
    for i in 1:m, j in 1:n
        if length(tbl.children[i, j]) > 1
            for k in 1:length(tbl.children[i, j])
                push!(c_indexes, (i, j, k))

                if haskey(idx_cs, (i, j))
                    push!(idx_cs[(i,j)], length(c_indexes))
                else
                    idx_cs[(i,j)] = [length(c_indexes)]
                end
            end
        end
    end

    penalties = Array(Float64, length(c_indexes))
    for (l, (i, j, k)) in enumerate(c_indexes)
        penalties[l] = tbl.children[i, j][k].penalty
    end

    # 0-1 configuration variables for every cell with multiple configurations
    @defVar(model, c[1:length(c_indexes)], Bin)

    # width for every column
    @defVar(model, 0 <= w[1:n] <= abswidth)

    # height for every row
    @defVar(model, 0 <= h[1:m] <= absheight)

    # maximize the "size" of the focused cells
    @setObjective(model, Max, sum{w[j], j=tbl.x_focus} +
                              sum{h[i], i=tbl.y_focus} -
                              sum{penalties[i] * c[i], i=1:length(c_indexes)})

    # optional proportionality constraints
    if tbl.x_prop != nothing
        k1 = 1
        while k1 < length(tbl.x_focus) && isnan(tbl.x_prop[k1])
            k1 += 1
        end

        for k in 2:length(tbl.x_focus)
            if isnan(tbl.x_prop[k])
                continue
            end

            j1 = tbl.x_focus[k1]
            jk = tbl.x_focus[k]
            @addConstraint(model, w[j1] / tbl.x_prop[k1] ==  w[jk] / tbl.x_prop[k])
        end
    end

    if tbl.y_prop != nothing
        k1 = 1
        while k1 < length(tbl.y_focus) && isnan(tbl.y_prop[k1])
            k1 += 1
        end

        for k in 2:length(tbl.y_focus)
            if isnan(tbl.y_prop[k])
                continue
            end

            i1 = tbl.y_focus[k1]
            ik = tbl.y_focus[k]
            @addConstraint(model, h[i1] / tbl.y_prop[k1] ==  h[ik] / tbl.y_prop[k])
        end
    end

    # fixed configuration constraints: constrain a set of cells
    # to have tho same configuration.
    for fixed_config in tbl.fixed_configs
        if isempty(fixed_config)
            continue
        end

        (i0, j0) = fixed_config[1]
        config_count = length(tbl.children[i0, j0])
        for (i, j) in fixed_config[2:end]
            for k in 1:config_count
                if !haskey(idx_cs, (i0, j0)) && !haskey(idx_cs, (i, j))
                    continue
                end
                idx_a = idx_cs[i0, j0][k]
                idx_b = idx_cs[(i,j)][k]
                @addConstraint(model, c[idx_a] == c[idx_b])
            end
        end
    end

    # configurations are mutually exclusive
    for cgroup in groupby(l -> (c_indexes[l][1], c_indexes[l][2]),
                          1:length(c_indexes))
        @addConstraint(model, sum{c[l], l=cgroup} == 1)
    end

    # minimum cell size constraints for cells with multiple configurations
    for (l, (i, j, k)) in enumerate(c_indexes)
        minw = minwidth(tbl.children[i, j][k])
        minh = minheight(tbl.children[i, j][k])
        if minw != nothing
            @addConstraint(model, w[j] >= minw * c[l])
        end

        if minh != nothing
            @addConstraint(model, h[i] >= minh * c[l])
        end
    end

    # minimum cell size constraint for fixed cells
    for i in 1:m, j in 1:n
        if length(tbl.children[i, j]) == 1
            minw = minwidth(tbl.children[i, j][1])
            minh = minheight(tbl.children[i, j][1])
            if minw != nothing
                @addConstraint(model, w[j] >= minw)
            end
            if minh != nothing
                @addConstraint(model, h[i] >= minh)
            end
        end
    end

    # widths and heights must add up
    @addConstraint(model, sum{w[i], i=1:n} == abswidth)
    @addConstraint(model, sum{h[i], i=1:m} == absheight)

    status = solve(model,suppress_warnings=true)

    w_solution = getValue(w)[:]
    h_solution = getValue(h)[:]
    c_solution = getValue(c)

    if status == :Infeasible || !all([is_approx_integer(c_solution[l])
                                      for l in 1:length(c_indexes)])
        #println(STDERR, "JuMP: Infeasible")
        # The brute force solver is better able to select between various
        # non-feasible solutions. So we let it have a go.
        return realize_brute_force(tbl, drawctx)
    end

    # Set positions and sizes of children
    root = context(units=tbl.units, order=tbl.order)

    x_solution = cumsum([w_solution[j] for j in 1:n]) .- w_solution
    y_solution = cumsum([h_solution[i] for i in 1:m]) .- h_solution

    if tbl.aspect_ratio != nothing
        force_aspect_ratio!(tbl, x_solution, y_solution,
                            w_solution, h_solution)
    end

    # set child positions according to layout solution
    feasible_eps = 1e-4
    feasible = true
    for i in 1:m, j in 1:n
        if length(tbl.children[i, j]) == 1
            ctx = copy(tbl.children[i, j][1])
            feasible == feasible && issatisfied(ctx, w_solution[j], h_solution[i])
            ctx.box = BoundingBox(
                x_solution[j]*mm, y_solution[i]*mm,
                w_solution[j]*mm, h_solution[i]*mm)
            compose!(root, ctx)
        end
    end

    for (l, (i, j, k)) in enumerate(c_indexes)
        if round(c_solution[l]) == 1
            ctx = copy(tbl.children[i, j][k])
            feasible == feasible && issatisfied(ctx, w_solution[j], h_solution[i])
            ctx.box = BoundingBox(
                x_solution[j]*mm, y_solution[i]*mm,
                w_solution[j]*mm, h_solution[i]*mm)
            compose!(root, ctx)
        end
    end

    if !feasible
        warn("Graphic may not be drawn correctly at the given size.")
    end

    return root
end
