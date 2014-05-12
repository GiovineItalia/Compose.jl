

# A special kind of container promise that performs table layout optimization.

type Table <: ContainerPromise
    # Direct children must be Contexts, and not just Containers.
    children::Matrix{Vector{Context}}

    # In the formulation of the table layout problem used here, we are trying
    # find a feasible solution in which the width + height of a particular cell
    # in the table is maximized.
    focused_cell::(Int, Int)

    # Coordinate system used for children
    units::UnitBox

    # Z-order of this context relative to its siblings.
    order::Int

    # Ignore this context and everything under it if we are
    # not drawing to the javascript backend.
    withjs::Bool

    # Root context should use its parents units
    units_inherited::Bool

    function Table(m::Integer, n::Integer, focus::Tuple;
                   units=UnitBox(), units_inherited=false, order=0, withjs=false)
        tbl = new(Array(Vector{Context}, (m, n)), (int(focus[1]), int(focus[2])),
                  units, order, withjs, units_inherited)
        for i in 1:m, j in 1:n
            tbl.children[i, j] = Array(Context, 0)
        end
        return tbl
    end
end

const table = Table


function getindex(t::Table, i::Integer, j::Integer)
    return t.children[i, j]
end


function setindex!(t::Table, child, i::Integer, j::Integer)
    t.children[i, j] = child
end


if Pkg.installed("JuMP") != nothing && Pkg.installed("GLPKMathProgInterface") != nothing
    using JuMP

    function realize(tbl::Table, drawctx::ParentDrawContext)
        model = Model()

        m, n = size(tbl.children)

        abswidth = drawctx.box.width
        absheight = drawctx.box.height

        c_indexes = {}
        for i in 1:m, j in 1:n
            if length(tbl.children[i, j]) > 1
                for k in 1:length(tbl.children[i, j])
                    push!(c_indexes, (i, j, k))
                end
            end
        end

        # 0-1 configuration variables for every cell with multiple configurations
        @defVar(model, 0 <= c[1:length(c_indexes)] <= 1, Int)

        # width for every column
        @defVar(model, 0 <= w[1:n] <= abswidth)

        # height for every row
        @defVar(model, 0 <= h[1:m] <= absheight)

        # maximize the "size" of the focused cell
        i_obj, j_obj = tbl.focused_cell
        @setObjective(model, Max, w[j_obj] + h[i_obj])

        # configurations are mutually exclusive
        for cgroup in groupby(1:length(c_indexes),
                              l -> (c_indexes[l][1], c_indexes[l][2]))
            @addConstraint(model, sum{c[l], l=cgroup} == 1)
        end

        # minimum cell size contraints for cells with multiple configurations
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

        status = solve(model)

        if status == :Infeasible
            warn("Graphic cannot be correctly drawn at the given size.")
            # TODO: What now? Will getValue give me garbage?
        end

        # Set positions and sizes of children
        root = context(units=tbl.units, order=tbl.order,
                       units_inherited=tbl.units_inherited)

        w_solution = getValue(w)
        h_solution = getValue(h)
        c_solution = getValue(c)

        x_solution = cumsum([w_solution[j] for j in 1:n])
        y_solution = cumsum([h_solution[i] for i in 1:m])

        # set child positions according to layout solution
        for i in 1:m, j in 1:n
            if length(tbl.children[i, j]) == 1
                ctx = copy(tbl.children[i, j][1])
                ctx.box = BoundingBox(
                    (x_solution[j] - w_solution[j])*mm,
                    (y_solution[i] - h_solution[i])*mm,
                    w_solution[j]*mm, h_solution[i]*mm)
                compose!(root, ctx)
            end
        end

        for (l, (i, j, k)) in enumerate(c_indexes)
            if c_solution[l] == 1
                ctx = copy(tbl.children[i, j][k])
                ctx.box = BoundingBox(
                    (x_solution[j] - w_solution[j])*mm,
                    (y_solution[i] - h_solution[i])*mm,
                    w_solution[j]*mm, h_solution[i]*mm)
                compose!(root, ctx)
            end
        end

        return root
    end
else
    # Solve the table layout using a brute force approach, when a MILP isn't
    # available.
    function realize(tbl::Table, drawctx::ParentDrawContext)

        # Iterate through every combination of children.
        choices = [length(child) > 1 ? (1:length(child)) : [0]
                   for child in tbl.children]

        m, n = size(tbl.children)

        maxobjective = 0.0
        optimal_choice = nothing

        # minimum sizes for each column and row
        minrowheights = Array(Float64, n)
        mincolwidths = Array(Float64, m)

        function compute_mincolrow_sizes(choice)
            fill!(minrowheights, Inf)
            fill!(mincolwidths, Inf)
            for i in 1:m, j in 1:n
                if isempty(tbl.children[i, j])
                    continue
                end

                choice_ij = choice[(j-1)*m + i]
                child = tbl.children[i, j][(choice_ij == 0 ? 1 : choice_ij)]
                mw, mh = minwidth(child), minheight(child)
                if mw != nothing && mw < mincolwidths[j]
                    mincolwidths[j] = mw
                end
                if mh != nothing && mh < minrowwidths[j]
                    minrowheights[j] = mh
                end
            end

            minrowheights[!isfinite(minrowheights)] = 0.0
            mincolwidths[!isfinite(mincolwidths)] = 0.0
        end

        for choice in product(choices...)
            compute_mincolrow_sizes(choice)

            minheight = sum(minrowheights)
            minwidth = sum(mincolwidths)

            # feasible?
            if minwidth < drawctx.box.width && minheight < drawctx.box.height
                maxfocusedwidth = drawctx.box.width - minwidth + mincolwidths[tbl.focused_cell[2]]
                maxfocusedheight = drawctx.box.height - minheight + minrowheights[tbl.focused_cell[1]]
                objective = maxfocusedwidth + maxfocusedheight

                if objective > maxobjective
                    maxobjective = objective
                    optimal_choice = choice
                end
            end
        end

        if optimal_choice == nothing
            warn("Graphic cannot be correctly drawn at the given size.")
            optimal_choice = first(choices)
        end

        compute_mincolrow_sizes(optimal_choice)

        mincolwidths[tbl.focused_cell[2]] =
            drawctx.box.width - sum(mincolwidths) + mincolwidths[tbl.focused_cell[2]]
        minrowheights[tbl.focused_cell[1]] =
            drawctx.box.height - sum(minrowheights) + minrowheights[tbl.focused_cell[1]]

        w_solution = mincolwidths
        h_solution = minrowheights

        x_solution = cumsum(mincolwidths)
        y_solution = cumsum(minrowheights)

        root = context(units=tbl.units, order=tbl.order,
                       units_inherited=tbl.units_inherited)

        for i in 1:m, j in 1:n
            if length(tbl.children[i, j]) == 1
                ctx = copy(tbl.children[i, j][1])
            elseif length(tbl.children[i, j]) > 1
                idx = optimal_choice[(j-1)*m + i]
                ctx = copy(tbl.children[i, j][idx])
            end
            ctx.box = BoundingBox(
                (x_solution[j] - w_solution[j])*mm,
                (y_solution[i] - h_solution[i])*mm,
                w_solution[j]*mm, h_solution[i]*mm)
            compose!(root, ctx)
        end

        return root
    end
end

