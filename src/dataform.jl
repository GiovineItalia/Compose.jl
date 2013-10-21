


# Data forms are forms applied over vectors. The purpose is twofold: it's a
# slightly more convenient way of generating a bunch of similar forms, and
# secondly, it allows us to exploit some of the nicer features of d3.


# A DataProperty is a leaf node that may only be attached to a DataForm.
type DataProperty{T <: PropertyPrimitive}
    ds::Vector{AbstractArray}
end


primitive{T}(dp::DataProperty{T}) = T


fill(ds::AbstractArray) = DataProperty{Fill}(AbstractArray[ds])
stroke(ds::AbstractArray) = DataProperty{Stroke}(AbstractArray[ds])
svgclass(ds::AbstractArray) = DataProperty{SVGClass}(AbstractArray[ds])
d3embed(code::String) = DataProperty{D3Embed}(AbstractArray[[code]])



type DataForm{T <: FormPrimitive} <: Form
    # A matrix of parameters to T's constructor.
    ds::Vector{AbstractArray}

    # A list of DataProperty objects applied to the generated Forms in parallel.
    dataprops::List{DataProperty}

    # Scalar property applied to every form.
    property::Property

    # shallow copy constructor
    function DataForm(a::DataForm{T})
        new(a.ds, a.dataprops, a.property)
    end

    function DataForm(ds::Vector{AbstractArray})
        new(ds, ListNil{DataProperty}(), empty_property)
    end
end


DataForm{T}(a::DataForm{T}) = DataForm{T}(a)


primitive{T}(df::DataForm{T}) = T
removable(df::DataForm) = false
children(df::DataForm) = []


copy(df::DataForm) = DataForm(df)


function compose(df::DataForm, p::Property)
    df = copy(df)
    df.property = combine(df.property, p)
    df
end


# Parallel compose.
function compose(df::DataForm, dp::DataProperty)
    df = copy(df)
    df.dataprops = cons(dp, df.dataprops)
    df
end


# DataProperties with regular forms.
compose(f::FormTree, dp::DataProperty) = compose(f, convert(PropertySeq, dp))


# DataForm constructors
function ellipse(x::AbstractArray, y::AbstractArray,
                 rx::AbstractArray, ry::AbstractArray)
    DataForm{Ellipse}(AbstractArray[x, y, rx, ry])
end

function circle(x::AbstractArray, y::AbstractArray, r::AbstractArray)
    ellipse(x, y, r, r)
end

function lines(pointss::AbstractArray...)
    DataForm{Lines}(AbstractArray[[[convert(Point, point) for point in points]
                                   for points in pointss]])
end


# Convert a DataProperty to a sequence of properties for processing by a non-d3
# backend.
function convert(::Type{PropertySeq}, dp::DataProperty)
    T = primitive(dp)
    PropertySeq(T([d[1] for d in dp.ds]...))
end


# Compose data properties with canvases
compose(a::CanvasTree, b::DataProperty) = compose(a, convert(PropertySeq, b))


# Default handling of data forms. Generate scalar forms and feed them into the
# backend.
function draw{T}(backend::Backend, t::Transform, unit_box::UnitBox,
                 box::AbsoluteBoundingBox, form::DataForm{T})
    # generate properties
    n = maximum(map(length, form.ds))
    ps = Property[empty_property for _ in 1:n]
    for dataprop in form.dataprops
        T = primitive(dataprop)
        for (i, row) in enumerate(zip({take(cycle(d), n) for d in dataprop.ds}...))
            ps[i] = combine(ps[i], PropertySeq(T(row...)))
        end
    end

    # combine with non-data properties
    for i in 1:n
        ps[i] = combine(ps[i], form.property)
    end

    # generate scalar forms
    for (i, row) in enumerate(zip([take(cycle(d), n) for d in form.ds]...))
        push_property(backend, ps[i])
        draw(backend, t, unit_box, box, T(row...))
        pop_property(backend)
    end
end


# Generate a d3 '.attr(...)' expression for x-coordinate values.
#
# Args:
#   backend: D3 backend instance.
#   ds: Data vector.
#   unit_box: Parent canvases coordinate system.
#   box: Parent canvases absolute bounding box.
#   dataindexes: Dict mapping indexes in the backend's data matrix to an index
#               within the zipped row.
#
# Returns:
#   A string containing a d3.js ".attr" expression.
#
# Modifies:
#   dataindexes
#
function make_d3_x_attr{T, N}(backend::D3, ds::AbstractArray{T, N},
                              t::Transform, unit_box::UnitBox,
                              box::AbsoluteBoundingBox,
                              dataindexes::Dict{Int, Int})

    # For singleton values, we compute the transform in place.
    if length(ds) == 1
        svg_fmt_float(absolute_x_position(x_measure(ds[1]), t, unit_box, box))
    else
        ds = Float64[absolute_x_position(x_measure(d), t, unit_box, box)
                     for d in ds]

        idx = data_idx(backend, ds)
        if !haskey(dataindexes, idx)
            dataindexes[idx] = length(dataindexes)
        end
        rowidx = dataindexes[idx]

        @sprintf("function(d) { return d[%d]; }", rowidx)
    end
end


function make_d3_width_attr{T, N}(backend::D3, ds::AbstractArray{T, N},
                                  t::Transform,
                                  unit_box::UnitBox,
                                  box::AbsoluteBoundingBox,
                                  dataindexes::Dict{Int, Int})
    if length(ds) == 1
        svg_fmt_float(absolute_units(x_measure(ds[1]), t, unit_box, box))
    else
        ds = Float64[absolute_x_position(x_measure(d), t, unit_box, box)
                     for d in ds]

        idx = data_idx(backend, ds)
        if !haskey(dataindexes, idx)
            dataindexes[idx] = length(dataindexes)
        end
        rowidx = dataindexes[idx]
        @sprintf("function(d) { return d[%d]; }", rowidx)
    end
end


# Generate a d3 '.attr(...)' expression for y-coordinate values.
#
# Args:
#   backend: D3 backend instance.
#   ds: Data vector.
#   unit_box: Parent canvases coordinate system.
#   box: Parent canvases absolute bounding box.
#   dataindexes: Dict mapping indexes in the backend's data matrix to an index
#               within the zipped row.
#
# Returns:
#   A string containing a d3.js ".attr" expression.

# Modifies:
#   dataindexes
#
function make_d3_y_attr{T, N}(backend::D3, ds::AbstractArray{T, N},
                              t::Transform,
                              unit_box::UnitBox,
                              box::AbsoluteBoundingBox,
                              dataindexes::Dict{Int, Int})

    # For singleton values, we compute the transform in place.
    if length(ds) == 1
        svg_fmt_float(absolute_y_position(y_measure(ds[1]), t, unit_box, box))
    else
        ds = Float64[absolute_y_position(y_measure(d), t, unit_box, box)
                     for d in ds]

        idx = data_idx(backend, ds)
        if !haskey(dataindexes, idx)
            dataindexes[idx] = length(dataindexes)
        end
        rowidx = dataindexes[idx]
        @sprintf("function(d) { return d[%d]; }", rowidx)
    end
end


function make_d3_height_attr{T, N}(backend::D3, ds::AbstractArray{T, N},
                                   t::Transform,
                                   unit_box::UnitBox,
                                   box::AbsoluteBoundingBox,
                                   dataindexes::Dict{Int, Int})
    # For singleton values, we compute the transform in place.
    if length(ds) == 1
        svg_fmt_float(absolute_units(y_measure(ds[1]), t, unit_box, box))
    else
        ds = Float64[absolute_units(y_measure(ds[1]), t, unit_box, box)
                     for d in ds]

        idx = data_idx(backend, ds)
        if !haskey(dataindexes, idx)
            dataindexes[idx] = length(dataindexes)
        end
        rowidx = dataindexes[idx]
        @sprintf("function(d) { return d[%d]; }", rowidx)
    end
end

# Build a series of ".attr" expressions representing properties applied to a
# data form.
function make_d3_data_property_expr(backend::D3,
                                    t::Transform,
                                    unit_box::UnitBox,
                                    box::AbsoluteBoundingBox,
                                    dataindexes::Dict{Int, Int},
                                    dps::List{DataProperty})
    expr = IOBuffer()
    for dp in dps
        print(expr, make_d3_data_property_expr(backend, t, unit_box,
                                               box, dataindexes, dp))
    end
    takebuf_string(expr)
end


function make_d3_data_property_expr(backend::D3,
                                    t::Transform,
                                    unit_box::UnitBox,
                                    box::AbsoluteBoundingBox,
                                    dataindexes::Dict{Int, Int},
                                    dp::DataProperty{Fill})
    colors = dp.ds[1]
    if length(colors) == 1
        @sprintf(".attr(\"fill\", %s)\n", json(color(colors[1])))
    else
        idx = data_idx(backend, colors)
        if !haskey(dataindexes, idx)
            dataindexes[idx] = length(dataindexes)
        end
        rowidx = dataindexes[idx]
        @sprintf(".attr(\"fill\", function(d, i) { return d[%d]; })\n", rowidx)
    end
end


function make_d3_data_property_expr(backend::D3,
                                    t::Transform,
                                    unit_box::UnitBox,
                                    box::AbsoluteBoundingBox,
                                    dataindexes::Dict{Int, Int},
                                    dp::DataProperty{Stroke})
    colors = dp.ds[1]
    if length(colors) == 1
        @sprintf(".attr(\"stroke\", %s)\n", json(color(colors[1])))
    else
        idx = data_idx(backend, colors)
        if !haskey(dataindexes, idx)
            dataindexes[idx] = length(dataindexes)
        end
        rowidx = dataindexes[idx]
        @sprintf(".attr(\"stroke\", function(d) { return d[%d]; })\n", rowidx)
    end
end


function make_d3_data_property_expr(backend::D3,
                                    t::Transform,
                                    unit_box::UnitBox,
                                    box::AbsoluteBoundingBox,
                                    dataindexes::Dict{Int, Int},
                                    dp::DataProperty{SVGClass})
    classes = dp.ds[1]
    if length(classes) == 1
        @sprintf(".attr(\"class\", \"%s\")\n", escape_string(classes[1]))
    else
        idx = data_idx(backend, classes)
        if !haskey(dataindexes, idx)
            dataindexes[idx] = length(dataindexes)
        end
        rowidx = dataindexes[idx]
        @sprintf(".attr(\"class\", function(d) { return d[%d]; })\n", rowidx)
    end
end


function make_d3_data_property_expr(backend::D3,
                                    t::Transform,
                                    unit_box::UnitBox,
                                    box::AbsoluteBoundingBox,
                                    dataindexes::Dict{Int, Int},
                                    dp::DataProperty{D3Embed})
    string(dp.ds[1][1], "\n")
end



# Build expression suitable for the argument of a '.data(...)' call.
#
# Args:
#   dataindexes: Dict mapping indexes in the backend's data matrix to an index
#               within the zipped row.
#
function make_d3_data_expr(dataindexes::Dict{Int, Int})
    idxpairs = Array(Tuple, length(dataindexes))
    for (i, (k, v)) in enumerate(dataindexes)
        idxpairs[i] = (v, k)
    end
    sort!(idxpairs)
    @sprintf("d3.zip(%s)",
             join(["data[$(idxpair[2])]" for idxpair in idxpairs], ","))
end


function draw(backend::D3, t::Transform, unit_box::UnitBox,
              box::AbsoluteBoundingBox, form::DataForm{Ellipse})

    class = next_dataform_class(backend)
    x, y, rx, ry = form.ds

    dataindexes = Dict{Int, Int}()
    x_expr = make_d3_x_attr(backend, x, t, unit_box, box, dataindexes)
    y_expr = make_d3_y_attr(backend, y, t, unit_box, box, dataindexes)

    dp_expr = make_d3_data_property_expr(backend, t, unit_box, box,
                                         dataindexes, form.dataprops)
    push_property(backend, form.property)

    if rx == ry
        r_expr = make_d3_width_attr(backend, rx, t, unit_box, box, dataindexes)
        @printf(backend.out,
                "g.selectAll(\"%s\")
                  .data(%s)
                  .enter()
                  .append(\"circle\")\n",
                  class, make_d3_data_expr(dataindexes))
        @printf(backend.out, ".attr(\"cx\", %s)\n", x_expr)
        @printf(backend.out, ".attr(\"cy\", %s)\n", y_expr)
        @printf(backend.out, ".attr(\"r\", %s)\n", r_expr)
    else
        rx_expr = make_d3_width_attr(backend, rx, t, unit_box, box, dataindexes)
        ry_expr = make_d3_height_attr(backend, ry, t, unit_box, box, dataindexes)
        @printf(backend.out,
                "g.selectAll(\"%s\")
                  .data(%s)
                  .enter()
                  .append(\"ellipse\")\n",
                  class, make_d3_data_expr(dataindexes))
        @printf(backend.out, ".attr(\"cx\", %s)\n", x_expr)
        @printf(backend.out, ".attr(\"cy\", %s)\n", y_expr)
        @printf(backend.out, ".attr(\"rx\", %s)\n", rx_expr)
        @printf(backend.out, ".attr(\"ry\", %s)\n", ry_expr)
    end

    print(backend.out, dp_expr)
    print(backend.out, ";\n")
    pop_property(backend)
end

