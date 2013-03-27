
import Base.map

# TODO: rename this file to pcompose.jl


# Data forms are 

# The purpose is twofold: it's a slightly more convenient way of generating a
# bunch of similar forms, and secondly, it allows us to exploit some of the
# nicer features of d3.


# A DataProperty is a leaf node that may only be attached to a DataForm.
type DataProperty{T <: PropertyPrimitive}
    ds::Vector{AbstractArray}
end


primitive{T}(dp::DataProperty{T}) = T


#fill(
#map(::Type{Fill},   ds::AbstractArray) = DataProperty{Fill}(AbstractArray[ds])
#map(::Type{Stroke}, ds::AbstractArray) = DataProperty{Stroke}(AbstractArray[ds])
# TODO: others



type DataForm{T <: FormPrimitive} <: Form
    # A matrix of parameters to T's constructor.
    ds::Vector{AbstractArray}

    # A list of DataProperty objects applied to the generated Forms in parallel.
    dataprops::List{DataProperty}

    # Scalar property applied to every form.
    property::Property

    # shallow copy constructor
    function DataForm(a::DataForm)
        new(a.ds, a.dataprops, a.property)
    end

    function DataForm(ds::Vector{AbstractArray})
        new(ds, ListNil{DataProperty}(), empty_property)
    end
end


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
function pcompose(df::DataForm, dp::DataProperty)
    df = copy(df)
    df.dataprops = cons(dp, df.dataprops)
    df
end


# DataForm constructors
function ellipse(x::AbstractArray, y::AbstractArray,
                 rx::AbstractArray, ry::AbstractArray)
                 
    DataForm{Ellipse}(AbstractArray[x, y, rx, ry])
end

function circle(x::AbstractArray, y::AbstractArray, r::AbstractArray)
    ellipse(x, y, r, r)
end



# Default handling of data forms. Generate scalar forms and feed them into the
# backend.
function draw{T}(backend::Backend, t::NativeTransform, unit_box::BoundingBox,
                 box::NativeBoundingBox, form::DataForm{T})

    println("HERE")
    # form properties 
    ps = Property[empty_property for _ in 1:length(form.ds[1])]
    for dataprop in enumerate(form.dataprops)
        T = primitive(dataprop)
        for (i, row) in enumerate(zip(dataprop.ds...))
            ps[i] = combine(ps[i], T(row...))
        end
    end

    # TODO: traverse the non-data properties and combine them with each ps[i].

    # TODO: Handle singleton arguments

    for (i, row) in enumerate(zip(form.ds...))
        println("HERE")
        push_property(backend, ps[i])
        draw(backend, t, unit_box, box, T(row...))
        pop_property(backend)
    end
end


# Generate a javascript to convert a canvas x-coordinate.
#
# Args:
#   x_idx: Either a symbol, indicating an index into a data frame, or
#          a scalar number.
function make_d3_data_xconv(x_idx::Integer,
                            unit_box::BoundingBox,
                            box::NativeBoundingBox)
    mx = box.width.value / unit_box.width.value
    bx = box.x0.value - unit_box.x0.value * box.width.value / unit_box.width.value
    x = @sprintf("function(d) {
                     return d[%d] * %f + %f;
                 }",
                 x_idx, mx, bx)
end

function make_d3_scalar_xconv(x::Real, unit_box::BoundingBox,
                              box::NativeBoundingBox)
    mx = box.width.value / unit_box.width.value
    bx = box.x0.value - unit_box.x0.value * box.width.value / unit_box.width.value
    "$(x * mx + bx)"
end


function make_d3_scalar_xconv(x, unit_box::BoundingBox,
                              box::NativeBoundingBox)
    "\"$(x)\""
end


# Generate a javascript to convert a canvas x-coordinate.
#
# Args:
#   y_idx: Either a symbol, indicating an index into a data frame, or
#          a scalar number.
function make_d3_data_yconv(y_idx::Integer,
                            unit_box::BoundingBox,
                            box::NativeBoundingBox)
    my = box.height.value / unit_box.height.value
    by = box.y0.value - unit_box.y0.value * box.height.value / unit_box.height.value
    y = @sprintf("function(d) {
                     return d[%s] * %f + %f;
                 }",
                 y_idx, my, by)
end


function make_d3_scalar_yconv(y::Real, unit_box::BoundingBox,
                              box::NativeBoundingBox)
    my = box.height.value / unit_box.height.value
    by = box.y0.value - unit_box.y0.value * box.height.value / unit_box.height.value
    "$(y * my + by)"
end


function make_d3_scalar_yconv(y, unit_box::BoundingBox,
                              box::NativeBoundingBox)
    "\"$(y)\""
end



function draw(backend::D3, t::NativeTransform, unit_box::BoundingBox,
              box::NativeBoundingBox, form::DataForm{Ellipse})

    x, y, rx, ry = form.ds

    zipped = AbstractArray[]

    # Holy fuck is this ugly.
    rowlen = 0
    if length(x) == 1
        x_expr = make_d3_scalar_xconv(x[1], unit_box, box)
    else
        x_expr = make_d3_data_xconv(length(zipped), unit_box, box)
        push!(zipped, x)
    end

    if length(y) == 1
        y_expr = make_d3_scalar_yconv(y[1], unit_box, box)
    else
        y_expr = make_d3_data_yconv(length(zipped), unit_box, box)
        push!(zipped, y)
    end

    if length(rx) == 1
        rx_expr = make_d3_scalar_xconv(rx[1], unit_box, box)
    else
        rx_expr = make_d3_data_xconv(length(zipped), unit_box, box)
        push!(zipped, rx)
    end

    if length(ry) == 1
        ry_expr = make_d3_scalar_yconv(ry[1], unit_box, box)
    else
        ry_expr = make_d3_data_yconv(length(zipped), unit_box, box)
        push!(zipped, ry)
    end

    class = next_dataform_class(backend)
    dataexpr = data_expr(backend, zipped)

    if rx == ry
        @printf(backend.out,
                "
                g.selectAll(\"%s\")
                 .data(%s)
                 .enter()
                 .append(\"circle\")
                   .attr(\"cx\", %s)
                   .attr(\"cy\", %s)
                   .attr(\"r\", %s);
                ",
                class, dataexpr, x_expr, y_expr, rx_expr)
    else
        # TODO
        #@printf(backend.out,
                #"""
                #d3.selectAll("%s")
                  #.data(%s)
                  #.enter()
                  #.append("ellipse")
                #""",
                #class, dataexpr)
        # TODO: Now figure out what to do with x, y, rx, ry
    end
end


# What we need now: text and lines. Also, serialization of the Aesthetics object
# into JSON.


