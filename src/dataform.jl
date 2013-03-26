
# Forms bound to a data frame.


type DataForm{T <: FormPrimitive} <: FormPrimitive
    df::AbstractDataFrame

    # An index should either be a Symbol giving the name of a column, or some
    # constant that will be used verbatim.
    idxs::Vector
end


# DataForm constructors
function ellipse(df::AbstractDataFrame, x, y, rx, ry)
    FormTree(DataForm{Ellipse}(df, [x, y, rx, ry]))
end

function circle(df::AbstractDataFrame, x, y, r)
    ellipse(df, x, y, r, r)
end



# Default handling of data forms. Generate scalar forms and feed them into the
# backend.
function draw{T}(backend::Backend, t::NativeTransform, unit_box::BoundingBox,
                 box::NativeBoundingBox, form::DataForm{T})
    row = Array(Any, length(form.idxs))
    for i in 1:size(form.df)[1]
        for (j, idx) in enumerate(form.idxs)
            if typeof(idx) === Symbol
                row[j] = form.df[i, idx]
            else
                row[j] = idx
            end
        end
        draw(backend, t, unit_box, box, T(row...))
    end
end




# Generate a javascript to convert a canvas x-coordinate.
#
# Args:
#   x_idx: Either a symbol, indicating an index into a data frame, or
#          a scalar number.
function make_d3_xconv(x_idx::Union(Symbol, Real),
                       unit_box::BoundingBox,
                       box::NativeBoundingBox)
    mx = box.width.value / unit_box.width.value
    bx = box.x0.value + unit_box.x0.value * box.width.value / unit_box.width.value
    if typeof(x_idx) === Symbol
        x = @sprintf("function(d) {
                         return d[\"%s\"] * %f + %f;
                     }",
                     x_idx, mx, bx)
    else
        x = "$(x_idx * mx + bx)"
    end
    x
end


# Generate a javascript to convert a canvas x-coordinate.
#
# Args:
#   y_idx: Either a symbol, indicating an index into a data frame, or
#          a scalar number.
function make_d3_yconv(y_idx::Union(Symbol, Real),
                       unit_box::BoundingBox,
                       box::NativeBoundingBox)
    my = box.height.value / unit_box.height.value
    by = box.y0.value + unit_box.y0.value * box.height.value / unit_box.height.value
    if typeof(y_idx) === Symbol
        y = @sprintf("function(d) {
                         return d[\"%s\"] * %f + %f;
                     }",
                     y_idx, my, by)
    else
        y = "$(y_idx * my + by)"
    end
    y
end


function draw(backend::D3, t::NativeTransform, unit_box::BoundingBox,
              box::NativeBoundingBox, form::DataForm{Ellipse})


    # What should this look like? Do we need to introduce a new group for every
    # form. It seems the d3 idiom of d3.select("circle")

    # 1. We need to generate a unique class, if one is not given.
    # 2.
    # 3.


    x_idx, y_idx, rx_idx, ry_idx = form.idxs

    x = make_d3_xconv(x_idx, unit_box, box)
    y = make_d3_yconv(y_idx, unit_box, box)
    rx = make_d3_xconv(rx_idx, unit_box, box)
    ry = make_d3_yconv(ry_idx, unit_box, box)
    class = next_dataform_class(backend)
    dataexpr = get_data_expr(backend, form.df)

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
                class, dataexpr, x, y, rx)
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


# Here's how we are going to pull this off. Instead of implementing the above,
# we implement.
    #function draw(backend::Backend, t::NativeTransform, unit_box::BoundingBox,
                  #box::NativeBoundingBox, form::DataForm)
    #end
# Then, we implement a special version for the D3 backend that generates the
# proper javascript transformation.


# Syntax will look something like this, I imagine.
#   circle(df, "x", "y", :(sqrt(r))) <<
#       on("mouseover",
#           """
#           d3.select()
#             .transition()
#             .remove())
#

# What will creating polygons look like? Or really anything without a fixed
# number of arguments to the constructor?
#
# So we need a way of specifying column-wise data forms as well as row-wise.




