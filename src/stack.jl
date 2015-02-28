
# Convenience function for rearranging contexts


# Create a new context containing the given contexts stacked horizontally.
#
# Args:
#  x0: X-position of the new root context
#  y0: Y-position of the new root context
#  height: Height of the root context.
#  aligned_contexts: One or more canvases accompanied with a vertical alignment
#                    specifier, giving the vertical positioning of the context.
#
function hstack(x0, y0, height, aligned_contexts::(Context, VAlignment)...)
    if isempty(aligned_contexts)
        return context(x0, y0, 0, height)
    end

    # To get the expected results, we scale width units, so that everything
    # fits.
    total_width_units = sum(Float64[context.box.width.cw
                                    for (context, _) in aligned_contexts])
    width = sum(Measure[context.box.width
                        for (context, _) in aligned_contexts])
    width = Measure(width,
                    cw=total_width_units > 0.0 ?
                        width.cw / total_width_units : 0.0)

    height = y_measure(height)

    root = context(x0, y0, width, height)
    x = Measure()
    for (context, aln) in aligned_contexts
        context = copy(context)
        context.box = copy(context.box)

        if context.box.width.cw != 0.0
            context.box =
                BoundingBox(context.box,
                    width=Measure(context.box.width,
                        cw=context.box.width.cw / total_width_units))
        end

        # Should we interpret vbottom to mean 0?
        context.box = BoundingBox(context.box, x0=x)
        if aln == vtop
            context.box = BoundingBox(context.box, y0=Measure{T}())
        elseif aln == vcenter
            context.box = BoundingBox(context.box,
                            y0=(height / 2) - (context.box.height / 2))
        elseif aln == vbottom
            context.box = BoundingBox(context.box,
                            y0=height - context.box.height)
        end

        root = compose!(root, context)
        x += context.box.width
    end

    return root
end


hstack() = context()


# Create a new context containing the given contexts stacked horizontally.
#
# This is the simple version of hstack. The root context will be placed on 0cx,
# 0cy, and its height will be the maximum of the contexts it contains. All
# contexts will be centered vertically.
#
function hstack(contexts::Context...; x0::MeasureOrNumber=0,
                y0::MeasureOrNumber=0, height=0)
    if height == 0
        height = maximum([context.box.height for context in contexts])
    end
    return hstack(x0, y0, height, [(context, vcenter) for context in contexts]...)
end


# Create a new context containing the given contexts stacked vertically.
#
# Args:
#  x0: X-position of the new root context
#  y0: Y-position of the new root context
#  width: Height of the root context.
#  aligned_contexts: One or more canvases accompanied with a horizontal alignment
#                    specifier, giving the horizontal positioning of the context.
#
function vstack(x0, y0, width, aligned_contexts::(Context, HAlignment)...)

    if isempty(aligned_contexts)
        return context(x0, y0, width, 0)
    end

    # Scale height units
    total_height_units = sum(Float64[context.box.height.ch
                                     for (context, _) in aligned_contexts])
    height = sum(Measure[context.box.height
                         for (context, _) in aligned_contexts])
    height = Measure(height,
                     ch=total_height_units > 0.0 ?
                          height.ch / total_height_units : 0.0)

    width = x_measure(width)

    root = context(x0, y0, width, height)
    y = Measure()
    for (context, aln) in aligned_contexts
        context = copy(context)
        context.box = copy(context.box)

        if context.box.height.ch != 0.0
            context.box =
                BoundingBox(context.box,
                    height=Measure(context.box.height,
                        ch=context.box.height.ch / total_height_units))
        end

        context.box = BoundingBox(context.box, y0=y)
        if aln == hleft
            context.box = BoundingBox(context.box, x0=Measure{T}())
        elseif aln == hcenter
            context.box = BoundingBox(context.box,
                            x0=(width / 2) - (context.box.width / 2))
        elseif aln == hright
            context.box = BoundingBox(context.box,
                            x0 = width - context.box.width)
        end

        root = compose(root, context)
        y += context.box.height
    end

    return root
end


vstack() = context()


# Create a new context containing the given contexts stacked horizontally.
#
# The simple version of vstack. The root context will be placed on 0cx, 0cy, and
# its width will be the maximum of the contexts it contains. All contexts will
# be centered horizontally..
#
function vstack(contexts::Context...; x0::MeasureOrNumber=0,
                y0::MeasureOrNumber=0, width::MeasureOrNumber=0)
    if width == 0
        width = max([context.box.width for context in contexts]...)
    end
    return vstack(x0, y0, width, [(context, hcenter) for context in contexts]...)
end

