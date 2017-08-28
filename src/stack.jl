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
function hstack(x0, y0, height, aligned_contexts::(Tuple{Context, VAlignment})...)
    isempty(aligned_contexts) && return context(x0, y0, 0, height)

    widths = [aligned_context[1].box.a[1] for aligned_context in aligned_contexts]
    width = sum(widths)
    total_width_units = sum_component(Length{:w, Float64}, width)

    if total_width_units > 0.0
        width -= total_width_units*w
        width += 1w
    end

    height = y_measure(height)

    root = context(x0, y0, width, height)
    x = 0w
    for (ctx, aln) in aligned_contexts
        ctx = copy(ctx)

        w_component = sum_component(Length{:w, Float64}, ctx.box.a[1])
        box_w = ctx.box.a[1]
        if w_component != 0.0
            box_w = scale_component(Length{:w, Float64},
                                    w_component / total_width_units,
                                    ctx.box.a[1])
        end

        y = ctx.box.x0[2]
        if aln == vtop
            y = 0h
        elseif aln == vcenter
            y = (height / 2) - (ctx.box.a[2] / 2)
        elseif aln == vbottom
            y = height - ctx.box.a[2]
        end

        ctx.box = BoundingBox((x, y), (box_w, ctx.box.a[2]))
        root = compose!(root, ctx)
        x += ctx.box.a[1]
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
        height = maximum([context.box.a[2] for context in contexts])
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
function vstack(x0, y0, width, aligned_contexts::(Tuple{Context, HAlignment})...)
    isempty(aligned_contexts) && return context(x0, y0, width, 0)

    heights = [aligned_context[1].box.a[2] for aligned_context in aligned_contexts]
    height = sum(heights)
    total_height_units = sum_component(Length{:h, Float64}, height)

    if total_height_units > 0.0
        height -= total_height_units*h
        height += 1h
    end

    width = x_measure(width)

    root = context(x0, y0, width, height)
    y = 0h
    for (ctx, aln) in aligned_contexts
        ctx = copy(ctx)

        h_component = sum_component(Length{:h, Float64}, ctx.box.a[2])
        box_h = ctx.box.a[2]
        if h_component != 0.0
            box_h = scale_component(Length{:h, Float64},
                                    h_component / total_height_units,
                                    ctx.box.a[2])
        end

        x = ctx.box.x0[1]
        if aln == hleft
            x = 0w
        elseif aln == hcenter
            x = (width / 2) - (ctx.box.a[1] / 2)
        elseif aln == hright
            x = width - ctx.box.a[1]
        end

        ctx.box = BoundingBox((x, y), (ctx.box.a[1], box_h))
        root = compose!(root, ctx)
        y += ctx.box.a[2]
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
        width = maximum([context.box.a[1] for context in contexts])
    end
    return vstack(x0, y0, width, [(context, hcenter) for context in contexts]...)
end
