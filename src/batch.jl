
"""
A form batch is a vectorized form with n primitives transformed into a simpler
representation: one primitive repositioned n times.

On certain backends this leads to more efficient drawing. For example, SVG can
be shortened by using <def> and <use> tags, and raster graphics can render the
form primitive to a back buffer and blit it into place for faster drawing.

Batching is an optimization transform that happens at draw time. There's
currently no mechanism to manually batch. E.g. contexts cannot have FormBatch
children.
"""
immutable FormBatch{P <: FormPrimitive}
    primitive::P
    offsets::Vector{AbsoluteVec2}
end


"""
Attempt to batch a form. Return a Nullable{FormBatch} which is null if the Form
could not be batched, and non-null if the original form can be replaced with teh
resulting FormBatch.
"""
function batch{P}(form::Form{P})
    return Nullable{FormBatch{P}}()
end


function batch{T <: CirclePrimitive}(form::Form{T})
    # circles can be batched if they all have the same radius.
    r = form.primitives[1].radius
    n = length(form.primitives)
    for i in 2:n
        if form.primitives[i].radius != r
            return Nullable{FormBatch{CirclePrimitive}}()
        end
    end

    prim = CirclePrimitive((0mm, 0mm), r)
    offsets = Array(AbsoluteVec2, n)
    for i in 1:n
        offsets[i] = form.primitives[i].center
    end

    return Nullable(FormBatch(prim, offsets))
end



