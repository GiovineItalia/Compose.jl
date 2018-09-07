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
struct FormBatch{P <: FormPrimitive}
    primitive::P
    offsets::Vector{AbsoluteVec2}
end

"""
Attempt to batch a form. Return a Nothing singleton if the Form could not be
batched, and FormBatch object if the original form can be replaced.
"""
batch(form::Form{P}) where P = nothing

# Note: in tests using random data, this optimization wasn't worth it. I'm
# keeping it around out of hopes I find a more clever version that is
# worthwhile, or benchmarks using real data show different results.

# maximum distance between offsets to be considered redundand in mm
const offset_redundancy_threshold = 0.05

"""
Produce a new array of offsets in which near duplicate values have been removed.
"""
function filter_redundant_offsets!(offsets::Vector{AbsoluteVec2})
    isempty(offsets) && return offsets

    sort!(offsets)
    nonredundant_offsets = AbsoluteVec2[offsets[1]]
    for i in 2:length(offsets)
        # use l1 distance for perf
        d = abs(offsets[i-1][1].value - offsets[i][1].value) +
            abs(offsets[i-1][2].value - offsets[i][2].value)
        d > offset_redundancy_threshold && push!(nonredundant_offsets, offsets[i])
    end
    @show (length(offsets), length(nonredundant_offsets))

    return nonredundant_offsets
end

#=
function batch{T <: CirclePrimitive}(form::Form{T})
    # circles can be batched if they all have the same radius.
    r = form.primitives[1].radius
    n = length(form.primitives)
    for i in 2:n
        form.primitives[i].radius == r || return Nothing
    end

    prim = CirclePrimitive((0mm, 0mm), r)
    offsets = Array{AbsoluteVec2}(n)
    for i in 1:n
        offsets[i] = form.primitives[i].center
    end

    return FormBatch(prim, offsets)
end
=#

# TODO: same for polygon, rectangle, ellipse

# TODO: batch needs to be exposed as something that users can construct and
# insert into a context. It doesn't make sense to make the same polygon over and
# over and then try to convert it to FormBach.


# Don't attempt to optimize for batching if the form is smaller than this.
const batch_length_threshold = 100

"""
Count the number of unique primitives in a property, stopping when max_count is
exceeded.
"""
function count_unique_primitives(property::Property, max_count::Int)
    unique_primitives = Set{eltype(property.primitives)}()
    for primitive in property.primitives
        push!(unique_primitives, primitive)
        length(unique_primitives) > max_count && break
    end

    return length(unique_primitives)
end

"""
Remove and return vector forms and vector properties from the Context.
"""
function excise_vector_children!(ctx::Context)
    # excise vector forms
    prev_form_child = form_child = ctx.form_children
    forms = Form[]
    while !isa(form_child, ListNull)
        if length(form_child.head.primitives) > 1
            push!(forms, form_child.head)
            if prev_form_child == form_child
                prev_form_child = ctx.form_children = form_child.tail
            else
                prev_form_child.tail = form_child.tail
            end
        else
            prev_form_child = form_child
        end

        form_child = form_child.tail
    end

    # excise vector properties
    prev_property_child = property_child = ctx.property_children
    properties = Property[]
    while !isa(property_child, ListNull)
        if length(property_child.head.primitives) > 1
            push!(properties, property_child.head)
            if prev_property_child == property_child
                prev_property_child = ctx.property_children = property_child.tail
            else
                prev_property_child.tail = property_child.tail
            end
        else
            prev_property_child = property_child
        end

        property_child = property_child.tail
    end

    return (forms, properties)
end

"""
Attempt to transform a tree into an equivalent tree that can more easily be
batched.

What this does is look for patterns in which a long vector form is accompanied
by a large vector property that has a relatively small number of unique values.
If there are n unique values, we can split it into n contexts, each with a
shorter vector form and only scalar properties.
"""
function optimize_batching(ctx::Context)
    # condition 1: has a 1 or more long vector forms
    max_form_length = 0
    form_child = ctx.form_children
    while !isa(form_child, ListNull)
        max_form_length = max(max_form_length, length(form_child.head.primitives))
        form_child = form_child.tail
    end

    max_form_length < batch_length_threshold && return ctx

    # condition 2: has a 1 or more long vector properties each with a smaller
    # number of unique values
    max_count = div(max_form_length, batch_length_threshold) + 1
    max_unique_primitives = 0
    prop_child = ctx.property_children
    while !isa(prop_child, ListNull)
        if length(prop_child.head.primitives) > 1
            max_unique_primitives = max(max_unique_primitives,
                    count_unique_primitives(prop_child.head, max_count))
        end
        prop_child = prop_child.tail
    end

    # don't batch when there are not many forms per unique property primitive
    if max_unique_primitives == 0 ||
        div(max_form_length, max_unique_primitives) + 1 < batch_length_threshold
        return ctx
    end

    # non-destructive since this happens at draw time and draw should not modify
    # the context.
    ctx = copy(ctx)

    # step 1: remove vector form and vector properties
    forms, properties = excise_vector_children!(ctx)

    # step 2: split primitives into groups on the cross product of property
    # primives
    n = length(forms[1].primitives)
    grouped_forms = Dict{UInt, Vector{Form}}()
    grouped_properties = Dict{UInt, Vector{Property}}()
    for i in 1:n
        h = UInt(0)
        for property in properties
            h = hash(property.primitives[i], h)
        end

        if !haskey(grouped_forms, h)
            grouped_forms[h] = Form[similar(form) for form in forms]
            group_prop = Array{Property}(undef, length(properties))
            for j in 1:length(properties)
                group_prop[j] = Property([properties[j].primitives[i]])
            end
            grouped_properties[h] = group_prop
        end

        for j in 1:length(forms)
            push!(grouped_forms[h][j].primitives, forms[j].primitives[i])
        end
    end

    # step 3: put forms in new contexts and insert into the ctx
    for (h, fs) in grouped_forms
        subctx = context()
        compose!(subctx, fs...)
        compose!(subctx, grouped_properties[h]...)
        compose!(ctx, subctx)
    end

    return ctx
end
