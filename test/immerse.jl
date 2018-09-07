# These tests are designed to ensure that Immerse.jl works. If you
# need to edit these tests to make them pass, that's fine, but please
# submit the corresponding fix to Immerse.

using Test
using Compose
import Cairo
import Measures

### The Immerse backend
srf = Cairo.CairoImageSurface(10, 10, Cairo.FORMAT_RGB24)
ctx = compose(compose(context(), rectangle(0.0w, 0.0h, 0.8w, 0.7h, :rect)), fill("tomato"))
be = Compose.ImmerseBackend(srf)
draw(be, ctx)
@test be.coords[:rect][2] == Compose.UnitBox()

### Finding tagged objects
const ContainersWithChildren = Union{Context,Compose.Table}
const Iterables = Union{ContainersWithChildren, AbstractArray}

iterable(ctx::ContainersWithChildren) = ctx.children
iterable(a::AbstractArray) = a

function find_tagged(root)
    handles = Dict{Symbol,Context}()
    find_tagged!(handles, root)
end

function find_tagged!(handles, obj::Iterables)
    for item in iterable(obj)
        if has_tag(item)
            handles[item.tag] = obj
        else
            find_tagged!(handles, item)
        end
    end
    handles
end

function find_tagged!(handles, obj::Context)
    for item in obj.form_children
        if has_tag(item)
            handles[item.tag] = obj
        end
    end
    for item in obj.container_children
        find_tagged!(handles, item)
    end
    handles
end

find_tagged!(handles, obj) = obj

has_tag(form::Compose.Form, tag) = form.tag == tag
has_tag(form::Compose.Form)      = form.tag != Compose.empty_tag

has_tag(obj, tag) = false
has_tag(obj)      = false

@test find_tagged(ctx)[:rect] == ctx

### Coordinate computations
function absolute_to_data(x, y, transform, unit_box, parent_box)
    xt, yt = invert_transform(transform, x, y)
    (unit_box.x0 + unit_box.width *(xt-parent_box.x0[1])/Measures.width(parent_box),
     unit_box.y0 + unit_box.height*(yt-parent_box.x0[2])/Measures.height(parent_box))
end

invert_transform(::Compose.IdentityTransform, x, y) = x, y

function invert_transform(t::Compose.MatrixTransform, x, y)
    @assert t.M[3,1] == t.M[3,2] == 0
    xyt = t.M\[x, y, 1.0]
    xyt[1], xyt[2]
end

box, units, transform = be.coords[:rect]
xd, yd = absolute_to_data(0.5mm, 0.5mm, transform, units, box)
@test isapprox(xd,0.1417; atol=0.0001)
