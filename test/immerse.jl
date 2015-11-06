# These tests are designed to ensure that Immerse.jl works. If you
# need to edit these tests to make them pass, that's fine, but please
# submit the corresponding fix to Immerse.

using Compose, Base.Test
import Cairo

### The Immerse backend
srf = Cairo.CairoImageSurface(10, 10, Cairo.FORMAT_RGB24)
ctx = compose(compose(context(), rectangle(0.0w, 0.0h, 0.8w, 0.7h, :rect)), fill("tomato"))
be = Compose.ImmerseBackend(srf)
draw(be, ctx)
@test be.coords[:rect][2] == Compose.UnitBox{Float64,Float64,Float64,Float64}(0.0,0.0,1.0,1.0)

### Finding tagged objects
typealias ContainersWithChildren Union{Context,Compose.Table}
typealias Iterables Union{ContainersWithChildren, AbstractArray}

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
