# The Immerse backend
# To the Cairo backend, this adds just one feature: keeping track of
# the rendered coordinates of tagged objects and svgclass("plotpanel")
# objects

export ImmerseBackend

mutable struct ImmerseBackend <: Backend
    cb::CAIROSURFACE
    coords::Dict{Symbol,Any}
    panelcoords::Vector
end

function ImmerseBackend(c::CairoSurface)
    cb = CAIROSURFACE(c)
    ImmerseBackend(cb, Dict{Symbol,Any}(), Any[])
end

function (img::ImmerseBackend)(x)
    draw(img, x)
end

root_box(backend::ImmerseBackend) = root_box(backend.cb)

push_property_frame(backend::ImmerseBackend, properties) =
        push_property_frame(backend.cb, properties)
pop_property_frame(backend::ImmerseBackend) = pop_property_frame(backend.cb)

function draw(backend::ImmerseBackend, root_container::Container)
    empty!(backend.coords)
    empty!(backend.panelcoords)
    drawpart(backend, root_container, IdentityTransform(), UnitBox(), root_box(backend))
    finish(backend)
end

function register_coords(backend::ImmerseBackend, box, units, transform, form::Form)
    if form.tag != empty_tag
        backend.coords[form.tag] = (box, units, transform)
    end
    nothing
end

function register_coords(backend::ImmerseBackend, box, units, transform, property::SVGClass)
    length(property.primitives) == 1 && property.primitives[1].value == "plotpanel" &&
            push!(backend.panelcoords, (box, units, transform))
    nothing
end

absolute_native_units(backend::ImmerseBackend, u::Float64) = absolute_native_units(backend.cb, u)
absolute_native_units(backend::ImmerseBackend, l::Length{:mm}) =
        absolute_native_units(backend.cb, l.value)
absolute_native_units(backend::ImmerseBackend, l::Tuple{Length{:mm},Length{:mm}}) =
        (absolute_native_units(backend.cb, l[1].value),
         absolute_native_units(backend.cb, l[2].value))

surface(backend::ImmerseBackend) = surface(backend.cb)

draw(backend::ImmerseBackend, form::Form) = draw(backend.cb, form)

finish(backend::ImmerseBackend) = finish(backend.cb)

iswithjs(::ImmerseBackend) = false
iswithousjs(::ImmerseBackend) = true
