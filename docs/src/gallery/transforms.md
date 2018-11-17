```@meta
Author = ["Mattriks"]
```


# [Transformations](@id transforms_gallery)

## [`Rotation`](@ref)
 
```@example
using Compose
set_default_graphic_size(15cm,5cm)

# This example also illustrates nested contexts
f_points = [(.1, .1), (.9, .1), (.9, .2), (.2, .2), (.2, .4),
    (.6, .4), (.6, .5), (.2, .5), (.2, .9), (.1, .9), (.1, .1)]
rect(c::String) = (context(), rectangle(), stroke(c))
circ(c::String, s::Float64=0.4) =  (context(), circle([x],[y],[0.03,s]), stroke(c))
fpoly(c::String) = (context(), polygon(f_points), fill(c))
contextC(θ::Float64) = (context(0.5,0.5,1.5,1.5, units=UnitBox(0,0,1,1),
        rotation=Rotation(θ,x,y)), fpoly("steelblue"), circ("orange"))
imgf(θ::Float64) =  compose(context(), fill(nothing), rect("black"),
        (context(0.15, 0.15, 0.7, 0.7, units=UnitBox(0,0,2,2)), rect("red"), 
        contextC(θ)) # context C in context B in context A
    ) 

x, y, θ = 0.5, 0.25, π/3
hstack(imgf(-θ), imgf(0.), imgf(θ))
```


