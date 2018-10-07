```@meta
Author = ["Mattriks"]
```


# [Properties](@id properties_gallery)

## [`fill`](@ref), [`fillopacity`](@ref)
 
```@example
using Compose
set_default_graphic_size(14cm,4cm)
img = compose(context(),
  (context(), circle(0.5, 0.5, 0.08), fillopacity(0.3), fill("orange")),
  (context(), circle([0.1, 0.26], [0.5], [0.1]), fillopacity(0.3), fill("blue")),
  (context(), circle([0.42, 0.58], [0.5], [0.1]), fillopacity(0.3), fill(["yellow","green"])),
  (context(), circle([0.74, 0.90], [0.5], [0.1]), fillopacity([0.5,0.3]), fill(["yellow","red"]) )     
)
```


