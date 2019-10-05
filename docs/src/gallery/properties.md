```@meta
Author = ["Mattriks"]
```


# [Properties](@id properties_gallery)


## [`arrow`](@ref)
 
```@example
using Compose
set_default_graphic_size(14cm,5cm)
θ, r = 2π*rand(3),  0.1.+0.08*rand(3)
c, s = r.*cos.(θ), r.*sin.(θ)
point_array = [[(0.5,0.5), 0.5.+(x,y)] for (x,y) in zip(c,s) ]
img = compose(context(), arrow(), stroke("black"), fill(nothing),
        (context(), arc(0.18, 0.5, 0.08, -π/4, 1π)),
        (context(), line(point_array), stroke(["red","green","deepskyblue"])),
        (context(), curve((0.7,0.5), (0.8,-0.5), (0.8,1.5), (0.9,0.5)))
)
```

## [`clip`](@ref)

```@example
using Colors, Compose
set_default_graphic_size(14cm,7cm)
X = rand(10,2)
colv = HSVA.(range(0,stop=180,length=10), 1, 1, 0.5)
img = compose(context(units=UnitBox(0,0,2,1)), stroke("black"),
    (context(), rectangle(), fill(nothing)),
  (context(), ngon([0.5,1.5],[0.5], [0.4], [10]), fill(nothing), stroke("lightgray")),
    (context(), xgon(X[:,1],X[:,2],[0.2],[4]), fill(colv)),
    (context(), xgon(X[:,1].+1,X[:,2],[0.2],[4]), fill(colv),
        clip(points(ngon(1.5,0.5, 0.4, 10))) )
)
```



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


