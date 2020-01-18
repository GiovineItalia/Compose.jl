```@meta
Author = ["Mattriks"]
```


# [Forms](@id forms_gallery)

## [`arc`](@ref)
 
```@example
using Compose
set_default_graphic_size(8cm,4cm)
colv = ["red","orange","green","blue", "purple"]
a = range(-π/4, stop=7π/4, length=6)+ 0.2*randn(6)
a[6] = a[1]

img1 = compose(context(),
    (context(), arc([0.5], [0.5], [0.3], [4.5π/4,π/4] , [7.5π/4,3π/4], [true,false]),
        stroke("black"), fill(["red","white"])) )
img2 = compose(context(),
    (context(), arc([0.5], [0.5], [0.3], a[1:5], a[2:6]), 
        stroke(colv), fill("transparent"), linewidth(2mm)) )
hstack(img1, img2)
```

## [`bezigon`](@ref)
 
```@example
using Colors, Compose
set_default_graphic_size(14cm,10cm)

petal = [[(0.4, 0.4), (0.4, 0.2), (0.5, 0.0)],  [(0.6, 0.2), (0.6, 0.4), (0.5, 0.5)]]
petalf(θ::Float64) = (context(rotation=Rotation(θ, 0.5,0.5)),
    bezigon((0.5, 0.5), petal), fill(LCHuvA(70.,50., 360*θ/2π, 0.4)))

theta = range(π/20, 2π, step=2π/10).-π
img = compose(context(), petalf.(theta)...)
```

## [`bitmap`](@ref)
```@example
using Main: SVGJSWritable #hide
using Compose
set_default_graphic_size(14cm,4cm)
rawimg = read(joinpath(@__DIR__,"..","assets/smiley.png"));
X = 0.9*rand(10,2)

img = compose(context(), 
    (context(), rectangle(), fill("transparent"), stroke("orange")),
    (context(), bitmap(["image/png"], [rawimg], X[:,1], X[:,2], [0.1], [0.1]))
)
SVGJSWritable(ans) #hide
```

## [`circle`](@ref)

```@example
using Colors, Compose
set_default_graphic_size(14cm,4cm)
colv = HSVA.([0:30:179;], 1, 1, 0.5)
img = compose(context(units=UnitBox(0,0,40,8)),
    (context(), circle([5.0:6:35;], [4], [4]), fill(colv), stroke("black"))
)
```

## [`curve`](@ref)
```@example
using Colors, Compose
set_default_graphic_size(14cm, 4cm)
epoint(x) = [(x,y) for y in rand(10)]
cpoint(t=0) = [(t+x,y) for (x,y) in zip(rand(10), 0.5*rand(10))]
colv = range(colorant"blue",stop=colorant"orange", length=10)

img = compose(context(units=UnitBox(0,0,2,1)),
    (context(), curve([(0.5,1.0)], cpoint(), cpoint(), epoint(0.5)), stroke(colv)),
    (context(), curve([(1.5,1.0)], cpoint(1), cpoint(1), epoint(1.5)), stroke(colv)) 
)
```
## [`ellipse`](@ref)
```@example
using Colors, Compose
set_default_graphic_size(14cm, 4cm)
colv1 = HSVA.([0:30:179;], 1, 1, 0.5)
r = 2*[1:6;]/24

colv2 = HSVA.([0:15:179;], 1, 1, 0.3)
θ = collect(range(0, stop=1.9π, length=10))
rl = 0.5*rand(10)
rw = 0.3*rand(10)
rot = Rotation.(θ, 1.5, 0.5)
ellipsef(i::Int) = ellipse(1.5, 0.5, rl[i], rw[i])

img = compose(context(units=UnitBox(0,0,2,1)),
        (context(), ellipse(r,[0.5],r,reverse(r)), stroke("black"), fill(colv1)),
[(context(rotation=rot[i]), ellipsef(i), fill(colv2[i]), stroke("black")) for i in 1:10]...
)
```

## [`line`](@ref)
```@example
using Compose
set_default_graphic_size(10cm, 10cm)
θ = collect(range(0, stop=2π, length=60))
point_array = [[(0,0.75), (x,y)] for (x,y) in zip(cos.(θ), sin.(θ))]

img = compose(context(), 
    (context(), rectangle(), fill("salmon"), fillopacity(0.3)),
    (context(0.12, 0.12, 0.76, 0.76, units=UnitBox(-1,-1,2,2)),  
        line(point_array), stroke("gold"), linewidth(1mm))
)
```

## [`ngon`](@ref), [`star`](@ref), [`xgon`](@ref) 
```@example
using Compose
set_default_graphic_size(14cm, 5cm)
rainbow = ["orange","green","indigo",
    "darkviolet","indigo","blue","green","yellow","orange","red"]
properties = [fillopacity(0.5), fill(rainbow), stroke("black")]
npoints = [7,5,3,2,3,4,5,6,7,8]
X = range(0.06, stop=0.94, length=10)
radii = 0.035*[-ones(3); ones(7)]
p = compose(context(),
    (context(), ngon(X, [0.16], radii, npoints),
        star(X, [0.5], radii, npoints),
        xgon(X, [0.84], radii, npoints), properties...))
```

## [`polygon`](@ref)
```@example
using Statistics, Compose
set_default_graphic_size(10cm,10cm)
X = randn(50,2)
X = 0.3*(X .- mean(X,dims=1))./std(X,dims=1)
hp = hypot.(X[:,1],X[:,2])
i = hp .> Statistics.quantile(hp, 0.82)
Z = X[i,:]
θ = atan.(Z[:,1], Z[:,2])
ord = sortperm(θ)
polypoints = [(x,y) for (x,y) in zip(Z[ord,1],Z[ord,2])]  

img = compose(context(units=UnitBox(-1,-1, 2,2)),
  (context(), line([(-1,-1), (-1,1), (1,1)]), stroke("black")),
  (context(), circle(0,0,0.02), fill("red")),
  (context(), circle(X[:,1],X[:,2],[0.02]), fill("transparent"),stroke("deepskyblue")),
  (context(), polygon(polypoints), fill("red"), fillopacity(0.1))
)
```

## [`rectangle`](@ref)
```@example
using Colors, Compose
set_default_graphic_size(14cm,4cm)
colv = HSVA.([0:15:179;], 1, 1, 0.3)
X = 0.9*rand(10,2)
rl = 0.3*rand(10).+0.03
rw = 0.3*rand(10).+0.03

img = compose(context(), 
    (context(), rectangle(), fill("transparent"), stroke("orange")),
     (context(), rectangle(X[:,1], X[:,2], rl, rw), fill(colv), stroke("black")) 
)
```


## [`sector`](@ref)
```@example
using Compose
set_default_graphic_size(14cm, 4cm)
colv = ["red","orange","green","blue", "purple"]
a = range(-π/4, stop=7π/4, length=6)+ 0.2*randn(6)
a[6] = a[1]

sectorobj = sector([0.5], [0.5], [0.3], a[1:5], a[2:6])
img1 = compose(context(),
    (context(), sectorobj, fill(colv)) )
img2 = compose(context(),
    (context(), sectorobj, stroke("white"), fill(colv), linewidth(1.4mm)) )
img3 = compose(context(),
    (context(), sectorobj, stroke(colv), fill("transparent"), linewidth(1.4mm)) )
hstack(img1, img2, img3)
```

## [`text`](@ref)
```@example
using Colors, Compose
set_default_graphic_size(10cm,10cm)
labels=rand(string.(names(Base)[280:end]), 30)
θ = collect(range(0, stop=58π/30, length=30))
X = 1 .+ 0.7*[cos.(θ) sin.(θ)]
colv = range(colorant"blue",stop=colorant"orange", length=30)
rot = Rotation.(θ, X[:,1], X[:,2])

img = compose(context(units=UnitBox(0,0,2,2)),
  (context(), text(1, 1, "Julia", hcenter, vcenter), stroke("red"), fontsize(30pt)),
  (context(), text(X[:,1], X[:,2], labels, [hcenter], [vcenter], rot), stroke(colv))
)
```

```@example
using Compose
set_default_graphic_size(10cm,8cm)

# This graphic illustrates text alignment
txt = [x*"\n"*y for  x in ["hleft", "hcenter","hright"], 
        y in ["vtop","vcenter","vbottom"] ]
x = repeat(0.1w.*[1,5,9], outer=3)
y = repeat(0.1h.*[1,5,9], inner=3)
xp = repeat([hleft,hcenter,hright], outer=3)
yp = repeat([vtop,vcenter,vbottom], inner=3)

img = compose(context(),
        (context(), circle(x, y, [0.01]), fill("red")),
        text(x, y, txt, xp, yp), fontsize(14pt)
)
```












