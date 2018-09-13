# Forms

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

## `bitmap`

## [`circle`](@ref)

## `curve`

## `ellipse`

## `line`

## [`polygon`](@ref)

## [`rectangle`](@ref)

## [`slice`](@ref)

```@example
using Compose
set_default_graphic_size(14cm, 4cm)
colv = ["red","orange","green","blue", "purple"]
a = range(-π/4, stop=7π/4, length=6)+ 0.2*randn(6)
a[6] = a[1]

sliceobj = slice([0.5], [0.5], [0.3], a[1:5], a[2:6])
img1 = compose(context(),
    (context(), sliceobj, fill(colv)) )
img2 = compose(context(),
    (context(), sliceobj, stroke("white"), fill(colv), linewidth(1.4mm)) )
img3 = compose(context(),
    (context(), sliceobj, stroke(colv), fill("transparent"), linewidth(1.4mm)) )
hstack(img1, img2, img3)
```

## [`text`](@ref)












