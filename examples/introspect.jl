using Compose
using Colors

tomato_bisque =
           compose(context(),
                   (context(), circle(), fill(colorant"bisque")),
                   (context(), rectangle(), fill(colorant"tomato")))
img = SVG("introspect.svg", 4inch, 4(sqrt(3)/2)inch)

draw(img, introspect(tomato_bisque))
