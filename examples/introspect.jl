using Compose
using Colors

tomato_bisque =
           compose(context(),
                   (context(), circle(), fill("bisque")),
                   (context(), rectangle(), fill("tomato")))
img = SVG("introspect.svg", 4inch, 4(sqrt(3)/2)inch)

introspect(tomato_bisque) |> img
