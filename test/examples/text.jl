# Draw some text to a PNG image.

using Compose

lines = "hello and goodbye\nFoo<sub>Sub</sub>Bar<sup>Sup</sup>\nA Third Line\nFoo<sub>Sub</sub>Bar<sup>Sup</sup>Pooh\nA Fifth Line\nA Sixth Line"
c = compose(context(),
            (context(), text(0px, 250px, lines), fontsize(8pt), fill("tomato")),
            (context(), text(100px, 250px, lines), fontsize(24pt), fill("bisque")))
draw(SVG("text-fontfallback.svg", 400px, 400px), c)

import Cairo, Fontconfig

draw(SVG("text-pango.svg", 400px, 400px), c)
draw(PNG("text.png", 400px, 400px, dpi=192), c)
draw(PDF("text.pdf", 400px, 400px), c)
