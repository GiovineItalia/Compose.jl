#!/usr/bin/env julia

# Draw some text to a PNG image.

using Compose

img = PNG("text.png", 400px, 400px)
c = compose(compose(context(), text(150px, 200px, "hello &amp; goodbye")), fill("tomato"))
draw(img, c)
