
using Compose

imgs = [SVG("transformations.svg", 7cm, 7cm),
PDF("transformations.pdf", 7cm, 7cm)]


img = compose(context(),
    (context(0,0,0.5,0.5), xgon(0.2,0.2,0.1,3,0.3),
    (context(mirror=Mirror(-π/4, 0.4,0.4)), xgon(0.2,0.2,0.1,3,0.3)) )
)

draw.(imgs, [img])

