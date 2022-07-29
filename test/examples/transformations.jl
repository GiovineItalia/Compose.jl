using Compose
import Cairo, Fontconfig

imgs = [SVG("transformations.svg", 7cm, 7cm),
        PDF("transformations.pdf", 7cm, 7cm)]


img = compose(context(),
    (context(0,0,0.5,0.5), xgon(0.2,0.2,0.1,3,0.3),
    (context(mirror=Mirror(-π/4, 0.4,0.4)), xgon(0.2,0.2,0.1,3,0.3)) ),
    (context(0.5,0,0.5,0.5), xgon(0.5,0.5,0.2,3,0.3), fill("silver"),
    (context(shear=Shear(0.8, 0, 0.5,0.5)), xgon(0.5,0.5,0.2,3,0.3), fill(nothing), stroke("black")) ),
    (context(0.0,0.5,0.5,0.5), rectangle(0.3,0.4,0.4,0.2), fill(nothing), stroke("silver"),
    (context(rotation=Rotation(π/6)), rectangle(0.3,0.4,0.4,0.2), stroke("black")) )
)

draw.(imgs, [img])

