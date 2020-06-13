using Compose
import Cairo, Fontconfig

set_default_graphic_size(14cm, 10cm)


y = [0.26, 0.5, missing, 0.4, NaN, 0.48, 0.58, 0.83]
p1 = collect(Tuple, zip(1:8, y))
p2 = collect(Tuple, zip(1:9, vcat(NaN, y)))

img = compose(context(units=UnitBox(0, 0, 10, 1)),
    (context(), line([p1]), stroke("black")),
    (context(), line([p2]), stroke("red"))
)

imgs = [SVG("forms_and_nans.svg"), PDF("forms_and_nans.pdf")]

draw.(imgs, [img])




