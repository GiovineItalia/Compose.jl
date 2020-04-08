using Compose
import Cairo, Fontconfig

set_default_graphic_size(6.6inch, 3.3inch)

# See Picasso's "dog" sketch
dog = [[(183, 268), (186, 256), (189, 244)], [(290, 244), (300, 230), (339, 245)], [(350,290), (360, 300), (355, 210)], 
[(370, 207), (380,196), (375, 193)], [(310, 220), (190, 220), (164, 205)], [(135, 194), (135, 265), (153, 275)],
[(168, 275), (170, 180), (150, 190)], [(122, 214), (142, 204), (85, 240)], [(100, 247), (125, 233), (140, 238)]]


ub = UnitBox(0,0, 500,500)
p = compose(context(), stroke("black"), fillopacity(0.1),
    (context(0,0, 0.5,1, units=ub,  rotation=Rotation(-π/8)), bezigon((180, 280), dog)),
    (context(0.5,0, 0.5,1, units=ub, rotation=Rotation(π/8)),  bezigon([(180, 280)], [dog]))
)

imgs = [SVG("bezigon.svg"), PNG("bezigon.png")]

draw.(imgs, [p])

