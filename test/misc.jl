using Colors, Base.Test

# showcompact
tomato_bisque =
           compose(context(),
                   (context(), circle(), fill(colorant"bisque")),
                   (context(), rectangle(), fill(colorant"tomato")))

io = IOBuffer()
showcompact(io, tomato_bisque)
str = takebuf_string(io)
@test str == "Context(Context(R,f),Context(C,f))"

# Tagging
function points(xa, ya)
    [(x, y) for (x, y) in zip(xa, ya)]
end

pnts = points(rand(5), rand(5))
p = polygon(pnts, :mypoints)
@test p.tag == :mypoints

r = rectangle(0, 1, 0.5, 0.8, :box)
@test r.tag == :box
r = rectangle(rand(5),rand(5),rand(5),rand(5),:manybox)
@test r.tag == :manybox

c = circle(0, 0.8, 1.2, :circle)
@test c.tag == :circle
c = circle(rand(5), rand(5), rand(5), :data)
@test c.tag == :data

elps = ellipse(0, 0.8, 1.2, 1.5, :ellipse)
@test elps.tag == :ellipse
elps = ellipse(rand(5),rand(5),rand(5),rand(5),:manyellipse)
@test elps.tag == :manyellipse

txt = text(1.5, 15, "hello", tag=:hello)
@test txt.tag == :hello
txt = text(rand(5),rand(5),map(x->randstring(5), 1:5), tag=:random)
@test txt.tag == :random

ln = line(pnts, :line)
@test ln.tag == :line

crv = curve((0,0), (1,0.5), (0.2,0.3), (0.7,-2.4), :curve)
@test crv.tag == :curve
crv = curve(pnts, pnts, pnts, pnts, :manycurve)
@test crv.tag == :manycurve

bm = bitmap("fake", rand(UInt8,10), 0, 1, 0.8, 0.7, :image)
@test bm.tag == :image

# type definitions & constructors (issue #149)
@test isa(Compose.polygon(), Compose.Polygon)
@test isa(Compose.polygon([(1,2),(3,5),(4,2)]), Compose.Polygon)

@test isa(Compose.rectangle(), Compose.Rectangle)
@test isa(Compose.rectangle(0,1,0.3,0.8), Compose.Rectangle)
@test isa(Compose.rectangle(rand(5),rand(5),rand(5),rand(5)), Compose.Rectangle)

@test isa(Compose.circle(), Compose.Circle)
@test isa(Compose.circle(3.2,1.4,0.8), Compose.Circle)
@test isa(Compose.circle(rand(5), rand(5), rand(5)), Compose.Circle)

@test isa(Compose.ellipse(), Compose.Ellipse)
@test isa(Compose.ellipse(0.5,0.5,0.3,0.2), Compose.Ellipse)
@test isa(Compose.ellipse(rand(5), rand(5), rand(5), rand(5)), Compose.Ellipse)

@test isa(Compose.text(0.5,0.4,"hello"), Compose.Text)
@test isa(Compose.text(rand(5),rand(5),["hello","there"]), Compose.Text)

@test isa(Compose.line(), Compose.Line)
@test isa(Compose.line([(1,2),(3,5),(4,2)]), Compose.Line)

# issue #172: default circle(xs, ys, rs) radius measure is context units
@test isequal(circle([0.5], [0.5], [0.1]).primitives[1].radius, 0.1cx)

# Gadfly issue 857 and 436
# make sure that newlines are respected by `text_extents`
font_family = "'PT Sans Caption','Helvetica Neue','Helvetica',sans-serif"
@test text_extents(font_family, 8pt, "test test")[1][2]*2 ==
                        text_extents(font_family, 8pt, "test\ntest")[1][2]
