using Compose, Test, Random, Colors

# must be before importing cairo
@testset "missing cairo errors" begin
    ctx = compose(context(), circle(), fill("gold"))
    @test_throws ErrorException ctx |> PNG("test.png")
    @test_throws ErrorException ctx |> PDF("test.pdf")
    @test_throws ErrorException ctx |> PS("test.ps")

    io = IOBuffer()
    @test_throws ErrorException show(io, MIME("image/png"), ctx)
    @test_throws ErrorException show(io, MIME("application/pdf"), ctx)
    @test_throws ErrorException show(io, MIME("application/ps"), ctx)
end

import Cairo

# showcompact
@testset "printing" begin
    @testset "context" begin
        io = IOBuffer()
        tomato_bisque = compose(context(),
                    (context(), circle(), fill(colorant"bisque")),
                    (context(), rectangle(), fill(colorant"tomato")))

        # compact printing
        show(IOContext(io, :compact=>true), tomato_bisque)
        str = String(take!(io))
        @test str == "Context(Context(R,f),Context(C,f))"

        # full printing
        show(io, context())
        str = String(take!(io))
        @test str == "Context(Measures.BoundingBox{Tuple{Measures.Length{:w,Float64},Measures.Length{:h,Float64}},Tuple{Measures.Length{:w,Float64},Measures.Length{:h,Float64}}}((0.0w, 0.0h), (1.0w, 1.0h)), nothing, nothing, nothing, nothing, List([]), List([]), List([]), 0, false, false, false, false, nothing, nothing, 0.0, Symbol(\"\"))"
    end
    @testset "table" begin
        t = Compose.Table(1, 1, UnitRange(1,1), UnitRange(3:3), aspect_ratio=1.6)
        io = IOBuffer()

        # compact printing
        show(IOContext(io, :compact=>true), t)
        str = String(take!(io))
        @test str == "1x1 Table:\n  Context[]\n"

        # full printing
        show(io, t)
        str = String(take!(io))
        @test str == "Compose.Table(Array{Context,1}[[]], 3:3, 1:1, nothing, nothing, 1.6, Any[], nothing, 0, false, false)"
    end
end

# Tagging
points(xa, ya) = [(x, y) for (x, y) in zip(xa, ya)]

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
oneline = text_extents(font_family, 8pt, "PegPeg")[1]
twolines = text_extents(font_family, 8pt, "Peg\nPeg")[1]
@test round(Int, oneline[1] / twolines[1]) == 2
@test round(Int, twolines[2] / oneline[2]) == 2

# PR 252
@test Compose.parse_colorant("red") == RGB(1.0,0.0,0.0)
@test Compose.parse_colorant(colorant"red") == RGB(1.0,0.0,0.0)
@test Compose.parse_colorant(["red","blue"]) == [RGB(1.0,0.0,0.0), RGB(0.0,0.0,1.0)]
@test Compose.parse_colorant(("red","blue")) == [RGB(1.0,0.0,0.0), RGB(0.0,0.0,1.0)]
@test Compose.parse_colorant("red","blue") == [RGB(1.0,0.0,0.0), RGB(0.0,0.0,1.0)]
@test Compose.parse_colorant("red",colorant"blue") == [RGB(1.0,0.0,0.0), RGB(0.0,0.0,1.0)]

# PR 263
@test ~ @isdefined Fontconfig
@test Compose.pango_to_svg("hello world") == "hello world"
import Fontconfig
@test Compose.pango_to_svg("hello world") == "hello world"

@testset "pango" begin
    @test Compose.escape_tex_chars("\\") == "\\textbackslash{}"
    @test Compose.pango_to_pgf("hello\\") == "\\text{hello\\textbackslash{}}"
end

@testset "pango utf-8 text_extents" begin
    @test @isdefined Fontconfig
    w1 = Compose.text_extents("", 8pt, "α")[1]
    w2 = Compose.text_extents("", 8pt, "αα")[1]
    @test w1 < w2
end

@testset "table" begin
    @testset "force aspect ratio" begin
        tbl = Compose.Table(1, 1, UnitRange(1,1), UnitRange(3:3), aspect_ratio=1.6);
        x_solution = [0.0, 5.8, 11.8]
        w_solution = [5.8, 6.0, 119.6]
        y_solution = [0.0, 85.3]
        h_solution = [85.3, 4.7]
        x0, w0 = copy(x_solution), copy(w_solution)
        Compose.force_aspect_ratio!(tbl, x_solution, y_solution, w_solution, h_solution)
        @test x_solution == x0
        @test w_solution == w0
        @test y_solution ≈ [5.275, 80.025]
        @test h_solution == [74.75, 4.7]
    end
end

@testset "Image keyword args" begin
    @test isa(PNG(4inch, 3inch, dpi=172), Compose.Image)
end

@testset "No Global RNG contamination" begin
    Random.seed!(23)
    withoutcompose = rand()
    Random.seed!(23)
    draw(SVG(10cm, 8cm, false), compose(context()))
    withcompose = rand()
    @test withoutcompose == withcompose
end

@testset "Image fillopacity" begin
    properties = [fill(["red","blue"]), fillopacity(0.3), stroke("black")]
    img1 = PNG(); Compose.push_property_frame(img1, properties)
    img2 = SVG(); Compose.push_property_frame(img2, properties)
    img3 = PGF(); Compose.push_property_frame(img3, properties)
    @test getfield.(img1.vector_properties[Compose.Property{Compose.FillOpacityPrimitive}].primitives, :value) == [0.3, 0.3]
    @test occursin("fill-opacity=\"0.3\"", String(img2.out.data))
    @test img3.fill_opacity == 0.3
end

@testset "Missing" begin
    @test Compose.x_measure(missing)===NaN*cx
    @test Compose.y_measure(missing)===NaN*cy
    @test Compose.size_x_measure(missing)===NaN*sx
    @test Compose.size_y_measure(missing)===NaN*sy
end

@testset "cx, sx and cy, sy units" begin
    ub, bb = UnitBox(-0.3, 0., 1.2, 1.), Compose.BoundingBox((50mm, 50mm), (40mm, 40mm))
    tr = Compose.IdentityTransform()
    xpos = (Compose.resolve_position(bb, ub, tr, 0sx), 
                Compose.resolve_position(bb, ub, tr, 0cx))
    ub, bb = UnitBox(0., -0.3, 1., 1.2), Compose.BoundingBox((10mm, 10mm), (40mm, 40mm))
    ypos = (Compose.resolve_position(bb, ub, tr, 0sy),
                Compose.resolve_position(bb, ub, tr, 0cy))
    @test ypos == xpos == (0.0mm, 10.0mm)
end
