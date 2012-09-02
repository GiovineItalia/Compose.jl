
# An SVG backend for compose.

require("backend.jl")
require("measure.jl")
require("color.jl")
require("form.jl")


# The SVG image we generate are going to use millimeters everywhere.
type SVGMeasure <: NativeMeasure
    value::Float64
end

*(u::Float64, v::SVGMeasure) = SVGMeasure(u * v.value)
+(u::SVGMeasure, v::SVGMeasure) = SVGMeasure(u.value + v.value)
-(u::SVGMeasure, v::SVGMeasure) = SVGMeasure(u.value - v.value)
convert(::Type{Float64}, u::SVGMeasure) = u.value
convert(::Type{SVGMeasure}, u::Float64) = SVGMeasure(u)


type SVG <: Backend
    width::SVGMeasure
    height::SVGMeasure
    f::IOStream
    close_stream::Bool
    indentation::Int
    scripts::Dict{String, String}

    function SVG(f::IOStream,
                 width::MeasureOrNumber,
                 height::MeasureOrNumber)
        img = new()
        img.width  = native_measure(width,  img)
        img.height = native_measure(height, img)
        img.f = f
        img.close_stream = false
        img.indentation = 0
        img.scripts = Dict{String, String}()

        write(img.f, @sprintf(
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.1\" width=\"%smm\" height=\"%smm\" viewBox=\"0 0 %s %s\" style=\"stroke:black;fill:black\" stroke-width=\"0.5\">\n",
              fmt_float(img.width.value), fmt_float(img.height.value),
              fmt_float(img.width.value), fmt_float(img.height.value)))

        img
    end

    function SVG(filename::String,
                 width::MeasureOrNumber,
                 height::MeasureOrNumber)
        f = open(filename, "w")
        img = SVG(f, width, height)
        img.close_stream = true
        img
    end
end


function finish(img::SVG)
    if length(img.scripts) > 0
        write(img.f, "<script type=\"application/ecmascript\"><![CDATA[\n")
        for (fn_name, js) in img.scripts
            @printf(img.f, "function %s(evt) {\n%s\n}\n\n", fn_name, js)
        end
        write(img.f, "]]></script>")
    end

    write(img.f, "</svg>\n")
    if img.close_stream
        close(img.f)
    end
end


# We are going to make to do compound units, or at least, PX + MM

function root_box(img::SVG)
    NativeBoundingBox(
        SVGMeasure(0.0),
        SVGMeasure(0.0),
        img.width,
        img.height)
end


native_zero(::SVG) = SVGMeasure(0.0)


# Float64 -> String, trimming trailing zeros when appropriate.
# This is largely taken from cairo's function _cairo_dtostr.
function fmt_float(x::Float64)
    if x < 0.1
        a = @sprintf("%0.18f", x)
    else
        a = @sprintf("%f", x)
    end

    n = length(a)
    while a[n] == '0'
        n -= 1
    end

    if a[n] == '.'
        n -= 1
    end

    a[1:n]
end


function indent(img::SVG)
    for i in 1:img.indentation
        write(img.f, "  ")
    end
end


function native_measure(u::SimpleMeasure{PixelUnit},
                        img::SVG)
    native_measure(convert(SimpleMeasure{MillimeterUnit}, u), img)
end


function native_measure(u::SimpleMeasure{MillimeterUnit},
                        img::SVG)
    SVGMeasure(u.value)
end


# Draw


function draw(img::SVG, form::LinesForm)
    n = length(form.points)
    if n <= 1; return; end

    indent(img)
    write(img.f, "<path d=\"")
    @printf(img.f, "M%s,%s L",
        fmt_float(form.points[1].x.value),
        fmt_float(form.points[1].y.value))
    for point in form.points[2:]
        @printf(img.f, " %s %s",
            fmt_float(point.x.value),
            fmt_float(point.y.value))
    end
    write(img.f, "\" />\n")
end


function draw(img::SVG, form::PolygonForm)
    n = length(form.points)
    if n <= 1; return; end

    indent(img)
    write(img.f, "<path d=\"")
    @printf(img.f, "M %s %s L",
        fmt_float(form.points[1].x.value),
        fmt_float(form.points[1].y.value))
    for point in form.points[2:]
        @printf(img.f, " %s %s",
            fmt_float(point.x.value),
            fmt_float(point.y.value))
    end
    write(img.f, " z\" />\n")
end


minmax(a, b) = a < b ? (a,b) : (b,a)


function draw(img::SVG, form::EllipseForm)
    cx = form.center.x.value
    cy = form.center.y.value
    rx = sqrt((form.x_point.x.value - cx)^2 +
              (form.x_point.y.value - cy)^2)
    ry = sqrt((form.y_point.x.value - cx)^2 +
              (form.y_point.y.value - cy)^2)
    theta = radians2degrees(atan2(form.x_point.y.value - cy,
                                  form.x_point.x.value - cx))

    indent(img)
    eps = 1e-6

    if abs(rx - ry) < eps
        @printf(img.f, "<circle cx=\"%s\" cy=\"%s\" r=\"%s\" />\n",
                fmt_float(cx), fmt_float(cy), fmt_float(rx))
    else
        @printf(img.f, "<ellipse cx=\"%s\" cy=\"%s\" rx=\"%s\" ry=\"%s\"",
                fmt_float(cx), fmt_float(cy),
                fmt_float(rx), fmt_float(ry))
        
        if abs(theta) >= eps
            @printf(img.f, " transform=\"rotate(%s %s %s)\"",
                    fmt_float(theta),
                    fmt_float(cx),
                    fmt_float(cy))
        end

        write(img.f, " />\n")
    end
end


# Applying properties


function push_property(img::SVG, p::Property)
    indent(img)
    write(img.f, "<g")
    for specific in p.specifics
        apply_property(img, specific)
    end
    write(img.f, ">\n")
    img.indentation += 1
end


function pop_property(img::SVG)
    img.indentation -= 1
    indent(img)
    write(img.f, "</g>\n")
end


# Nop catchall
function apply_property(img::SVG, p::PropertyType)
end


function apply_property(img::SVG, p::Stroke)
    @printf(img.f, " stroke=\"%s\"", cssfmt(p.value))
end


function apply_property(img::SVG, p::Fill)
    @printf(img.f, " fill=\"%s\"", cssfmt(p.value))
end


function apply_property(img::SVG, p::LineWidth)
    @printf(img.f, " stroke-width=\"%s\"", fmt_float(p.value.value))
end


function apply_property(img::SVG, p::ID)
    @printf(img.f, " id=\"%s\"", escape_string(p.value))
end


function apply_property(img::SVG, p::OnClick)
    fn_name = add_script(img, p.value)
    @printf(img.f, " onclick=\"%s(evt)\"", fn_name)
end


# Add some javascript. Return the unique name generated for the wrapper function
# containing the given code.
function add_script(img::SVG, js::String)
    n = length(img.scripts)
    fn_name = @sprintf("js_chunk_%04d", n + 1)
    img.scripts[fn_name] = js
    fn_name
end

