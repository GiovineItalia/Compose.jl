
# An experimental d3 backend for compose.

export D3


# In d3 svg we use millimeters as the base measure.
type D3Measure <: NativeMeasure
    value::Float64
end

*(u::Float64, v::D3Measure) = D3Measure(u * v.value)
+(u::D3Measure, v::D3Measure) = D3Measure(u.value + v.value)
-(u::D3Measure, v::D3Measure) = D3Measure(u.value - v.value)
convert(::Type{Float64}, u::D3Measure) = u.value
convert(::Type{D3Measure}, u::Float64) = D3Measure(u)
function convert(::Type{SimpleMeasure{MillimeterUnit}}, u::D3Measure)
    SimpleMeasure{MillimeterUnit}(u.value)
end


function show(io::IO, u::D3Measure)
    @printf(io, "%smm", svg_fmt_float(u.value))
end


type D3 <: Backend
    # Image size in millimeters
    width::D3Measure
    height::D3Measure

    # Output stream
    out::IO

    # Should f be closed when the backend is finished?
    close_stream::Bool

    # Current level of indentation.
    indentation::Int

    # Code to be inserted after the end of a property group.
    hooks::Vector{String}

    # Keep track of which properties that are push are empty to we can avoid
    # printiing them.
    empty_properties::Vector{Bool}

    # Form count used to generate unique classes for DataForms.
    dataform_count::Int

    # A counter to assign unique ids to clip paths.
    clippath_count::Int

    # Store datasets for serialization.
    data::Dict{Uint64, (Any, Int)}


    function D3(f::IO,
                width::MeasureOrNumber,
                height::MeasureOrNumber)
        img = new()
        img.width  = native_measure(width, img)
        img.height = native_measure(height, img)
        img.out = f
        img.close_stream = false
        img.indentation = 0
        img.hooks = Array(String, 0)
        img.empty_properties = Array(Bool, 0)
        img.dataform_count = 0
        img.clippath_count = 0
        img.data = Dict{Uint64, (Any, Int)}()

        width_value = svg_fmt_float(width.value)
        height_value = svg_fmt_float(height.value)
        write(img.out,
              """
              function draw(data) {
                var g = d3.select("#chart")
                          .append("svg")
                            .attr("width", "$(width_value)mm")
                            .attr("height", "$(height_value)mm")
                            .attr("viewBox", "0 0 $(width_value) $(height_value)")
                            .attr("stroke-width", "0.5")
                            .attr("style", "stroke:black;fill:black");
                g.append("defs");
                var t = {"x": 0.0,
                         "y": 0.0};
              """)
        img
    end

    function D3(filename::String,
                width::MeasureOrNumber,
                height::MeasureOrNumber)
        f = open(filename, "w")
        img = D3(f, width, height)
        img.close_stream = true
        img
    end
end


to_json(c::ColorValue) = repr("#$(hex(c))")


function write_data(img::D3, d::AbstractArray)
    write(img.out, "  [")
    n = length(d)
    for (i, x) in enumerate(d)
        write(img.out, to_json(x))
        if i < n
            write(img.out, ",")
        end
    end
    write(img.out, "]")
end


function write_data(img::D3)
    write(img.out, "var data = [\n")
    datapairs = Array(Tuple, length(img.data))
    for (i, (_, (d, j))) in enumerate(img.data)
        datapairs[i] = (j, d)
    end
    sortby!(datapairs, jd -> jd[1])

    for (i, (_, d)) in enumerate(datapairs)
        write_data(img, d)
        if i < length(img.data)
            write(img.out, ",\n")
        end
    end
    write(img.out, "];\n\n")
end


function finish(img::D3)
    write(img.out, "}\n\n")
    write_data(img)
    write(img.out, "draw(data);")
    if img.close_stream
        close(img.out)
    end
end


# Generate a unique class for a data form
function next_dataform_class(backend::D3)
    class = @sprintf("form%d", backend.dataform_count)
    backend.dataform_count += 1
    class
end


# Index of the the given array in the serialized data.
function data_idx(backend::D3, d::AbstractArray)
    id = object_id(d)
    if !has(backend.data, d)
        backend.data[id] = (d, length(backend.data))
    end
    backend.data[id][2]
end


# Conversion to D3 units (i.e. millimeters)
function native_measure(u::SimpleMeasure{PixelUnit},
                        img::D3)
    native_measure(convert(SimpleMeasure{MillimeterUnit}, u), img)
end


function native_measure(u::SimpleMeasure{MillimeterUnit},
                        img::D3)
    D3Measure(u.value)
end


function root_box(img::D3)
    NativeBoundingBox(
        D3Measure(0.0),
        D3Measure(0.0),
        img.width,
        img.height)
end


native_zero(::D3) = D3Measure(0.0)


function indent(img::D3)
    for i in 1:img.indentation
        write(img.out, "  ")
    end
end


# Nop catchall
function draw(img::D3, form::FormPrimitive)
end


function make_svg_path(points::Vector{Point})
    path = IOBuffer()
    @printf(path, "M %s %s L",
        svg_fmt_float(points[1].x.value),
        svg_fmt_float(points[1].y.value))
    for point in points[2:]
        @printf(path, " %s %s",
            svg_fmt_float(point.x.value),
            svg_fmt_float(point.y.value))
    end
    bytestring(path)
end


function draw(img::D3, form::Polygon)
    n = length(form.points)
    if n <= 1; return; end

    indent(img)
    write(img.out, "g.append(\"svg:path\")\n")
    indent(img)
    write(img.out, "   .attr(\"d\", \"")
    write(img.out, make_svg_path(form.points))
    write(img.out, " z\");\n")
end


function draw(img::D3, form::Lines)
    n = length(form.points)
    if n <= 1; return; end

    indent(img)
    write(img.out, "g.append(\"svg:path\")\n")
    indent(img)
    write(img.out, "   .attr(\"d\", \"")
    write(img.out, make_svg_path(form.points))
    write(img.out, "\");\n")
end



function draw(img::D3, form::Text)
    indent(img)
    write(img.out, "g.append(\"svg:text\")\n")
    indent(img)
    @printf(img.out, "   .attr(\"x\", %s)\n", svg_fmt_float(form.pos.x.value))
    indent(img)
    @printf(img.out, "   .attr(\"y\", %s)\n", svg_fmt_float(form.pos.y.value))

    if is(form.halign, hcenter)
        indent(img)
        print(img.out, "   .attr(\"text-anchor\", \"middle\")\n")
    elseif is(form.halign, hright)
        indent(img)
        print(img.out, "   .attr(\"text-anchor\", \"end\")\n")
    end

    if is(form.valign, vcenter)
        indent(img)
        print(img.out, "   .style(\"dominant-baseline\", \"central\")\n")
    elseif is(form.valign, vtop)
        indent(img)
        print(img.out, "   .style(\"dominant-baseline\", \"text-before-edge\")\n")
    end

    if !isidentity(form.t)
        indent(img)
        @printf(img.out, "   .attr(\"transform\", \"rotate(%s, %s, %s)\")\n",
                svg_fmt_float(radians2degrees(atan2(form.t.M[2,1], form.t.M[1,1]))),
                svg_fmt_float(form.pos.x.value),
                svg_fmt_float(form.pos.y.value))
    end

    indent(img)
    @printf(img.out, "   .text(\"%s\");\n", pango_to_svg(form.value))
end


# Nop catchall
function push_property(img::D3, property::Property)
    p = property
    hasproperties = false;
    clip = nothing
    while !is(p, empty_property)
        if !is(typeof(p.primitive), D3Hook)
            hasproperties = true
        end
        if typeof(p.primitive) === Clip
                clip = p.primitive
        end
        p = p.next
    end

    # Nothing to do
    if property === empty_property
        push!(img.empty_properties, true)
        return
    end
    push!(img.empty_properties, false)

    indent(img)
    write(img.out, "(function (g) {\n")
    img.indentation += 1
    indent(img)

    if !is(clip, nothing)
        clipid = @sprintf("clippath%d", img.clippath_count)
        img.clippath_count += 1
        write(img.out,
            """
                d3.select("defs")
                  .append("svg:clipPath")
                    .attr("id", "$(clipid)")
                    .append("svg:path")
                      .attr("d", "$(make_svg_path(clip.points)) z");
            """)
    end

    if hasproperties
        write(img.out, "g")
    end
    p = property
    hook = ""
    while !is(p, empty_property)
        if typeof(p.primitive) === D3Hook
            hook = string(hook, "\n", p.primitive.code)
        else
            apply_property(img, p.primitive)
        end
        if !is(p.next , empty_property)
            write(img.out, "\n ")
            indent(img)
        end
        p = p.next
    end
    if hasproperties
        write(img.out, ";\n");
    end
    push!(img.hooks, hook)
end


function pop_property(img::D3)
    if pop!(img.empty_properties)
        return
    end

    hook = pop!(img.hooks)
    write(img.out, hook)
    img.indentation -= 1;
    indent(img)
    write(img.out, "}(g.append(\"g\")));\n")
end


function apply_property(img::D3, p::PropertyPrimitive)

end


function apply_property(img::D3, p::Stroke)
    @printf(img.out, ".attr(\"stroke\", \"%s\")", svg_fmt_color(p.value))
end


function apply_property(img::D3, p::Fill)
    @printf(img.out, ".attr(\"fill\", \"%s\")", svg_fmt_color(p.value))
end


function apply_property(img::D3, p::LineWidth)
    @printf(img.out, ".attr(\"stroke-width\", \"%s\")",
            svg_fmt_float(p.value.value))
end


function apply_property(img::D3, p::Font)
    @printf(img.out, ".attr(\"font-family\", \"%s\")",
            escape_string(p.family))
end


function apply_property(img::D3, p::FontSize)
    @printf(img.out, ".attr(\"font-size\", \"%s\")",
            svg_fmt_float(p.value.value))
end


function apply_property(img::D3, p::Clip)
    clipid = @sprintf("clippath%d", img.clippath_count - 1)
    @printf(img.out, ".attr(\"clip-path\", \"url(#%s)\")", clipid)
end


function apply_property(img::D3, p::SVGID)
    @printf(img.out, ".attr(\"id\", \"%s\"\)", escape_string(p.value))
end


function apply_property(img::D3, p::SVGClass)
    @printf(img.out, ".attr(\"class\", \"%s\"\)", escape_string(p.value))
end


function apply_property(img::D3, p::D3Embed)
    print(img.out, p.code)
end



