
# An experimental d3 backend for compose.

export D3, JS


const d3_js = readall(joinpath(Pkg.dir("Compose"), "data", "d3.v3.min.js"))

# Include the the d3 javascript library
function prepare_display(d::Display)
    display(d, "text/html", """<script charset="utf-8">$(d3_js)</script>""")
end

try
    display("text/html", """<script charset="utf-8">$(d3_js)</script>""")
catch
end


type D3 <: Backend
    # Image size in millimeters
    width::Float64
    height::Float64

    # Output stream
    out::IO

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

    # True when finish has been called and no more drawing should occur
    finished::Bool

    # Backend is responsible for opening/closing the file
    ownedfile::Bool

    # Filename when ownedfile is true
    filename::Union(String, Nothing)

    # Emit the graphic on finish when writing to a buffer.
    emit_on_finish::Bool

    function D3(out::IO,
                width::MeasureOrNumber,
                height::MeasureOrNumber,
                emit_on_finish::Bool=true)
        width = size_measure(width)
        height = size_measure(height)

        if !isabsolute(width) || !isabsolute(height)
            Error("D3 image size must be given in absolute units.")
        end

        img = new()
        img.width  = width.abs
        img.height = height.abs
        img.out = out
        img.indentation = 0
        img.hooks = Array(String, 0)
        img.empty_properties = Array(Bool, 0)
        img.dataform_count = 0
        img.clippath_count = 0
        img.data = Dict{Uint64, (Any, Int)}()
        img.finished = false
        img.ownedfile = false
        img.filename = nothing
        img.emit_on_finish = emit_on_finish
        writeheader(img)
        img
    end

    function D3(filename::String,
                width::MeasureOrNumber,
                height::MeasureOrNumber)
        out = open(filename, "w")
        img = D3(out, width, height)
        img.ownedfile = true
        img.filename = filename
        img
    end

    function D3(width::MeasureOrNumber, height::MeasureOrNumber,
                emit_on_finish::Bool=true)
        img = D3(IOBuffer(), width, height, emit_on_finish)
        img
    end
end


typealias JS D3


json(c::ColorValue) = repr("#$(hex(c))")


function write_data(img::D3, d::AbstractArray, n)
    write(img.out, "  [")
    for (i, x) in enumerate(take(cycle(d), n))
        write(img.out, json(x))
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
    sort!(datapairs, by=jd -> jd[1])

    # cycle data shorter than the longest to replicate the
    # behavior of the other backends
    n = length(datapairs) > 1 ? maximum([length(d) for (_, d) in datapairs]) : 0

    for (i, (_, d)) in enumerate(datapairs)
        write_data(img, d, n)
        if i < length(img.data)
            write(img.out, ",\n")
        end
    end
    write(img.out, "];\n\n")
end


function finish(img::D3)
    if img.finished
        return
    end

    # make all lines invariant to scaling.
    write(img.out,
    """
        d3.select(parent_id)
          .selectAll("path")
          .each(function() {
              var sw = parseFloat(window.getComputedStyle(this).getPropertyValue("stroke-width"));
              d3.select(this)
                .attr("vector-effect", "non-scaling-stroke")
                .style("stroke-width", sw + "mm");
          });
    """)

    write(img.out, "}\n\n")
    write_data(img)
    write(img.out,
        """
        var draw = function(parent_id) {
            draw_with_data(data, parent_id);
        };
        """)

    if method_exists(flush, (typeof(img.out),))
        flush(img.out)
    end

    if img.ownedfile
        close(img.out)
    end
    img.finished = true

    if img.emit_on_finish && typeof(img.out) == IOBuffer
        display(img)
    end
end


function writeheader(img::D3)
    width_value = svg_fmt_float(img.width)
    height_value = svg_fmt_float(img.height)
    write(img.out,
          """
          function draw_with_data(data, parent_id) {
            var g = d3.select(parent_id)
                      .append("svg")
                        .attr("width", "$(width_value)mm")
                        .attr("height", "$(height_value)mm")
                        .attr("viewBox", "0 0 $(width_value) $(height_value)")
                        .attr("stroke-width", "0.5")
                        .attr("style", "stroke:black;fill:black");
            g.append("defs");
            var ctx = {
                "scale": 1.0,
                "tx": 0.0,
                "ty": 0.0
            };
          """)
end


function reset(img::D3)
    if img.ownedfile
        img.out = open(img.filename, "w")
    else
        try
            seekstart(img.out)
        catch
            error("Backend can't be reused, since the output stream is not seekable.")
        end
    end
    writeheader(img)
    img.finished = false
end


function isfinished(img::D3)
    img.finished
end


function writemime(io::IO, ::MIME"text/html", img::D3)
    divid = string("gadflyplot-", randstring(20))
    write(io,
    """
    <div id="$(divid)"></div>
    <script>
    $(takebuf_string(img.out))
    draw("#$(divid)");
    </script>
    """)
end


# Generate a unique class for a data form
function next_dataform_class(backend::D3)
    class = @sprintf("form%d", backend.dataform_count)
    backend.dataform_count += 1
    class
end


# Index of the the given array in the serialized data.
function data_idx(backend::D3, d::AbstractArray)
    id = uint64(object_id(d))
    if !haskey(backend.data, d)
        backend.data[id] = (d, length(backend.data))
    end
    backend.data[id][2]
end


function root_box(img::D3)
    AbsoluteBoundingBox(
        0.0,
        0.0,
        img.width,
        img.height)
end


function indent(img::D3)
    for i in 1:img.indentation
        write(img.out, "  ")
    end
end


# Nop catchall
function draw(img::D3, form::FormPrimitive)
end


function draw(img::D3, form::Polygon)
    n = length(form.points)
    if n <= 1; return; end

    indent(img)
    write(img.out, "g.append(\"svg:path\")\n")
    indent(img)
    write(img.out, "   .attr(\"d\", \"")
    print_svg_path(img.out, form.points, true)
    write(img.out, " z\");\n")
end


function draw(img::D3, form::Lines)
    n = length(form.points)
    if n <= 1; return; end

    indent(img)
    write(img.out, "g.append(\"svg:path\")\n")
    indent(img)
    write(img.out, "   .attr(\"d\", \"")
    print_svg_path(img.out, form.points)
    write(img.out, "\");\n")
end


function draw(img::D3, form::Ellipse)
    cx = form.center.x.abs
    cy = form.center.y.abs
    rx = sqrt((form.x_point.x.abs - cx)^2 +
              (form.x_point.y.abs - cy)^2)
    ry = sqrt((form.y_point.x.abs - cx)^2 +
              (form.y_point.y.abs - cy)^2)
    theta = radians2degrees(atan2(form.x_point.y.abs - cy,
                                  form.x_point.x.abs - cx))

    if !all(isfinite([cx, cy, rx, ry, theta]))
        return
    end

    indent(img)
    eps = 1e-6
    if abs(rx - ry) < eps
        write(img.out, "g.append(\"svg:circle\")\n")
        indent(img)
        @printf(img.out, "  .attr(\"cx\", %s).attr(\"cy\", %s).attr(\"r\", %s)\n",
                svg_fmt_float(cx), svg_fmt_float(cy), svg_fmt_float(rx))
    else
        write(img.out, "g.append(\"svg:circle\")\n")
        indent(img)
        @printf(img.out, "  .attr(\"cx\", %s).attr(\"cy\", %s).attr(\"rx\", %s).attr(\"ry\", %s)\n",
                svg_fmt_float(cx), svg_fmt_float(cy),
                svg_fmt_float(rx), svg_fmt_float(ry))
    end
    indent(img)

    if abs(theta) >= eps
        @printf(img.out, ".attr(\"transform\", \"rotate(%s %s %s)\")",
                svg_fmt_float(cx), svg_fmt_float(cy), svg_fmt_float(rx))
    end

    write(img.out, ";\n");
end


# Converting text with tango markup to d3 code. Messy business.

function pango_tag_to_tspan(out, tag::String, indent::String)
    print(out, ".append(\"tspan\")\n")
    if tag == "sup"
        # print(out, indent, "  ", ".attr(\"baseline-shift\", \"super\")\n")
        print(out, indent, "  ", ".attr(\"dy\", \"-1ex\")\n")
    elseif tag == "sub"
        # print(out, indent, "  ", ".attr(\"baseline-shift\", \"sub\")\n")
        print(out, indent, "  ", ".attr(\"dy\", \"1ex\")\n")
    elseif tag == "i"
        print(out, indent, "  ", ".attr(\"font-style\", \"italic\")\n")
    elseif tag == "b"
        print(out, indent, "  ", ".attr(\"font-weight\", \"bold\")\n")
    end
end


function pango_to_d3(out, text::String, indentnum=0)
    print(out, Base.repeat(" ", indentnum))
    print(out, ".call(function(text) {\n")

    indentnum += 2; indent = Base.repeat(" ", indentnum)
    print(out, indent, "text");
    indentnum += 4; indent = Base.repeat(" ", indentnum)

    opentagpat = r"<\s*([^>\s]*)\s*>"
    openmat = match(opentagpat, text)
    if !is(openmat, nothing)

        # find end tag
        closemat = match(Regex(string(".*(</\\s*", openmat.captures[1], "\\s*>)")), text)
        if closemat === nothing
            error("No matching tag for <$(openmat.captures[1])>")
        end

        tag = openmat.captures[1]
        i0 = openmat.offset
        i1 = i0 + length(openmat.match)
        j0 = closemat.offsets[1]
        j1 = j0 + length(closemat.captures[1])

        if i0 > 1
            print(out, ".append(\"tspan\").text(\"$(escape_string(text[1:i0-1]))\")\n")
            print(out, indent)
        end

        pango_tag_to_tspan(out, tag, indent)
        pango_to_d3(out, text[i1:j0-1], indentnum)
        # reset font-style, since that seems to be inherited
        print(out, ".append(\"tspan\").attr(\"font-style\", \"normal\")")

        if j1 < length(text)
            print(out, indent, ".append(\"tspan\").text(\"$(escape_string(text[j1:end]))\")\n")
        end
        print(out, indent, ";\n");
    else
        print(out, ".text(\"$(escape_string(text))\");\n")
    end

    indentnum -= 6; indent = Base.repeat(" ", indentnum)
    print(out, indent, "})\n");
end


function draw(img::D3, form::Text)
    indent(img)
    write(img.out, "g.append(\"svg:text\")\n")
    indent(img)
    @printf(img.out, "   .attr(\"x\", %s)\n", svg_fmt_float(form.pos.x.abs))
    indent(img)
    @printf(img.out, "   .attr(\"y\", %s)\n", svg_fmt_float(form.pos.y.abs))

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
                svg_fmt_float(form.pos.x.abs),
                svg_fmt_float(form.pos.y.abs))
    end

    indent(img)
    pango_to_d3(img.out, form.value)
    print(img.out, ";\n")
    #@printf(img.out, "   .text(\"%s\");\n",
            #escape_string(pango_to_svg(form.value)))
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
                    .attr("id", parent_id + "_$(clipid)")
                    .append("svg:path")
                      .attr("d", " """)
        print_svg_path(img.out, clip.points, true)
        write(img.out, " z\");")
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
    @printf(img.out, ".attr(\"stroke-width\", %s)",
            svg_fmt_float(p.value.abs))
end


function apply_property(img::D3, p::Font)
    @printf(img.out, ".attr(\"font-family\", \"%s\")",
            escape_string(p.family))
end


function apply_property(img::D3, p::FontSize)
    @printf(img.out, ".style(\"font-size\", \"%spx\")",
            svg_fmt_float(p.value.abs))
end


function apply_property(img::D3, p::Clip)
    clipid = @sprintf("clippath%d", img.clippath_count - 1)
    @printf(img.out, ".attr(\"clip-path\", \"url(#\" + parent_id + \"_%s)\")", clipid)
end


function apply_property(img::D3, p::Opacity)
    @printf(img.out, ".attr(\"opacity\", %0.2f)", p.value)
end


function apply_property(img::D3, p::StrokeOpacity)
    @printf(img.out, ".attr(\"stroke-opacity\", %0.2f)", p.value)
end


function apply_property(img::D3, p::SVGID)
    @printf(img.out, ".attr(\"id\", \"%s\"\)", escape_string(p.value))
end


function apply_property(img::D3, p::SVGClass)
    @printf(img.out, ".attr(\"class\", \"%s\"\)", escape_string(p.value))
end


function apply_property(img::D3, p::SVGAttribute)
    @printf(img.out, ".attr(\"%s\", \"%s\")", p.attribute, p.value)
end

function apply_property(img::D3, p::D3Embed)
    print(img.out, p.code)
end


