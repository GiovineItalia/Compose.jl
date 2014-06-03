
#const snapsvgjs = readall(Pkg.dir("Compose", "data", "snap.svg-min.js"))
const snapsvgjs = readall(Pkg.dir("Compose", "data", "snap.svg.js"))

# Packages can insert extra XML namespaces here to be defined in the output
# SVG.
const xmlns = Dict()


# Format a floating point number into a decimal string of reasonable precision.
function svg_fmt_float(x::Float64)
    # All svg (in our use) coordinates are in millimeters. This number gives the
    # largest deviation from the true position allowed in millimeters.
    const eps = 0.01
    a = @sprintf("%0.8f", round(x / eps) * eps)
    n = length(a)

    while a[n] == '0'
        n -= 1
    end

    if a[n] == '.'
        n -= 1
    end

    a[1:n]
end

# Format a color for SVG.
svg_fmt_color(c::ColorValue) = @sprintf("#%s", hex(c))
svg_fmt_color(c::Nothing) = "none"


# Javascript in a <script> tag in SVG needs to escape '"' and '<'.
#=function escape_script(js::String)=#
    #=return replace(replace(js, "&", "&amp;"), "<", "&lt;")=#
#=end=#

# Javascript contained to CDATA block needs to avoid ']]'
function escape_script(js::String)
    return replace(js, "]]", "] ]")
end


# When subtree rooted at a context is drawn, it pushes its property children
# in the form of a property frame.
type SVGPropertyFrame
    # Vector properties in this frame.
    vector_properties::Dict{Type, Property}

    # True if this property frame has scalar properties. Scalar properties are
    # emitted as a group (<g> tag) that must be closed when the frame is popped.
    has_scalar_properties::Bool

    # True if this property frame includes a link (<a> tag) that needs
    # to be closed.
    has_link::Bool

    # True if the property frame include a mask (<mask> tag) than needs to be
    # closed.
    has_mask::Bool

    function SVGPropertyFrame()
        return new(Dict{Type, Property}(), false, false, false)
    end
end


type SVG <: Backend
    # Image size in millimeters.
    width::Float64
    height::Float64

    # Output stream.
    out::IO

    # Current level of indentation.
    indentation::Int

    # Stack of property frames (groups of properties) currently in effect.
    property_stack::Vector{SVGPropertyFrame}

    # SVG forbids defining the same property twice, so we have to keep track
    # of which vector property of which type is in effect. If two properties of
    # the same type are in effect, the one higher on the stack takes precedence.
    vector_properties::Dict{Type, Union(Nothing, Property)}

    # Clip-paths that need to be defined at the end of the document.
    clippaths::Dict{ClipPrimitive, Int}

    # Embedded objects included immediately before the </svg> tag, such as extra
    # javascript or css.
    embobj::Set{String}

    # True when finish has been called and no more drawing should occur
    finished::Bool

    # Backend is responsible for opening/closing the file
    ownedfile::Bool

    # Filename when ownedfile is true
    filename::Union(String, Nothing)

    # Emit the graphic on finish when writing to a buffer.
    emit_on_finish::Bool

    # IDs of the SVG element currently being generated, or nothing if it has
    # none.
    current_id::Union(String, Nothing)

    # A counter used to generate unique IDs
    id_count::Int

    # Javsacript to include before any JSCall code.
    jsheader::Vector{String}

    # User javascript from JSCall attributes
    scripts::Vector{String}

    # Use javascript extensions to add interactivity, etc.
    withjs::Bool

    function SVG(out::IO,
                 width,
                 height,
                 emit_on_finish::Bool=true,
                 withjs::Bool=false)
        width = size_measure(width)
        height = size_measure(height)
        if !isabsolute(width) || !isabsolute(height)
            error("SVG image size must be specified in absolute units.")
        end

        img = new()
        img.width  = width.abs
        img.height = height.abs
        img.out = out
        img.indentation = 0
        img.property_stack = Array(SVGPropertyFrame, 0)
        img.vector_properties = Dict{Type, Union(Nothing, Property)}()
        img.clippaths = Dict{ClipPrimitive, Int}()
        img.embobj = Set{String}()
        img.finished = false
        img.emit_on_finish = emit_on_finish
        img.current_id = nothing
        img.id_count = 0
        img.jsheader = String[]
        img.scripts = String[]
        img.withjs = withjs
        img.ownedfile = false
        img.filename = nothing
        writeheader(img)
        return img
    end

    # Write to a file.
    function SVG(filename::String, width, height, withjs::Bool=false)
        out = open(filename, "w")
        img = SVG(out, width, height, true, withjs)
        img.ownedfile = true
        img.filename = filename
        return img
    end

    # Write to buffer.
    function SVG(width::MeasureOrNumber, height::MeasureOrNumber,
                 emit_on_finish::Bool=true, withjs::Bool=false)
        return SVG(IOBuffer(), width, height, emit_on_finish, withjs)
    end
end


function iswithjs(img::SVG)
    return img.withjs
end


# Return the next unique element ID. Sort of like gensym for SVG elements.
function genid(img::SVG)
    img.id_count += 1
    return @sprintf("id%d", img.id_count)
end


# Constructors that turn javascript extensions on
function SVGJS(out::IO, width, height, emit_on_finish::Bool=true)
    return SVG(out, width, height, emit_on_finish, true)
end


function SVGJS(filename::String, width, height)
    return SVG(filename, width, height, true)
end


function SVGJS(width::MeasureOrNumber, height::MeasureOrNumber,
               emit_on_finish::Bool=true)
    return SVG(width, height, emit_on_finish, true)
end


function writeheader(img::SVG)
    widthstr = svg_fmt_float(img.width)
    heightstr = svg_fmt_float(img.height)
    write(img.out,
          """
          <?xml version="1.0" encoding="UTF-8"?>
          <svg xmlns="http://www.w3.org/2000/svg"
               xmlns:xlink="http://www.w3.org/1999/xlink"
          """)

    for (ns, uri) in xmlns
        write(img.out, """     xmlns:$(ns)="$(uri)"\n""")
    end

    write(img.out,
          """
               version="1.1"
               width="$(widthstr)mm" height="$(heightstr)mm" viewBox="0 0 $(widthstr) $(heightstr)"
               stroke="$(svg_fmt_color(default_stroke_color))"
               fill="$(svg_fmt_color(default_fill_color))"
               stroke-width="$(svg_fmt_float(default_line_width.abs))"
          """)
    if img.withjs
        write(img.out, "\n     id=\"composegraphic\"")
    end
    write(img.out, ">\n")
    return img
end


function reset(img::SVG)
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


function finish(img::SVG)
    if img.finished
        return
    end

    while !isempty(img.property_stack)
        pop_property_frame(img)
    end

    for obj in img.embobj
        write(img.out, obj)
        write(img.out, "\n")
    end

    if length(img.clippaths) > 0
        write(img.out, "<defs>\n")
        for (clippath, i) in img.clippaths
            write(img.out, "<clipPath id=\"clippath$(i)\">\n  <path d=\"")
            print_svg_path(img.out, clippath.points)
            write(img.out, "\" />\n</clipPath\n>")
        end
        write(img.out, "</defs>\n")
    end

    if img.withjs
        # Prevent require.js from loading snap.svg, since we want to do so by
        # including it inline in a <script> tag, which doesn't really work with
        # require.js.
        write(img.out,
        """
        <script>
        if (typeof define != "undefined") {
            window.define_ = define;
            define = undefined;
        }
        </script>
        """)

        write(img.out,
            """
            <script> <![CDATA[
            $(escape_script(snapsvgjs))
            ]]> </script>
            """)

        # Restore the require.js define function.
        write(img.out,
            """
            <script>
            if (typeof define_ != "undefined") {
                define = define_;
            }
            </script>
            """)

        if !isempty(img.scripts) || !isempty(img.jsheader)
            write(img.out, "<script> <![CDATA[\n")
            for script in img.jsheader
                write(img.out, escape_script(script), "\n")
            end
            write(img.out, """var fig = Snap("#composegraphic");""")
            for script in img.scripts
                write(img.out, escape_script(script), "\n")
            end
            write(img.out, "]]> </script>\n")
        end

    end

    write(img.out, "</svg>\n")
    if method_exists(flush, (typeof(img.out),))
        flush(img.out)
    end

    if img.ownedfile
        close(img.out)
    end

    img.finished = true

    # If we are writing to a buffer. Collect the string and emit it.
    if img.emit_on_finish && typeof(img.out) == IOBuffer
        display(img)
    end
end


function isfinished(img::SVG)
    img.finished
end


function writemime(io::IO, ::MIME"image/svg+xml", img::SVG)
    write(io, takebuf_string(img.out))
end


function root_box(img::SVG)
    AbsoluteBoundingBox(0.0, 0.0, img.width, img.height)
end


function indent(img::SVG)
    for i in 1:img.indentation
        write(img.out, "  ")
    end
end


# Draw

# Generate SVG path data from an array of points.
#
# Args:
#   out: Output stream.
#   points: points on the path
#   bridge_gaps: when true, remove non-finite values, rather than forming
#                separate lines.
#
# Returns:
#   A string containing SVG path data.
#
function print_svg_path(out, points::Vector{Point}, bridge_gaps::Bool=false)
    isfirst = true
    for point in points
        x, y = point.x.abs, point.y.abs
        if !(isfinite(x) && isfinite(y))
            isfirst = true
            continue
        end

        if isfirst
            isfirst = false
            @printf(out, "M%s,%s L",
                    svg_fmt_float(x),
                    svg_fmt_float(y))
        else
            @printf(out, " %s %s",
                    svg_fmt_float(x),
                    svg_fmt_float(y))
        end
    end
end


# Return array of paths to draw with printpath
# array is formed by splitting by NaN values
function make_paths(points::Vector{Point})
    paths = {}
    nans = find(xy -> isnan(xy[1]) || isnan(xy[2]),
                [(point.x.abs, point.y.abs) for point in points])

    if length(nans) == 0
        push!(paths, points)
    else
        nans = [0, nans, length(points) + 1]
        i, n = 1, length(nans)
        while i <= n-1
            if nans[i] + 1 < nans[i + 1]
                push!(paths, points[(nans[i]+1):(nans[i+1] - 1)])
            end
            i += 1
        end
    end
    paths
end


# Property Printing
# -----------------


function print_property(img::SVG, property::StrokePrimitive)
    @printf(img.out, " stroke=\"%s\"", svg_fmt_color(property.color))
end


function print_property(img::SVG, property::FillPrimitive)
    @printf(img.out, " fill=\"%s\"", svg_fmt_color(property.color))
end


function print_poperty(img::SVG, property::StrokeDashPrimitive)
    @printf(img.out, " stroke-dasharray=\"%s\"",
            join(map(v -> svg_fmt_float(v.abs), property.value), ","))
end


# Format a line-cap specifier into the attribute string that SVG expects.
svg_fmt_linecap(::LineCapButt) = "butt"
svg_fmt_linecap(::LineCapSquare) = "square"
svg_fmt_linecap(::LineCapRound) = "round"


function print_property(img::SVG, property::StrokeLineCapPrimitive)
    @printf(img.out, " stroke-linecap=\"%s\"", svg_fmt_linecap(property.value))
end


# Format a line-join specifier into the attribute string that SVG expects.
svg_fmt_linejoin(::LineJoinMiter) = "miter"
svg_fmt_linejoin(::LineJoinRound) = "round"
svg_fmt_linejoin(::LineJoinBevel) = "bevel"


function print_property(img::SVG, property::StrokeLineJoinPrimitive)
    @printf(img.out, " stroke-linejoin=\"%s\"", svg_fmt_linejoin(property.value))
end


function print_property(img::SVG, property::LineWidthPrimitive)
    @printf(img.out, " stroke-width=\"%s\"", svg_fmt_float(property.value.abs))
end


function print_property(img::SVG, property::FillOpacityPrimitive)
    @printf(img.out, " opacity=\"%s\"", svg_fmt_float(property.value))
end


function print_property(img::SVG, property::StrokeOpacityPrimitive)
    @printf(img.out, " stroke-opacity=\"%s\"", svg_fmt_float(property.value))
end


function print_property(img::SVG, property::VisiblePrimitive)
    @printf(img.out, " visibility=\"%s\"",
            property.value ? "visible" : "hidden")
end


# I may end up applying the same clip path to many forms separately, so I
# shouldn't make a new one for each applicaiton. Where should that happen?
function print_property(img::SVG, property::ClipPrimitive)
    url = clippathurl(img, property)
    @printf(img.out, " clip-path=\"url(#%s)\"", url)
end


function print_property(img::SVG, property::FontPrimitive)
    @printf(img.out, " font-family=\"%s\"", escape_string(property.family))
end


function print_property(img::SVG, property::FontSizePrimitive)
    @printf(img.out, " font-size=\"%s\"", svg_fmt_float(property.value.abs))
end


function print_property(img::SVG, property::SVGIDPrimitive)
    @printf(img.out, " id=\"%s\"", escape_string(property.value))
end


function print_property(img::SVG, property::SVGClassPrimitive)
    @printf(img.out, " class=\"%s\"", escape_string(property.value))
end


function print_property(img::SVG, property::SVGAttributePrimitive)
    @printf(img.out, " %s=\"%s\"",
            property.attribute, escape_string(property.value))
end


function print_property(img::SVG, property::JSIncludePrimitive)
    push!(img.jsheader, property.value)
end


function print_property(img::SVG, property::JSCallPrimitive)
    push!(img.scripts,
          @sprintf("fig.select(\"#%s\")\n   .%s;",
                   img.current_id, property.code))
end


# Print the property at the given index in each vector property
function print_vector_properties(img::SVG, idx::Int)
    if haskey(img.vector_properties, JSCall)
        if haskey(img.vector_properties, SVGID)
            img.current_id = img.vector_properties[SVGID].primitives[idx].value
        else
            img.current_id = genid(img)
            print_property(img, SVGIDPrimitive(img.current_id))
        end
    end

    for (propertytype, property) in img.vector_properties
        if property === nothing
            continue
        end
        if idx > length(property.primitives)
            error("Vector form and vector property differ in length. Can't distribute.")
        end
        print_property(img, property.primitives[idx])
    end

    img.current_id = nothing
end


# Form Drawing
# ------------


function draw(img::SVG, form::Form)
    for (idx, primitive) in enumerate(form.primitives)
        draw(img, primitive, idx)
    end
end


function draw(img::SVG, prim::RectanglePrimitive, idx::Int)
    indent(img)
    @printf(img.out, "<rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\"",
            svg_fmt_float(prim.corner.x.abs),
            svg_fmt_float(prim.corner.y.abs),
            svg_fmt_float(prim.width.abs),
            svg_fmt_float(prim.height.abs))
    print_vector_properties(img, idx)
    write(img.out, "/>\n")
end


function draw(img::SVG, prim::PolygonPrimitive, idx::Int)
     n = length(prim.points)
     if n <= 1; return; end

     indent(img)
     write(img.out, "<path d=\"")
     print_svg_path(img.out, prim.points, true)
     write(img.out, " z\"")
     print_vector_properties(img, idx)
     write(img.out, "/>\n")
end


function draw(img::SVG, prim::CirclePrimitive, idx::Int)
    indent(img)
    @printf(img.out, "<circle cx=\"%s\" cy=\"%s\" r=\"%s\"",
            svg_fmt_float(prim.center.x.abs),
            svg_fmt_float(prim.center.y.abs),
            svg_fmt_float(prim.radius.abs))
    print_vector_properties(img, idx)
    write(img.out, "/>\n")
end


function draw(img::SVG, prim::EllipsePrimitive, idx::Int)
    cx = prim.center.x.abs
    cy = prim.center.y.abs
    rx = sqrt((prim.x_point.x.abs - cx)^2 +
              (prim.x_point.y.abs - cy)^2)
    ry = sqrt((prim.y_point.x.abs - cx)^2 +
              (prim.y_point.y.abs - cy)^2)
    theta = rad2deg(atan2(prim.x_point.y.abs - cy,
                          prim.x_point.x.abs - cx))

    if !all(isfinite([cx, cy, rx, ry, theta]))
        return
    end

    indent(img)
    @printf(img.out, "<ellipse cx=\"%s\" cy=\"%s\" rx=\"%s\" ry=\"%s\"",
            svg_fmt_float(cx), svg_fmt_float(cy), svg_fmt_float(rx),
            svg_fmt_float(ry))
    if abs(theta) > 1e-4
        @printf(img.out, " transform=\"rotate(%s %s %s)\"",
                svg_fmt_float(theta), svg_fmt_float(cx), svg_fmt_float(cy))
    end
    print_vector_properties(img, idx)
    write(img.out, "/>\n")
end


function draw(img::SVG, prim::LinesPrimitive, idx::Int)
     n = length(prim.points)
     if n <= 1; return; end

     indent(img)
     write(img.out, "<path d=\"")
     print_svg_path(img.out, prim.points, true)
     write(img.out, "\"")
     print_vector_properties(img, idx)
     write(img.out, "/>\n")
end


function draw(img::SVG, prim::TextPrimitive, idx::Int)
    indent(img)
    @printf(img.out, "<text x=\"%s\" y=\"%s\"",
            svg_fmt_float(prim.position.x.abs),
            svg_fmt_float(prim.position.y.abs))

    if is(prim.halign, hcenter)
        print(img.out, " text-anchor=\"middle\"")
    elseif is(prim.halign, hright)
        print(img.out, " text-anchor=\"end\"")
    end

    # NOTE: "dominant-baseline" is the correct way to vertically center text
    # in SVG, but implementations are pretty inconsistent (chrome in particular
    # does a really bad job). We fake it by shifting by some reasonable amount.
    if is(prim.valign, vcenter)
        print(img.out, " dy=\"0.4em\"")
        #print(img.out, " style=\"dominant-baseline:central\"")
    elseif is(prim.valign, vtop)
        print(img.out, " dy=\"0.6em\"")
        #print(img.out, " style=\"dominant-baseline:text-before-edge\"")
    end

    if prim.rot.theta != 0.0
        @printf(img.out, " transform=\"rotate(%s, %s, %s)\"",
                svg_fmt_float(rad2deg(prim.rot.theta)),
                svg_fmt_float(prim.rot.offset.x.abs),
                svg_fmt_float(prim.rot.offset.y.abs))
    end
    print_vector_properties(img, idx)

    @printf(img.out, ">%s</text>\n",
            pango_to_svg(prim.value))
end


function draw(img::SVG, prim::CurvePrimitive, idx::Int)
    indent(img)
    @printf(img.out, "<path d=\"M%s,%s C%s,%s %s,%s %s,%s\"",
        svg_fmt_float(prim.anchor0.x.abs),
        svg_fmt_float(prim.anchor0.y.abs),
        svg_fmt_float(prim.ctrl0.x.abs),
        svg_fmt_float(prim.ctrl0.y.abs),
        svg_fmt_float(prim.ctrl1.x.abs),
        svg_fmt_float(prim.ctrl1.y.abs),
        svg_fmt_float(prim.anchor1.x.abs),
        svg_fmt_float(prim.anchor1.y.abs))
    print_vector_properties(img, idx)
    write(img.out, "/>\n")
end


function draw(img::SVG, prim::BitmapPrimitive, idx::Int)
    indent(img)
    @printf(img.out, "<image x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\"",
            svg_fmt_float(prim.corner.x.abs), svg_fmt_float(prim.corner.y.abs),
            svg_fmt_float(prim.width.abs), svg_fmt_float(prim.height.abs))
    print_vector_properties(img, idx)
    @printf(img.out, " xlink:href=\"data:%s;base64,%s\"></image>\n",
            prim.mime, base64(prim.data))
end


# Applying properties
# -------------------


# Return a URL corresponding to a ClipPrimitive
function clippathurl(img::SVG, property::ClipPrimitive)
    idx = get!(() -> length(img.clippaths) + 1, img.clippaths, property)
    return string("clippath", idx)
end


function push_property_frame(img::SVG, properties::Vector{Property})
    if isempty(properties)
        return
    end

    frame = SVGPropertyFrame()
    applied_properties = Set{Type}()
    scalar_properties = Array(Property, 0)
    for property in properties
        if !isrepeatable(property) && (typeof(property) in applied_properties)
            continue
        elseif isscalar(property)
            push!(scalar_properties, property)
            push!(applied_properties, typeof(property))
            frame.has_scalar_properties = true
        else
            frame.vector_properties[typeof(property)] = property
            img.vector_properties[typeof(property)] = property
        end
    end
    push!(img.property_stack, frame)
    if isempty(scalar_properties)
        return
    end

    id_needed = any([isa(property, JSCall) for property in scalar_properties])
    for property in scalar_properties
        if isa(property, SVGID)
            img.current_id = p.primitives[1].value
        end
    end

    if img.current_id === nothing
        img.current_id = genid(img)
        push!(scalar_properties, svgid(img.current_id))
    end

    indent(img)
    write(img.out, "<g")
    for property in scalar_properties
        print_property(img, property.primitives[1])
    end
    write(img.out, ">\n");
    img.current_id = nothing
    img.indentation += 1
end


function pop_property_frame(img::SVG)
    @assert !isempty(img.property_stack)
    frame = pop!(img.property_stack)

    if frame.has_scalar_properties
        img.indentation -= 1
        indent(img)
        write(img.out, "</g>")
        if frame.has_link
            write(img.out, "</a>")
        end
        if frame.has_mask
            write(img.out, "</mask>")
        end
        write(img.out, "\n")
    end

    for (propertytype, property) in frame.vector_properties
        img.vector_properties[propertytype] = nothing
        for i in length(img.property_stack):-1:1
            if haskey(img.property_stack[i].vector_properties, propertytype)
                img.vector_properties[propertytype] =
                    img.property_stack.vector_properties[i]
            end
        end
    end
end

