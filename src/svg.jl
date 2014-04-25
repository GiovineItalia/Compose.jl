


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

    # Javascript fragments are placed in a function with a unique name. This is
    # a map of unique function names to javascript code.
    scripts::Dict{String, String}

    # Clip-paths that need to be defined at the end of the document.
    clippaths::Vector{Vector{Point}}

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

    function SVG(out::IO,
                 width,
                 height,
                 emit_on_finish::Bool=true)
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
        img.clippaths = Array(Vector{Point}, 0)
        img.scripts = Dict{String, String}()
        img.embobj = Set{String}()
        img.finished = false
        img.emit_on_finish = emit_on_finish
        img.ownedfile = false
        img.filename = nothing
        writeheader(img)
        img
    end

    # Write to a file.
    function SVG(filename::String, width, height)
        f = open(filename, "w")
        img = SVG(f, width, height)
        img.ownedfile = true
        img.filename = filename
        img
    end

    # Write to buffer.
    function SVG(width::MeasureOrNumber, height::MeasureOrNumber,
                 emit_on_finish::Bool=true)
        img = SVG(IOBuffer(), width, height, emit_on_finish)
        img
    end
end


function writeheader(img::SVG)
    widthstr = svg_fmt_float(img.width)
    heightstr = svg_fmt_float(img.height)
    write(img.out,
          """
          <?xml version="1.0" encoding="UTF-8"?>
          <svg xmlns="http://www.w3.org/2000/svg"
               xmlns:xlink="http://www.w3.org/1999/xlink"
               version="1.1"
               width="$(widthstr)mm" height="$(heightstr)mm" viewBox="0 0 $(widthstr) $(heightstr)"
               stroke="$(svg_fmt_color(default_stroke_color))"
               fill="$(svg_fmt_color(default_fill_color))"
               stroke-width="$(svg_fmt_float(default_line_width.abs))">
          """)
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

    if length(img.scripts) > 0
        write(img.out, "<script type=\"application/ecmascript\"><![CDATA[\n")
        for (fn_name, js) in img.scripts
            @printf(img.out, "function %s(evt) {\n%s\n}\n\n", fn_name, js)
        end
        write(img.out, "]]></script>\n")
    end

    if length(img.clippaths) > 0
        write(img.out, "<defs>\n")
        for (i, clippath) in enumerate(img.clippaths)
            write(img.out, "<clipPath id=\"clippath$(i)\">\n  <path d=\"")
            print_svg_path(img.out, clippath)
            write(img.out, "\" />\n</clipPath\n>")
        end
        write(img.out, "</defs>\n")
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


# Print the property at the given index in each vector property
function print_vector_properties(img::SVG, idx::Int)
    for (propertytype, property) in img.vector_properties
		if idx > length(property.primitives)
			error("Vector form and vector property differ in length. Can't distribute.")
		end
		print_property(img, property.primitives[idx])
    end
end


# TODO: the rest of these


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


function draw(img::SVG, form::PolygonPrimitive)
     n = length(form.points)
     if n <= 1; return; end

     indent(img)
     write(img.out, "<path d=\"")
     print_svg_path(img.out, form.points, true)
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


# TODO: the rest of these


# Applying properties
# -------------------

# We are going to need to be able to pop batches of properties corresponding to
# contexts

# So we need to change this interface to push_properties

function push_property_frame(img::SVG, properties::Vector{Property})
    if isempty(properties)
        return
    end

    frame = SVGPropertyFrame()
    applied_properties = Set{Type}()
    scalar_properties = Array(Property, 0)
    for property in properties
        if isscalar(property) && !(typeof(property) in applied_properties)
            push!(scalar_properties, property)
            push!(applied_properties, typeof(property))
            frame.has_scalar_properties = true
        else
            frame.vector_properties[typeof(property)] = property
            img.vector_properties[typeof(property)] = property
        end

        # TODO: set frame flags for masks and links
    end
    push!(img.property_stack, frame)
    if isempty(scalar_properties)
        return
    end

    indent(img)
    write(img.out, "<g")
    for property in scalar_properties
        print_property(img, property.primitives[1])
    end
    write(img.out, ">\n");
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


# OLD VERSION:
# It's going to be trickier than this, since we no longer have one "Property"
# associated with each canvas or form.
function push_property(img::SVG, property::Property)
    if property === empty_property
        push!(img.empty_properties, true)
        push!(img.linked_properties, false)
        push!(img.mask_properties, false)
    else
        push!(img.empty_properties, false)
        indent(img)

        # There can only be one property of each type. E.g, defining 'fill'
        # twice in the same element is not valid svg.
        properties = Dict{Symbol, PropertyPrimitive}()
        rawproperties = {}
        p = property
        while !is(p, empty_property)
            if typeof(p.primitive) == SVGAttribute
                push!(rawproperties, p.primitive)
            else
                properties[symbol(string(typeof(p.primitive)))] = p.primitive
            end
            p = p.next
        end

        # Special-case for the mask property.
        if haskey(properties, :SVGDefineMask)
            write(img.out, @sprintf("<mask id=\"%s\">", properties[:SVGDefineMask].id))
            push!(img.mask_properties, true)
        else
            push!(img.mask_properties, false)
        end

        # Special-case for links.
        if haskey(properties, :SVGLink)
            write(img.out, @sprintf("<a xlink:href=\"%s\">",
                  properties[:SVGLink].target === nothing ? '#' : properties[:SVGLink].target))
            push!(img.linked_properties, true)
        else
            push!(img.linked_properties, false)
        end

        write(img.out, "<g")
        for p in chain(values(properties), rawproperties)
            apply_property(img, p)
        end
        write(img.out, ">\n")

        img.indentation += 1
    end
end


# # Nop catchall
# function apply_property(img::SVG, p::PropertyPrimitive)
# end


# function apply_property(img::SVG, p::Stroke)
#     @printf(img.out, " stroke=\"%s\"", svg_fmt_color(p.value))
# end


# function apply_property(img::SVG, p::StrokeDash)
#     @printf(img.out, " stroke-dasharray=\"%s\"", join(map(v -> svg_fmt_float(v.abs), p.value), ","))
# end


# function apply_property(img::SVG, p::Fill)
#     @printf(img.out, " fill=\"%s\"", svg_fmt_color(p.value))
# end


# function apply_property(img::SVG, p::LineWidth)
#     @printf(img.out, " stroke-width=\"%s\"", svg_fmt_float(p.value.abs))
# end


# function apply_property(img::SVG, p::Visible)
#     @printf(img.out, " visibility=\"%s\"",
#             p.value ? "visible" : "hidden")
# end


# function apply_property(img::SVG, p::Opacity)
#     @printf(img.out, " opacity=\"%s\"", fmt_float(p.value))
# end


# function apply_property(img::SVG, p::StrokeOpacity)
#     @printf(img.out, " stroke-opacity=\"%s\"", fmt_float(p.value))
# end



# function apply_property(img::SVG, p::SVGID)
#     @printf(img.out, " id=\"%s\"", escape_string(p.value))
# end

# function apply_property(img::SVG, p::SVGClass)
#     @printf(img.out, " class=\"%s\"", escape_string(p.value))
# end

# function apply_property(img::SVG, p::Font)
#     @printf(img.out, " font-family=\"%s\"", escape_string(p.family))
# end

# function apply_property(img::SVG, p::FontSize)
#     @printf(img.out, " font-size=\"%s\"", svg_fmt_float(p.value.abs))
# end

# function apply_property(img::SVG, p::Clip)
#     push!(img.clippaths, p.points)
#     i = length(img.clippaths)
#     write(img.out, " clip-path=\"url(#clippath$(i))\"")
# end

# function apply_property(img::SVG, p::SVGEmbed)
#     add!(img.embobj, p.markup)
# end


# function apply_property(img::SVG, p::SVGMask)
#     @printf(img.out, " mask=\"url(#%s)\"", p.id)
# end


# function apply_property(img::SVG, p::SVGAttribute)
#     @printf(img.out, " %s=\"%s\"", p.attribute, p.value)
# end


# for event in events
#     prop_name = lowercase(string(event))
#     @eval begin
#         function apply_property(img::SVG, p::($event))
#             fn_name = add_script(img, p.value, object_id(p.value))
#             @printf(img.out, " %s=\"%s(evt)\"", ($prop_name), fn_name)
#         end
#     end
# end


# # Add some javascript. Return the unique name generated for the wrapper function
# # containing the given code.
# function add_script(img::SVG, js::String, obj_id::Integer)
#     fn_name = @sprintf("js_chunk_%x", obj_id)
#     if !haskey(img.scripts, fn_name)
#         img.scripts[fn_name] = replace(js, r"^"m, "  ")
#     end
#     fn_name
# end

