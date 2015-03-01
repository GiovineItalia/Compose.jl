type PGFPropertyFrame
    # Vector properties in this frame.
    vector_properties::Dict{Type, Property}

    # True if this property frame has scalar properties. Scalar properties are
    # emitted as a group {scope} that must be closed when the frame is popped.
    has_scalar_properties::Bool

    function PGFPropertyFrame()
        return new(Dict{Type, Property}(), false)
    end
end

type PGF <: Backend
    # Image size in millimeters.
    width::Float64
    height::Float64

    # Output stream.
    out::IO

    # Fill properties cannot be "cleanly" applied to
    # multiple form primitives.  It must be applied
    # each time an object is drawn
    fill::Union(Nothing, ColorValue)
    fill_opacity::Float64

    stroke::Union(Nothing, ColorValue)
    stroke_opacity::Float64

    fontfamily::Union(Nothing,String)
    fontsize::Float64

    # Current level of indentation.
    indentation::Int

    # Output buffer.  We want the ability to add to the beginning of
    buf::IOBuffer

    # Skip drawing if visible is false
    visible::Bool

    # Have not found an easy way to define color as
    # a draw parameter.  Whenever we encounter a color, we add it to the
    # color_set set.  That way, we can write out all the color
    # definitions at the same time.
    color_set::Set{ColorValue}

    # Stack of property frames (groups of properties) currently in effect.
    property_stack::Vector{PGFPropertyFrame}

    # SVG forbids defining the same property twice, so we have to keep track
    # of which vector property of which type is in effect. If two properties of
    # the same type are in effect, the one higher on the stack takes precedence.
    vector_properties::Dict{Type, Union(Nothing, Property)}

    # Clip-paths that need to be defined at the end of the document.
    # Not quite sure how to deal with clip paths yet
    clippath::Union(Nothing,ClipPrimitive)
    # clippaths::Dict{ClipPrimitive, String}

    # True when finish has been called and no more drawing should occur
    finished::Bool

    # Backend is responsible for opening/closing the file
    ownedfile::Bool

    # Filename when ownedfile is true
    filename::Union(String, Nothing)

    # Emit the graphic on finish when writing to a buffer.
    emit_on_finish::Bool

    # Emit only the tikzpicture environment
    only_tikz :: Bool

    function PGF(out::IO,
                 width,
                 height,
                 emit_on_finish::Bool=true,
                 only_tikz = false)
        width = size_measure(width)
        height = size_measure(height)
        if !isabsolute(width) || !isabsolute(height)
            error("PGF image size must be specified in absolute units.")
        end

        img = new()
        img.buf = IOBuffer()
        img.fill = default_fill_color
        img.stroke = default_stroke_color
        img.fill_opacity = 1.0
        img.stroke_opacity = 1.0
        img.width  = width.abs
        img.height = height.abs
        img.fontfamily = nothing
        img.clippath = nothing
        img.fontsize = 12.0
        img.indentation = 0
        img.out = out
        img.color_set = Set{ColorValue}([color("black")])
        img.property_stack = Array(PGFPropertyFrame, 0)
        img.vector_properties = Dict{Type, Union(Nothing, Property)}()
        # img.clippaths = Dict{ClipPrimitive, String}()
        img.visible = true
        img.finished = false
        img.emit_on_finish = emit_on_finish
        img.ownedfile = false
        img.filename = nothing
        img.only_tikz = only_tikz
        return img
    end

    # Write to a file.
    function PGF(filename::String, width, height, only_tikz = false)
        out = open(filename, "w")
        img = PGF(out, width, height, true, only_tikz)
        img.ownedfile = true
        img.filename = filename
        return img
    end

    # Write to buffer.
    function PGF(width::MeasureOrNumber, height::MeasureOrNumber,
                 emit_on_finish::Bool=true, only_tikz = false)
        return PGF(IOBuffer(), width, height, emit_on_finish, only_tikz)
    end
end

function finish(img::PGF)
    if img.finished
        return
    end

    while !isempty(img.property_stack)
        pop_property_frame(img)
    end


    # if length(img.clippaths) > 0
    #     for (clippath, id) in img.clippaths
    #         write(img.out, "\\clip")
    #         print_svg_path(img.out, clippath.points)
    #         write(img.out, ";\n")
    #     end
    # end

    writeheader(img)
    writecolors(img)
    write(img.out, takebuf_array(img.buf))
    write(img.out,
        """
        \\end{tikzpicture}
        """
        )
    !img.only_tikz &&  write(img.out,
        """
        \\end{document}
        """
        )

    if method_exists(flush, (typeof(img.out),))
        flush(img.out)
    end
    close(img.buf)

    if img.ownedfile
        close(img.out)
    end

    img.finished = true

    # If we are writing to a buffer. Collect the string and emit it.
    if img.emit_on_finish && typeof(img.out) == IOBuffer
        display(img)
    end
end


function isfinished(img::PGF)
    img.finished
end

function root_box(img::PGF)
    AbsoluteBoundingBox(0.0, 0.0, img.width, img.height)
end

function writeheader(img::PGF)
    !img.only_tikz && write(img.out,
        """
        \\documentclass{minimal}
        \\usepackage{pgfplots}
        \\usepackage{fontspec}
        \\usepackage{amsmath}
        \\usepackage[active,tightpage]{preview}
        \\PreviewEnvironment{tikzpicture}
        \\begin{document}
        """)
    write(img.out, """
        \\begin{tikzpicture}[x=1mm,y=-1mm]
        """)
    return img
end

function writecolors(img::PGF)
    for color in img.color_set
        @printf(img.out, "\\definecolor{mycolor%s}{rgb}{%s,%s,%s}\n",
            hex(color),
            svg_fmt_float(color.r),
            svg_fmt_float(color.g),
            svg_fmt_float(color.b)
            )
    end
end

function print_pgf_path(out::IO, points::Vector{SimplePoint},
                        bridge_gaps::Bool=false)
    isfirst = true
    for point in points
        x, y = point.x.abs, point.y.abs
        if !(isfinite(x) && isfinite(y))
            isfirst = true
            continue
        end

        if isfirst
            isfirst = false
            @printf(out, " (%s,%s)",
                    svg_fmt_float(x),
                    svg_fmt_float(y))
        else
            @printf(out, " -- (%s,%s)",
                    svg_fmt_float(x),
                    svg_fmt_float(y))
        end
    end
end

function get_vector_properties(img::PGF, idx::Int)
    props_str = ASCIIString[]
    modifiers = ASCIIString[]
    for (propertytype, property) in img.vector_properties
        if property === nothing
            continue
        end
        if idx > length(property.primitives)
            error("Vector form and vector property differ in length. Can't distribute.")
        end
        push_property!(props_str, img, property.primitives[idx])
    end

    if img.fill != nothing
        push!(props_str, string("fill=mycolor",hex(img.fill)))
        if img.fill_opacity < 1.0
            push!(props_str, string("fill opacity=",svg_fmt_float(img.fill_opacity)))
        end
    end

    if img.stroke != nothing
        push!(props_str, string("draw=mycolor",hex(img.stroke)))
        if img.stroke_opacity < 1.0
            push!(props_str, string("draw opacity=",svg_fmt_float(img.stroke_opacity)))
        end
    end



    return modifiers, props_str
end

function push_property!(props_str, img::PGF, property::StrokeDashPrimitive)
    if isempty(property.value)
        return
    else
        push!(props_str, string("dash pattern=", join(map(v -> join(v, " "),zip(cycle(["on", "off"]),map(v -> string(svg_fmt_float(v.abs), "mm"), property.value)))," ")))
    end
end

function push_property!(props_str, img::PGF, property::StrokePrimitive)
    if isa(property.color, AlphaColorValue)
        img.stroke = property.color.c
        img.stroke_opacity = property.color.alpha
    else
        img.stroke = property.color
    end

    if img.stroke != nothing
        push!(img.color_set, convert(RGB, property.color))
    end
end

function push_property!(props_str, img::PGF, property::FillPrimitive)
    if isa(property.color, AlphaColorValue)
        img.fill = property.color.c
        img.fill_opacity = property.color.alpha
    else
        img.fill = property.color
    end

    if img.fill != nothing
        push!(img.color_set, convert(RGB, property.color))
    end
end

function push_property!(props_str, img::PGF, property::VisiblePrimitive)
    img.visible = property.value
end

function push_property!(props_str, img::PGF, property::LineWidthPrimitive)
    push!(props_str, string("line width=", svg_fmt_float(property.value.abs), "mm"))
end

pgf_fmt_linecap(::LineCapButt) = "butt"
pgf_fmt_linecap(::LineCapSquare) = "rect"
pgf_fmt_linecap(::LineCapRound) = "round"

function push_property!(props_str, img::PGF, property::StrokeLineCapPrimitive)
    push!(props_str, string("line cap=", pgf_fmt_linecap(property.value)))
end

function push_property!(props_str, img::PGF, property::StrokeLineJoinPrimitive)
    push!(props_str, string("line join=", svg_fmt_linejoin(property.value)))
end

function push_property!(props_str, img::PGF, property::FillOpacityPrimitive)
    img.fill_opacity = property.value
end

function push_property!(props_str, img::PGF, property::StrokeOpacityPrimitive)
    img.stroke_opacity = property.value
end

function push_property!(props_str, img::PGF, property::FontPrimitive)
    # Can only only work with one font family for now

    img.fontfamily = strip(split(escape_string(property.family),',')[1],'\'')
end

function push_property!(props_str, img::PGF, property::FontSizePrimitive)
    img.fontsize = property.value.abs
    # @printf(img.out, " font-size=\"%s\"", svg_fmt_float(property.value.abs))
end

function push_property!(props_str, img::PGF, property::ClipPrimitive)
    img.clippath = property
    # Not quite sure how to handle clipping yet, stub for now
end

# Stubs for SVG and JS specific properties
function push_property!(props_str, img::PGF, property::JSIncludePrimitive)
end

function push_property!(props_str, img::PGF, property::JSCallPrimitive)
end

function push_property!(props_str, img::PGF, property::SVGClassPrimitive)
end

function push_property!(props_str, img::PGF, property::SVGAttributePrimitive)
end

function iswithjs(img::PGF)
    return false
end

function iswithousjs(img::PGF)
    return true
end

function draw(img::PGF, form::Form)
    for (idx, primitive) in enumerate(form.primitives)
        draw(img, primitive, idx)
    end
end

function draw(img::PGF, prim::LinePrimitive, idx::Int)

    n = length(prim.points)
    if n <= 1; return; end

    modifiers, props = get_vector_properties(img, idx)
    if !img.visible; return; end

    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] ", join(props, ","));
    print_pgf_path(img.buf, prim.points, true)
    write(img.buf, ";\n")
end

function draw(img::PGF, prim::RectanglePrimitive, idx::Int)
    width = max(prim.width.abs, 0.01)
    height = max(prim.height.abs, 0.01)

    modifiers, props = get_vector_properties(img, idx)
    if !img.visible; return; end

    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] ", join(props, ","));
    @printf(img.buf, "(%s,%s) rectangle +(%s,%s);\n",
            svg_fmt_float(prim.corner.x.abs),
            svg_fmt_float(prim.corner.y.abs),
            svg_fmt_float(width),
            svg_fmt_float(height))
end

function draw(img::PGF, prim::PolygonPrimitive, idx::Int)
    n = length(prim.points)
    if n <= 1; return; end

    modifiers, props = get_vector_properties(img, idx)
    if !img.visible; return; end

    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] ", join(props, ","))
    print_pgf_path(img.buf, prim.points, true)
    write(img.buf, " -- cycle;\n")
end

function draw(img::PGF, prim::CirclePrimitive, idx::Int)
    modifiers, props = get_vector_properties(img, idx)
    if !img.visible; return; end
    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] ", join(props, ","))
    @printf(img.buf, "(%s,%s) circle [radius=%s];\n",
        svg_fmt_float(prim.center.x.abs),
        svg_fmt_float(prim.center.y.abs),
        svg_fmt_float(prim.radius.abs))
end

function draw(img::PGF, prim::EllipsePrimitive, idx::Int)

    modifiers, props = get_vector_properties(img, idx)
    if !img.visible; return; end

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

    modifiers, props = get_vector_properties(img, idx)
    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] ", join(props, ","))
    @printf(img.buf, "(%s,%s) circle [x radius=%s, y radius=%s",
        svg_fmt_float(cx),
        svg_fmt_float(cy),
        svg_fmt_float(rx),
        svg_fmt_float(ry))
    if abs(theta) > 1e-4
        @printf(img.buf, " rotate=%s", svg_fmt_float(theta))
    end
    write(img.buf, "];\n")
end

function draw(img::PGF, prim::CurvePrimitive, idx::Int)
    modifiers, props = get_vector_properties(img, idx)
    if !img.visible; return; end
    write(img.buf, join(modifiers))
    @printf(img.buf, "\\path [%s] ", join(props, ","))
    @printf(img.buf, "(%s,%s) .. controls (%s,%s) and (%s,%s) .. (%s,%s);\n",
        svg_fmt_float(prim.anchor0.x.abs),
        svg_fmt_float(prim.anchor0.y.abs),
        svg_fmt_float(prim.ctrl0.x.abs),
        svg_fmt_float(prim.ctrl0.y.abs),
        svg_fmt_float(prim.ctrl1.x.abs),
        svg_fmt_float(prim.ctrl1.y.abs),
        svg_fmt_float(prim.anchor1.x.abs),
        svg_fmt_float(prim.anchor1.y.abs))
end

function draw(img::PGF, prim::TextPrimitive, idx::Int)

    # Rotation direction is reversed!
    modifiers, props = get_vector_properties(img, idx)
    if !img.visible; return; end
    push!(props, string(
        "rotate around={",
            svg_fmt_float(-rad2deg(prim.rot.theta)),
            ": (",
            svg_fmt_float(prim.rot.offset.x.abs - prim.position.x.abs),
            ",",
            svg_fmt_float(prim.rot.offset.y.abs - prim.position.y.abs),
            ")}"))
    if is(prim.halign, hcenter)
        push!(props, "inner sep=0.0")
    elseif is(prim.halign, hright)
        push!(props, "left,inner sep=0.0")
    else
        push!(props, "right,inner sep=0.0")
    end
    write(img.buf, join(modifiers))
    @printf(img.buf, "\\draw (%s,%s) node [%s]{\\fontsize{%smm}{%smm}\\selectfont \$%s\$};\n",
        svg_fmt_float(prim.position.x.abs),
        svg_fmt_float(prim.position.y.abs),
        replace(join(props, ","), "fill", "text"),
        svg_fmt_float(img.fontsize),
        svg_fmt_float(1.2*img.fontsize),
        pango_to_pgf(prim.value)
    )
end


function indent(img::PGF)
    for i in 1:img.indentation
        write(img.buf, "  ")
    end
end



function push_property_frame(img::PGF, properties::Vector{Property})
    if isempty(properties)
        return
    end

    frame = PGFPropertyFrame()
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

    write(img.buf, "\\begin{scope}\n")
    prop_str = String[]
    for property in scalar_properties
        push_property!(prop_str, img, property.primitives[1])
    end
    if length(prop_str) > 0
        @printf(img.buf, "[%s]\n", join(prop_str, ","))
    end
    if img.clippath != nothing
        write(img.buf, "\\clip ")
        print_pgf_path(img.buf, img.clippath.points)
        write(img.buf, ";\n")
    end
   if img.fontfamily != nothing
       @printf(img.buf, "\\fontspec{%s}\n", img.fontfamily)
    end
end


function pop_property_frame(img::PGF)
    @assert !isempty(img.property_stack)
    frame = pop!(img.property_stack)

    if frame.has_scalar_properties
        write(img.buf, "\\end{scope}\n")
        # There should be a better way to to this:
        # Maybe put the applied properties on a stack then
        # pop them out here?
        # FIX ME!
        img.fill = default_fill_color
        img.stroke = default_stroke_color
        img.fill_opacity = 1.0
        img.stroke_opacity = 1.0
        img.fontfamily = nothing
        img.clippath = nothing
        img.visible = true
    end

    for (propertytype, property) in frame.vector_properties
        img.vector_properties[propertytype] = nothing
        for i in length(img.property_stack):-1:1
            if haskey(img.property_stack[i].vector_properties, propertytype)
                img.vector_properties[propertytype] =
                    img.property_stack[i].vector_properties[propertytype]
            end
        end
    end
end

# Horrible abuse of Latex inline math mode just to
# get something working first.
# FIX ME!
function pango_to_pgf(text::String)
    pat = r"<(/?)\s*([^>]*)\s*>"
    input = convert(Array{Uint8}, text)
    output = IOBuffer()
    lastpos = 1
    for mat in eachmatch(pat, text)
        write(output, "\\text{")
        write(output, input[lastpos:mat.offset-1])
        write(output, "}")

        if mat.captures[2] == "sup"
            if mat.captures[1] == "/"
                write(output, "}")
            else
                write(output, "^{")
            end
        elseif mat.captures[2] == "sub"
            if mat.captures[1] == "/"
                write(output, "}")
            else
                write(output, "_{")
            end
        elseif mat.captures[2] == "i"
            if mat.captures[1] == "/"
                write(output, "}")
            else
                write(output, "\\textit{")
            end
        elseif mat.captures[2] == "b"
            if mat.captures[1] == "/"
                write(output, "}")
            else
                write(output, "\\textbf{")
            end
        end
        lastpos = mat.offset + length(mat.match)
    end
    write(output, "\\text{")
    write(output, input[lastpos:end])
    write(output, "}")

    # Cleanup characters
    str_out = bytestring(output)
    replace(str_out, "%", "\\%")
end


