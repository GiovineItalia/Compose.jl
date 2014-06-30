type PGF <: Backend
    # Image size in millimeters.
    width::Float64
    height::Float64

    # Output stream.
    out::IO

    # Unique ID for the figure.
    id::String

    # Have not found an easy way to define color as
    # a draw parameter.  Must be defined ahead of time
    color_stack::Vector{ColorValue}

    # Stack of property frames (groups of properties) currently in effect.
    property_stack::Vector{SVGPropertyFrame}

    # SVG forbids defining the same property twice, so we have to keep track
    # of which vector property of which type is in effect. If two properties of
    # the same type are in effect, the one higher on the stack takes precedence.
    vector_properties::Dict{Type, Union(Nothing, Property)}

    # Clip-paths that need to be defined at the end of the document.
    clippaths::Dict{ClipPrimitive, String}

    # True when finish has been called and no more drawing should occur
    finished::Bool

    # Backend is responsible for opening/closing the file
    ownedfile::Bool

    # Filename when ownedfile is true
    filename::Union(String, Nothing)

    # Emit the graphic on finish when writing to a buffer.
    emit_on_finish::Bool

    function PGF(out::IO,
                 width,
                 height,
                 emit_on_finish::Bool=true)
        width = size_measure(width)
        height = size_measure(height)
        if !isabsolute(width) || !isabsolute(height)
            error("PGF image size must be specified in absolute units.")
        end

        img = new()
        img.id = string("fig-", replace(string(Base.Random.uuid4()), "-", ""))
        img.width  = width.abs
        img.height = height.abs
        img.out = out
        img.color_stack = Array(ColorValue,0)
        img.property_stack = Array(SVGPropertyFrame, 0)
        img.vector_properties = Dict{Type, Union(Nothing, Property)}()
        img.clippaths = Dict{ClipPrimitive, String}()
        img.finished = false
        img.emit_on_finish = emit_on_finish
        img.ownedfile = false
        img.filename = nothing
        writeheader(img)
        return img
    end

    # Write to a file.
    function PGF(filename::String, width, height)
        out = open(filename, "w")
        img = PGF(out, width, height, true)
        img.ownedfile = true
        img.filename = filename
        return img
    end

    # Write to buffer.
    function PGF(width::MeasureOrNumber, height::MeasureOrNumber,
                 emit_on_finish::Bool=true)
        return SVG(IOBuffer(), width, height, emit_on_finish)
    end
end

function finish(img::PGF)
    if img.finished
        return
    end

    while !isempty(img.property_stack)
        pop_property_frame(img)
    end


    if length(img.clippaths) > 0
        for (clippath, id) in img.clippaths
            write(img.out, "\\clip")
            print_svg_path(img.out, clippath.points)
            write(img.out, ";\n")
        end
    end

    write(img.out,
        """
        \\end{tikzpicture}
        \\end{document}
        """
        )

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


function isfinished(img::PGF)
    img.finished
end

function push_property_frame(img::PGF, properties::Vector{Property})
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

    prop_str = String[]
    for property in scalar_properties
        push_property!(prop_str, img, property.primitives[1])
    end

    while length(img.color_stack) > 0
        c = convert(RGB, pop!(img.color_stack))
        @printf(img.out, "\\definecolor{mycolor%s}{RGB}{%i,%i,%i}\n",
            hex(c), int(255*c.r), int(255*c.g), int(255*c.b))
    end
    write(img.out, "\\begin{scope}")
    if length(prop_str) > 0
        @printf(img.out, "[%s]\n", join(prop_str, ","))
    end
end

function pop_property_frame(img::PGF)
    @assert !isempty(img.property_stack)
    frame = pop!(img.property_stack)

    if frame.has_scalar_properties
        write(img.out, "\\end{scope}\n")
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

function root_box(img::PGF)
    AbsoluteBoundingBox(0.0, 0.0, img.width, img.height)
end

function writeheader(img::PGF)
    write(img.out,
        """
        \\documentclass{minimal}
        \\usepackage{pgfplots}
        \\usepackage[active,tightpage]{preview}
        \\PreviewEnvironment{tikzpicture}
        \\begin{document}
        \\begin{tikzpicture}[x=1mm,y=-1mm]
        """)
    return img
end

function print_pgf_path(out, points::Vector{Point}, bridge_gaps::Bool=false)
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

function get_vector_properties(img::PGF)
    isfirst = true
    props_str = []
    for (propertytype, property) in img.vector_properties
        if property === nothing
            continue
        end
        if idx > length(property.primitives)
            error("Vector form and vector property differ in length. Can't distribute.")
        end
        push_property!(props_str, img, property.primitives[idx])
    end

    while length(img.color_stack) > 0
        c = pop!(img.color_stack)
        @printf(img.out, "\\definecolor{mycolor%s}{RGB}{%i,%i,%i}\n",
            hex(c), int(255*c.r), int(255*c.g), int(255*c.b))
    end

    if length(props_str) > 0
        return string("[", join(props_str, ", "), "]")
    else
        return ""
    end
end

function push_property!(props_str, img::PGF, property::StrokeDashPrimitive)
    if isempty(property.value)
        return
    else
        push!(props_str, string("dash pattern=", join(map(v -> join(v, " "),zip(cycle(["on", "off"]),map(v -> string(svg_fmt_float(v.abs), "mm"), property.value)))," ")))
        # return
    end
end

function push_property!(props_str, img::PGF, property::StrokePrimitive)
    push!(props_str, string("color=mycolor", hex(property.color)))
    push!(img.color_stack, property.color)
end

function push_property!(props_str, img::PGF, property::FillPrimitive)
    push!(props_str, string("draw/.style={fill=mycolor", hex(property.color), "}"))
    push!(img.color_stack, property.color)
end

function push_property!(props_str, img::PGF, property::LineWidthPrimitive)
    push!(props_str, string("line width=", svg_fmt_float(property.value.abs), "mm"))
end

function draw(img::PGF, form::Form)
    for (idx, primitive) in enumerate(form.primitives)
        draw(img, primitive)
    end
end

function draw(img::PGF, prim::PolygonPrimitive)
     n = length(prim.points)
     if n <= 1; return; end

     props = get_vector_properties(img)
     @printf(img.out, "\\draw %s", props)
     print_pgf_path(img.out, prim.points, true)
     write(img.out, " -- cycle;\n")
end

function draw(img::PGF, prim::LinePrimitive)
     n = length(prim.points)
     if n <= 1; return; end

     props = get_vector_properties(img)
     @printf(img.out, "\\draw %s", props)
     print_pgf_path(img.out, prim.points, true)
     write(img.out, ";\n")
end