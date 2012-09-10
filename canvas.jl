
# Canvas: a thing upon which other things are placed.

require("measure.jl")
require("form.jl")
require("property.jl")


type Units
    # TODO: using a bounding box implies we can use any sort of units, when in
    # fact the box used here should not have units associated with it.
    box::BoundingBox

    function Units(width::MeasureOrNumber,
                   height::MeasureOrNumber)
        new(BoundingBox(0.0, 0.0, width, height))
    end

    function Units(x0::MeasureOrNumber,
                   y0::MeasureOrNumber,
                   width::MeasureOrNumber,
                   height::MeasureOrNumber)
        new(BoundingBox(x0, y0, width, height))
    end

    function Units(box::BoundingBox)
        new(box)
    end
end


type Canvas
    box::BoundingBox
    property::Property
    children::Vector{Canvas}
    form::Form
    unit_box::BoundingBox
    rot::Rotation

    function Canvas(opts::Union(Units, Rotation)...)
        Canvas(0.0w, 0.0h, 1.0w, 1.0h, opts...)
    end

    function Canvas(x0::MeasureOrNumber,
                    y0::MeasureOrNumber,
                    width::MeasureOrNumber,
                    height::MeasureOrNumber,
                    opts::Union(Units, Rotation)...)
        c = new(BoundingBox(x0, y0, width, height),
                Property(),
                Canvas[],
                Form(),
                BoundingBox(),
                Rotation())

        for opt in opts
            if typeof(opt) == Rotation
                c.rot = opt
            elseif typeof(opt) == Units
                c.unit_box = opt.box
            end
        end

        c
    end

    # copy constructor
    function Canvas(canvas::Canvas)
        new(copy(canvas.box),
            copy(canvas.property),
            copy(canvas.children),
            copy(canvas.form),
            copy(canvas.unit_box),
            copy(canvas.rot))
    end
end


copy(canvas::Canvas) = Canvas(canvas)


