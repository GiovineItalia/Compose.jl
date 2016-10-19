using Compose

include("misc.jl")
include("immerse.jl")

# Run the examples
const testdir = dirname(@__FILE__)
cd(testdir)

exampledir = joinpath(testdir, "..", "examples")
for ex in readdir(exampledir)
    endswith(ex, ".jl") || continue
    file = joinpath(exampledir, ex)
    expr = quote
        module $(Symbol(replace(ex, ".", "_")))
        using Compose
        using Colors
        using Compat
        srand(1) #Needed so that SVG uuid is reproducible
        include($file)
        end
    end
    expr.head = :toplevel
    eval(expr)
end

if !haskey(ENV, "TRAVIS")
    include("compare_examples.jl")
end
