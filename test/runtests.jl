using Compose
using Test

include("misc.jl")
@testset "SVG Correctness Tests" begin
    include("svg.jl")
end
include("immerse.jl")

# Run the examples
cd(joinpath(@__DIR__, "output"))

exampledir = joinpath(@__DIR__, "examples")
for ex in readdir(exampledir)
    endswith(ex, ".jl") || continue
    file = joinpath(exampledir, ex)
    expr = quote
        module $(Symbol(replace(ex, "."=>"_")))
        using Compose
        using Colors
        using Random
        Random.seed!(1) #Needed so that SVG uuid is reproducible
        include($file)
        end
    end
    expr.head = :toplevel
    eval(expr)
end
