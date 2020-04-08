using Test

include("misc.jl")
include("svg.jl")
include("immerse.jl")

# Run the examples
cd(joinpath(@__DIR__, "output"))
exampledir = joinpath(@__DIR__, "examples")
for ex in readdir(exampledir)
    endswith(ex, ".jl") || continue
    file = joinpath(exampledir, ex)
    run(`$(Base.julia_cmd()) --startup-file=no $file`)
end
