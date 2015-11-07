using Compose

include("misc.jl")
include("immerse.jl")

# Run the examples
const testdir = dirname(@__FILE__)
cd(testdir)

exampledir = joinpath(testdir, "..", "examples")
for ex in readdir(exampledir)
    endswith(ex, ".jl") || continue
    srand(1) #Needed so that SVG uuid is reproducible
    include(joinpath(exampledir, ex))
end

if !haskey(ENV, "TRAVIS")
    include("compare_examples.jl")
end
