using Compose

#Run the examples
testdir = joinpath(Pkg.dir("Compose"), "test")
cd(testdir)

exampledir = joinpath(Pkg.dir("Compose"), "examples")
for ex in readdir(exampledir)
    endswith(ex, ".jl") || continue
    srand(1) #Needed so that SVG uuid is reproducible
    include(joinpath(exampledir, ex))
end

#Compare with cached output
cachedout = joinpath(Pkg.dir("Compose"), "test", "data")
differentfiles = String[]
for output in readdir(cachedout)
    cached = open(readall, joinpath(cachedout, output))
    genned = open(readall, joinpath(testdir, output))
    if cached != genned
        push!(differentfiles, output)
    else #Delete generated file
        rm(joinpath(testdir, output))
    end
end

#Print out which files differ and their diffs
if length(differentfiles)>0
    #Capture diffs
    diffs = map(
        output -> output * ":\n" *
            readall(ignorestatus(`diff $(joinpath(cachedout, output)) $(joinpath(testdir, output))`)) *
            "\n\n",
        differentfiles)
    error(string("Generated output differs from cached test output:\n",
        join(differentfiles, "\n"), "\n\n", join(diffs, "\n")))
end

