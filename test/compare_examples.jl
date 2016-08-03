# Compare with cached output
cachedout = joinpath(testdir, "data")
differentfiles = AbstractString[]
const creator_producer = r"(Creator|Producer)"
for output in readdir(cachedout)
    cached = open(readlines, joinpath(cachedout, output))
    genned = open(readlines, joinpath(testdir, output))
    same = (n=length(cached)) == length(genned)
    if same
        lsame = Bool[cached[i] == genned[i] for i = 1:n]
        if !all(lsame)
            for idx in find(!lsame)
                # Don't worry about lines that are due to
                # Creator/Producer (e.g., Cairo versions)
                if !isempty(search(cached[idx], creator_producer))
                    lsame[idx] = true
                end
            end
        end
        same = same & all(lsame)
    end
    if !same
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
            readstring(ignorestatus(`diff $(joinpath(cachedout, output)) $(joinpath(testdir, output))`)) *
            "\n\n",
        differentfiles)
    error(string("Generated output differs from cached test output:\n",
        join(differentfiles, "\n"), "\n\n", join(diffs, "\n")))
end
