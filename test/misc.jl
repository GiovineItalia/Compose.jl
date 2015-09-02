using Colors, Base.Test

tomato_bisque =
           compose(context(),
                   (context(), circle(), fill(colorant"bisque")),
                   (context(), rectangle(), fill(colorant"tomato")))

io = IOBuffer()
showcompact(io, tomato_bisque)
str = takebuf_string(io)
@test str == "Context(Context(f,R),Context(f,C))"
