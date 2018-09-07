# Basic list

abstract type List{T} end

struct ListNull{T} <: List{T} end

copy(l::ListNull{T}) where {T} = l

mutable struct ListNode{T} <: List{T}
    head::T
    tail::List{T}
end

copy(l::ListNode{T}) where {T} = ListNode{T}(l.head, l.tail)

head(l::ListNode) = l.head
tail(l::ListNode) = l.tail

# iterator
function Base.iterate(l::List, state=l)
    return typeof(state) <: ListNull ? nothing : (state.head, state.tail)
end
cons(value, l::List{T}) where T = ListNode{T}(value, l)

function length(l::List{T}) where T
    n = 0
    while typeof(l) != ListNull{T}
        n += 1
        l = l.tail
    end
    n
end

function cat(a::List{T}, b::List{T}) where T
    if a === nothing
        b
    else
        a = copy(a)
        u = a
        while u.tail !== nothing
            u.tail = copy(u.tail)
            u = u.tail
        end
        u.tail = b
        a
    end
end

function show(io::IO, a::List{T}) where T
    print(io, "List([")
    while typeof(a) != ListNull{T}
        print(io, a.head)
        if typeof(a.tail) != ListNull{T}
            print(io, ", ")
        end
        a = a.tail
    end
    print(io, "])")
end
