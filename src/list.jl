# Basic list

@compat abstract type List{T} end

struct ListNull{T} <: List{T} end

copy{T}(l::ListNull{T}) = l

type ListNode{T} <: List{T}
    head::T
    tail::List{T}
end

copy{T}(l::ListNode{T}) = ListNode{T}(l.head, l.tail)

head(l::ListNode) = l.head
tail(l::ListNode) = l.tail

# iterator
start(l::List) = l
next(::List, l::List) = (l.head, l.tail)
done(::List, l::List) = typeof(l) <: ListNull
cons{T}(value, l::List{T}) = ListNode{T}(value, l)

function length{T}(l::List{T})
    n = 0
    while typeof(l) != ListNull{T}
        n += 1
        l = l.tail
    end
    n
end

function cat{T}(a::List{T}, b::List{T})
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

function show{T}(io::IO, a::List{T})
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
