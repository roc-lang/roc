# Effect slots (design.md Effect Slots, Checking Effects And Const Roots).
# Each section is its own module; the verdicts and types must agree.
H := [H].{}

# === effectful call makes the lambda effectful
e! : U64 => U64
e! = |n| n

g! = |x| e!(x)

# === pure annotation rejects an effectful callback through a wrapper
e! : U64 => U64
e! = |n| n

w = |f| |x| f(x)

z : U64 -> U64
z = |n| w(e!)(n)

# === pure annotation rejects a direct effectful call
e! : U64 => U64
e! = |n| n

z : U64 -> U64
z = |n| e!(n)

# === effectful top-level value is rejected
e! : U64 => U64
e! = |n| n

top = e!(1.U64)

# === creating an effectful function is not effectful
e! : U64 => U64
e! = |n| n

w = |f| |x| f(x)

top = w(e!)

# === pure annotation accepts a pure callback through a generic apply
e! : U64 => U64
e! = |n| n

ap = |f, x| f(x)

p = |n| n

z : U64 -> U64
z = |n| ap(p, n)

# === pure and effectful uses of one generic apply are independent
e! : U64 => U64
e! = |n| n

ap = |f, x| f(x)

p = |n| n

u! = |_u| { a: ap(e!, 1.U64), b: ap(p, 1.U64) }

z : U64 -> U64
z = |n| ap(p, n)

# === an effectful local lambda that is never called
e! : U64 => U64
e! = |n| n

z : U64 -> U64
z = |n| {
    h = |y| e!(y)
    n
}

# === an effectful local lambda that is called
e! : U64 => U64
e! = |n| n

z : U64 -> U64
z = |n| {
    h = |y| e!(y)
    h(n)
}
