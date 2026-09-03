module [first, second]

# A function reached by destructuring a computed value is that value's single
# function, so using it at two types is a type mismatch reported by the
# checker, not a crash later on.
mk = |_| { id: |x| x }
{ id } = mk({})

first = id("s")
second = id(1)
