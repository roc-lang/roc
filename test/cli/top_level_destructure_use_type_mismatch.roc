module [first, second]

# A function bound by a top-level destructure is a single value, so using it at
# two types is a type mismatch reported by the checker, not a crash later on.
{ id } = { id: |x| x }

first = id("s")
second = id(1)
