## A block-local alias of a value is monomorphic: `made`'s top-level row is
## quantified, but a local alias of it is one runtime value, so used at one
## wider width its row widens to that width. (Two different widths are a type
## error; see the checker's "block-local value alias" tests.)
LocalValueAlias := {}

show_direct : [A, B(Str), C, D] -> Str
show_direct = |v| match v { A => "A", B(s) => "B(${s})", C => "C", D => "D" }

made : [B(Str), D]
made = B("x")

other : [B(Str), D]
other = D

expect {
    local = made
    show_direct(local) == "B(x)"
}

expect {
    local = other
    again = local
    show_direct(again) == "D"
}
