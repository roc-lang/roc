app [main!] { pf: platform "./platform/main.roc" }

make : Str -> (I64 -> Str)
make = |prefix| |n| "${prefix}${n.to_str()}"

stored = make("a captured string longer than inline storage: ")

countdown : I64 -> I64
countdown = |n| if n == 0 42 else Box.unbox(Box.box(countdown))(n - 1)

main! = || {
    returned = Box.unbox(Box.box(make))("a returned string longer than inline storage: ")
    "${returned(countdown(20000))}${stored(8)}"
}
