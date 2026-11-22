# expect: pass errors
# One type error of each common shape; every mode reports errors and none crashes.
Pt : { n : I64, s : Str }

Sh : [A(I64), B(Str, Bool), C]

size : Sh -> I64
size = |sh| match sh {
    A(n) => n
    B(s, _) => s
    C => 0
}

total : List(Pt) -> I64
total = |ps| {
    var $acc = 0
    for p in ps {
        $acc = $acc + p.missing
    }
    $acc
}

twice : I64 -> Try(I64, [Oops])
twice = |n| Ok(n? + undefined_name)

expect size(D) == 2

main! = |_args| {
    echo!(Str.inspect((size(A("3")), total([{ n: 1 }]), twice(4))))
    Ok({})
}
