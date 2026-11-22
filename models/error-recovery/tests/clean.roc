# expect: pass clean
# A well-typed program: every mode reports no errors.
Pt : { n : I64, s : Str }

Sh : [A(I64), B(Str, Bool), C]

size : Sh -> I64
size = |sh| match sh {
    A(n) => n
    B(s, _) => Str.count_utf8_bytes(s).to_i64_wrap()
    C => 0
}

total : List(Pt) -> I64
total = |ps| {
    var $acc = 0
    for p in ps {
        $acc = $acc + p.n
    }
    $acc
}

safe : I64 -> Try(I64, [Oops])
safe = |n| if n < 0 { Err(Oops) } else { Ok(n) }

twice : I64 -> Try(I64, [Oops])
twice = |n| Ok(safe(n)? + safe(n)?)

expect size(B("ab", True)) == 2

main! = |_args| {
    echo!(Str.inspect((size(A(3)), total([{ n: 1, s: "x" }]), twice(4))))
    Ok({})
}
