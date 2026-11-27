Rec1 := [Leaf1, Node1(Rec1, I64)].{ is_eq : _ }
W2 :: ([A, B], [A, B]).{ is_eq : _ }
W3 := Str.{ is_eq : _ }

R0 : [Arm0({}, Str, Str, (W2, { a : [A, B] }, ([A, B], Bool)), [A, B], [A, B], Bool), Arm1(W2, [A, B]), Arm2(({ a : Rec1, b : Str }, (W2, { a : [A, B] }, ([A, B], Bool))))]

f0 : ({ a : Rec1, b : Str }, (W2, { a : [A, B] }, ([A, B], Bool))), U64 -> R0
f0 = |x, k| match x {
    ({ a: Rec1.Leaf1, b: "${s1}/${s2}.b", .. }, (W2.(x3, _), { a: x4, ..rest5 }, (_, x6)) as w7) => Arm0(rest5, s1, s2, w7, x3, x4, x6)
    ({ .. }, (y15, { a: _, .. }, (y16, _))) | ({ .. }, (y15, { a: _ }, (y16, _))) => Arm1(y15, y16)
    x17 if k == 0 => Arm2(x17)
}

arm0 : R0 -> U64
arm0 = |r| match r {
    Arm0(_, _, _, _, _, _, _) => 0
    Arm1(_, _) => 1
    Arm2(_) => 2
}

cases0 : List((({ a : Rec1, b : Str }, (W2, { a : [A, B] }, ([A, B], Bool))), U64, R0))
cases0 = [
    (({ a: Rec1.Leaf1, b: "//a.b" }, (W2.(A, B), { a: B }, (B, False))), 0, Arm0({}, "", "/a", (W2.(A, B), { a: B }, (B, False)), A, B, False)),
    (({ a: Rec1.Leaf1, b: "//a.b" }, (W2.(A, B), { a: B }, (B, False))), 1, Arm0({}, "", "/a", (W2.(A, B), { a: B }, (B, False)), A, B, False)),
    (({ a: Rec1.Leaf1, b: "é/a.b.b" }, (W2.(B, B), { a: A }, (B, False))), 0, Arm1(W2.(B, B), B)),
    (({ a: Rec1.Leaf1, b: "é/a.b.b" }, (W2.(B, B), { a: A }, (B, False))), 1, Arm1(W2.(B, B), B)),
    (({ a: Rec1.Node1(Rec1.Leaf1, 0), b: "ab" }, (W2.(B, B), { a: A }, (A, True))), 1, Arm1(W2.(B, B), A)),
    (({ a: Rec1.Leaf1, b: "a" }, (W2.(B, A), { a: A }, (B, True))), 1, Arm1(W2.(B, A), B)),
    (({ a: Rec1.Leaf1, b: "ab" }, (W2.(A, A), { a: A }, (B, False))), 0, Arm1(W2.(A, A), B)),
    (({ a: Rec1.Node1(Rec1.Leaf1, 2), b: "" }, (W2.(B, B), { a: B }, (A, True))), 0, Arm1(W2.(B, B), A)),
    (({ a: Rec1.Node1(Rec1.Leaf1, 2), b: "" }, (W2.(B, B), { a: B }, (A, True))), 1, Arm1(W2.(B, B), A)),
    (({ a: Rec1.Leaf1, b: "ab" }, (W2.(B, B), { a: B }, (A, True))), 0, Arm1(W2.(B, B), A)),
    (({ a: Rec1.Leaf1, b: "ab" }, (W2.(B, B), { a: B }, (A, True))), 1, Arm1(W2.(B, B), A)),
    (({ a: Rec1.Leaf1, b: "é/b" }, (W2.(B, A), { a: A }, (B, False))), 0, Arm1(W2.(B, A), B)),
    (({ a: Rec1.Leaf1, b: "é/b" }, (W2.(B, A), { a: A }, (B, False))), 1, Arm1(W2.(B, A), B)),
    (({ a: Rec1.Node1(Rec1.Leaf1, 0), b: "" }, (W2.(B, B), { a: A }, (A, False))), 1, Arm1(W2.(B, B), A)),
    (({ a: Rec1.Leaf1, b: "ab" }, (W2.(B, B), { a: A }, (A, False))), 0, Arm1(W2.(B, B), A)),
    (({ a: Rec1.Leaf1, b: "é/b" }, (W2.(A, B), { a: A }, (B, False))), 1, Arm1(W2.(A, B), B)),
    (({ a: Rec1.Leaf1, b: "é/b" }, (W2.(B, B), { a: B }, (B, False))), 0, Arm1(W2.(B, B), B)),
    (({ a: Rec1.Node1(Rec1.Leaf1, (-9223372036854775807 - 1)), b: "é/b" }, (W2.(A, A), { a: A }, (A, False))), 0, Arm1(W2.(A, A), A)),
    (({ a: Rec1.Node1(Rec1.Leaf1, (-9223372036854775807 - 1)), b: "é/b" }, (W2.(A, A), { a: A }, (A, False))), 1, Arm1(W2.(A, A), A)),
    (({ a: Rec1.Node1(Rec1.Leaf1, 9223372036854775807), b: "a" }, (W2.(A, B), { a: B }, (B, False))), 0, Arm1(W2.(A, B), B)),
    (({ a: Rec1.Leaf1, b: "" }, (W2.(B, B), { a: A }, (A, True))), 1, Arm1(W2.(B, B), A)),
    (({ a: Rec1.Leaf1, b: "ab" }, (W2.(A, A), { a: B }, (A, False))), 1, Arm1(W2.(A, A), A)),
    (({ a: Rec1.Node1(Rec1.Leaf1, 9223372036854775807), b: "é/b" }, (W2.(B, B), { a: A }, (B, True))), 1, Arm1(W2.(B, B), B)),
    (({ a: Rec1.Node1(Rec1.Leaf1, 9223372036854775807), b: "" }, (W2.(B, A), { a: A }, (B, True))), 1, Arm1(W2.(B, A), B)),
    (({ a: Rec1.Leaf1, b: "é/b" }, (W2.(B, A), { a: B }, (A, True))), 0, Arm1(W2.(B, A), A)),
    (({ a: Rec1.Leaf1, b: "é/b" }, (W2.(B, A), { a: B }, (A, True))), 1, Arm1(W2.(B, A), A)),
    (({ a: Rec1.Leaf1, b: "é/b" }, (W2.(B, A), { a: B }, (B, False))), 1, Arm1(W2.(B, A), B)),
]

check0 : U64 -> Str
check0 = |z| {
    var $out = ""
    var $i = 0.U64
    for (v, k, want) in cases0 {
        got = f0(v, k + z)
        $out = if got == want $out else Str.concat($out, "u0 case ${$i.to_str()} arm ${arm0(got).to_str()}\n")
        $i = $i + 1
    }
    $out
}

R1 : [Arm0, Arm1, Arm2, Arm3([A(U8), B(W3)])]

f1 : [A(U8), B(W3)], U64 -> R1
f1 = |x, _k| match x {
    A(3) | B(_) | A(2) => Arm0
    A(2) => Arm1
    A(3) => Arm2
    x19 => Arm3(x19)
}

arm1 : R1 -> U64
arm1 = |r| match r {
    Arm0 => 0
    Arm1 => 1
    Arm2 => 2
    Arm3(_) => 3
}

cases1 : List(([A(U8), B(W3)], U64, R1))
cases1 = [
    (A(3), 1, Arm0),
    (B(W3.("")), 0, Arm0),
    (B(W3.("")), 1, Arm0),
    (A(2), 1, Arm0),
    (B(W3.("a")), 0, Arm0),
    (B(W3.("a")), 1, Arm0),
    (B(W3.("ab")), 0, Arm0),
    (B(W3.("ab")), 1, Arm0),
    (B(W3.("é/b")), 0, Arm0),
    (B(W3.("é/b")), 1, Arm0),
    (A(0), 0, Arm3(A(0))),
    (A(0), 1, Arm3(A(0))),
    (A(255), 0, Arm3(A(255))),
]

check1 : U64 -> Str
check1 = |z| {
    var $out = ""
    var $i = 0.U64
    for (v, k, want) in cases1 {
        got = f1(v, k + z)
        $out = if got == want $out else Str.concat($out, "u1 case ${$i.to_str()} arm ${arm1(got).to_str()}\n")
        $i = $i + 1
    }
    $out
}

main! = |args| {
    z = args.len() * 0
    echo!(Str.concat(check0(z), Str.concat(check1(z), "done")))
    Ok({})
}
