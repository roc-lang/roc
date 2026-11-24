N1 :: [A1(Rec2), B1, C1([A, B(Bool), C, D(Str)], Rec3)].{ is_eq : _ }
Rec2 :: [Leaf2, Node2(Rec2, Str)].{ is_eq : _ }
Rec3 :: [Nil3, One3(I64), Two3(Rec3, Rec3)].{ is_eq : _ }

R0 : [Arm0, Arm1(Rec2), Arm2, Arm3(N1)]

f0 : N1, U64 -> R0
f0 = |x, k| match x {
    N1.A1(Leaf2) | C1(D("a"), Nil3) => Arm0
    A1(x1) if k == 1 => Arm1(x1)
    N1.A1(Node2(_, _)) => Arm2
    x2 => Arm3(x2)
}

arm0 : R0 -> U64
arm0 = |r| match r {
    Arm0 => 0
    Arm1(_) => 1
    Arm2 => 2
    Arm3(_) => 3
}

diff0 : R0, R0 -> Str
diff0 = |g, w| match (g, w) {
    (Arm1(g0), Arm1(w0)) => (if g0 == w0 "" else " x1")
    (Arm3(g0), Arm3(w0)) => (if g0 == w0 "" else " x2")
    _ => ""
}

cases0 : List((N1, U64, R0))
cases0 = [
    (N1.A1(Rec2.Leaf2), 0, Arm0),
    (N1.A1(Rec2.Leaf2), 1, Arm0),
    (N1.C1(D("a"), Rec3.Nil3), 0, Arm0),
    (N1.C1(D("a"), Rec3.Nil3), 1, Arm0),
    (N1.A1(Rec2.Node2(Rec2.Leaf2, "")), 1, Arm1(Rec2.Node2(Rec2.Leaf2, ""))),
    (N1.C1(B(False), Rec3.One3(2)), 0, Arm3(N1.C1(B(False), Rec3.One3(2)))),
    (N1.C1(B(False), Rec3.One3(2)), 1, Arm3(N1.C1(B(False), Rec3.One3(2)))),
    (N1.C1(B(True), Rec3.Two3(Rec3.One3(9223372036854775807), Rec3.One3(-1))), 1, Arm3(N1.C1(B(True), Rec3.Two3(Rec3.One3(9223372036854775807), Rec3.One3(-1))))),
    (N1.A1(Rec2.Node2(Rec2.Node2(Rec2.Leaf2, "ab"), "a")), 0, Arm2),
    (N1.A1(Rec2.Node2(Rec2.Node2(Rec2.Leaf2, "ab"), "a")), 1, Arm1(Rec2.Node2(Rec2.Node2(Rec2.Leaf2, "ab"), "a"))),
    (N1.B1, 0, Arm3(N1.B1)),
    (N1.A1(Rec2.Node2(Rec2.Node2(Rec2.Leaf2, "a"), "a")), 0, Arm2),
    (N1.A1(Rec2.Node2(Rec2.Node2(Rec2.Leaf2, "a"), "a")), 1, Arm1(Rec2.Node2(Rec2.Node2(Rec2.Leaf2, "a"), "a"))),
    (N1.C1(D("a"), Rec3.One3(-1)), 0, Arm3(N1.C1(D("a"), Rec3.One3(-1)))),
    (N1.C1(B(True), Rec3.Two3(Rec3.One3(9223372036854775807), Rec3.Two3(Rec3.One3(2), Rec3.One3(2)))), 1, Arm3(N1.C1(B(True), Rec3.Two3(Rec3.One3(9223372036854775807), Rec3.Two3(Rec3.One3(2), Rec3.One3(2)))))),
]

check0 : U64 -> Str
check0 = |z| {
    var $out = ""
    var $i = 0.U64
    for (v, k, want) in cases0 {
        got = f0(v, k + z)
        $out = if got == want $out else Str.concat($out, "u0 case ${$i.to_str()} arm ${arm0(got).to_str()}${diff0(got, want)}\n")
        $i = $i + 1
    }
    $out
}

R1 : [Arm0, Arm1, Arm2, Arm3, Arm4]

f1 : Str, U64 -> R1
f1 = |x, k| match x {
    "" => Arm0
    "/${_}/${_}/" | "/${_}a" | "${_}/" => Arm1
    "/${_}.b" if k == 0 => Arm2
    "/${_}.b${_}a" | "a" | "${_}a${_}" => Arm3
    _ => Arm4
}

arm1 : R1 -> U64
arm1 = |r| match r {
    Arm0 => 0
    Arm1 => 1
    Arm2 => 2
    Arm3 => 3
    Arm4 => 4
}

diff1 : R1, R1 -> Str
diff1 = |g, w| match (g, w) {
    _ => ""
}

cases1 : List((Str, U64, R1))
cases1 = [
    ("", 1, Arm0),
    ("/b///", 0, Arm4),
    ("/bb///", 0, Arm4),
    ("/ba", 0, Arm1),
    ("//aa", 0, Arm3),
    ("b/", 0, Arm1),
    ("/", 1, Arm1),
    ("/aa.b", 1, Arm3),
    ("/aba.b", 0, Arm2),
    ("/b.b.ba", 1, Arm1),
    ("/.ba", 0, Arm1),
    ("/.ba", 1, Arm1),
    ("a", 1, Arm3),
    ("aaxb", 1, Arm3),
    ("aaaaba", 1, Arm3),
    ("ab", 0, Arm3),
    ("é/b", 0, Arm4),
]

check1 : U64 -> Str
check1 = |z| {
    var $out = ""
    var $i = 0.U64
    for (v, k, want) in cases1 {
        got = f1(v, k + z)
        $out = if got == want $out else Str.concat($out, "u1 case ${$i.to_str()} arm ${arm1(got).to_str()}${diff1(got, want)}\n")
        $i = $i + 1
    }
    $out
}

R2 : [Arm0(Str), Arm1, Arm2(Str), Arm3(Str)]

f2 : Str, U64 -> R2
f2 = |x, _k| match x {
    "${s11}.b" => Arm0(s11)
    "ab${_}.b${_}a" | "${_}/" | "é" => Arm1
    "/${s14}/" => Arm2(s14)
    x15 => Arm3(x15)
}

arm2 : R2 -> U64
arm2 = |r| match r {
    Arm0(_) => 0
    Arm1 => 1
    Arm2(_) => 2
    Arm3(_) => 3
}

diff2 : R2, R2 -> Str
diff2 = |g, w| match (g, w) {
    (Arm0(g0), Arm0(w0)) => (if g0 == w0 "" else " s11")
    (Arm2(g0), Arm2(w0)) => (if g0 == w0 "" else " s14")
    (Arm3(g0), Arm3(w0)) => (if g0 == w0 "" else " x15")
    _ => ""
}

cases2 : List((Str, U64, R2))
cases2 = [
    ("aba.b", 0, Arm0("aba")),
    ("aba.b", 1, Arm0("aba")),
    ("é.b", 0, Arm0("é")),
    ("é.b", 1, Arm0("é")),
    ("ab.b/a", 1, Arm1),
    ("ababé.ba", 1, Arm1),
    ("b/", 0, Arm1),
    ("b/", 1, Arm1),
    ("ab/", 1, Arm1),
    ("é", 1, Arm1),
    ("/a//", 0, Arm3("/a//")),
    ("/a//", 1, Arm3("/a//")),
    ("//é/", 0, Arm3("//é/")),
    ("//é/", 1, Arm3("//é/")),
    ("é/b", 0, Arm3("é/b")),
    ("é/b", 1, Arm3("é/b")),
    ("ab", 1, Arm3("ab")),
    ("a", 0, Arm3("a")),
    ("a", 1, Arm3("a")),
    ("", 0, Arm3("")),
]

check2 : U64 -> Str
check2 = |z| {
    var $out = ""
    var $i = 0.U64
    for (v, k, want) in cases2 {
        got = f2(v, k + z)
        $out = if got == want $out else Str.concat($out, "u2 case ${$i.to_str()} arm ${arm2(got).to_str()}${diff2(got, want)}\n")
        $i = $i + 1
    }
    $out
}

main! = |args| {
    z = args.len() * 0
    echo!(Str.concat(check0(z), Str.concat(check1(z), Str.concat(check2(z), "done"))))
    Ok({})
}
