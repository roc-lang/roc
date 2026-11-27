W1 := Str.{ is_eq : _ }
N2 :: [A2(Rec3, (Str, U8, U64))].{ is_eq : _ }
Rec3 :: [Nil3, One3([A, B]), Two3(Rec3, Rec3)].{ is_eq : _ }

R0 : [Arm0, Arm1, Arm2(W1), Arm3]

f0 : W1, U64 -> R0
f0 = |x, k| match x {
    W1.("/${_}a") | W1.("") => Arm0
    W1.("ab") | _ => Arm1
    y3 | y3 if k == 1 => Arm2(y3)
    _ => Arm3
}

arm0 : R0 -> U64
arm0 = |r| match r {
    Arm0 => 0
    Arm1 => 1
    Arm2(_) => 2
    Arm3 => 3
}

cases0 : List((W1, U64, R0))
cases0 = [
    (W1.("/éaa"), 1, Arm1),
    (W1.("/a"), 1, Arm0),
    (W1.(""), 0, Arm0),
    (W1.("ab"), 0, Arm1),
    (W1.("ab"), 1, Arm1),
    (W1.("a"), 0, Arm1),
    (W1.("a"), 1, Arm1),
    (W1.("é/b"), 0, Arm1),
    (W1.("é/b"), 1, Arm1),
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

R1 : [Arm0(Str, U8, U64), Arm1, Arm2(List(N2), (Str, U8, U64), [A, B], Str, U8, U64)]

f1 : List(N2), U64 -> R1
f1 = |x, _k| match x {
    [N2.A2(One3(_), (x4, x5, x6))] => Arm0(x4, x5, x6)
    [A2(Two3(_, _), _)] | [N2.A2(Nil3, _), N2.A2(_, (_, _, _)), ..] | [..] => Arm1
    [.. as rest20, A2(One3(x15), (x16, x17, x18) as w19)] => Arm2(rest20, w19, x15, x16, x17, x18)
}

arm1 : R1 -> U64
arm1 = |r| match r {
    Arm0(_, _, _) => 0
    Arm1 => 1
    Arm2(_, _, _, _, _, _) => 2
}

cases1 : List((List(N2), U64, R1))
cases1 = [
    ([N2.A2(Rec3.One3(B), ("ab", 0, 0))], 0, Arm0("ab", 0, 0)),
    ([N2.A2(Rec3.One3(B), ("ab", 0, 0))], 1, Arm0("ab", 0, 0)),
    ([N2.A2(Rec3.One3(A), ("a", 0, 1))], 0, Arm0("a", 0, 1)),
    ([N2.A2(Rec3.Two3(Rec3.One3(B), Rec3.Nil3), ("", 1, 18446744073709551615))], 0, Arm1),
    ([N2.A2(Rec3.Two3(Rec3.One3(B), Rec3.Nil3), ("", 1, 18446744073709551615))], 1, Arm1),
    ([N2.A2(Rec3.Two3(Rec3.One3(A), Rec3.Nil3), ("", 255, 1))], 0, Arm1),
    ([N2.A2(Rec3.Nil3, ("é/b", 255, 0)), N2.A2(Rec3.Nil3, ("", 2, 0))], 1, Arm1),
    ([N2.A2(Rec3.Nil3, ("a", 1, 2)), N2.A2(Rec3.Two3(Rec3.Nil3, Rec3.Two3(Rec3.One3(A), Rec3.Nil3)), ("a", 255, 1)), N2.A2(Rec3.Nil3, ("é/b", 1, 2))], 0, Arm1),
    ([N2.A2(Rec3.Nil3, ("a", 1, 2)), N2.A2(Rec3.Two3(Rec3.Nil3, Rec3.Two3(Rec3.One3(A), Rec3.Nil3)), ("a", 255, 1)), N2.A2(Rec3.Nil3, ("é/b", 1, 2))], 1, Arm1),
    ([N2.A2(Rec3.Nil3, ("é/b", 1, 1)), N2.A2(Rec3.One3(A), ("a", 1, 1))], 0, Arm1),
    ([N2.A2(Rec3.Nil3, ("é/b", 1, 1)), N2.A2(Rec3.One3(A), ("a", 1, 1))], 1, Arm1),
    ([N2.A2(Rec3.Nil3, ("", 0, 18446744073709551615)), N2.A2(Rec3.Nil3, ("", 2, 0))], 0, Arm1),
    ([N2.A2(Rec3.Nil3, ("", 0, 18446744073709551615)), N2.A2(Rec3.Nil3, ("", 2, 0))], 1, Arm1),
    ([N2.A2(Rec3.One3(A), ("é/b", 1, 2))], 0, Arm0("é/b", 1, 2)),
    ([N2.A2(Rec3.One3(A), ("é/b", 1, 2))], 1, Arm0("é/b", 1, 2)),
    ([N2.A2(Rec3.One3(B), ("", 0, 1))], 1, Arm0("", 0, 1)),
    ([N2.A2(Rec3.Nil3, ("", 1, 0)), N2.A2(Rec3.Two3(Rec3.One3(B), Rec3.One3(B)), ("a", 1, 1)), N2.A2(Rec3.Two3(Rec3.Nil3, Rec3.One3(B)), ("", 0, 2))], 0, Arm1),
    ([N2.A2(Rec3.Nil3, ("", 1, 0)), N2.A2(Rec3.Two3(Rec3.One3(B), Rec3.One3(B)), ("a", 1, 1)), N2.A2(Rec3.Two3(Rec3.Nil3, Rec3.One3(B)), ("", 0, 2))], 1, Arm1),
    ([N2.A2(Rec3.Nil3, ("", 2, 2))], 0, Arm1),
    ([N2.A2(Rec3.Nil3, ("", 2, 2))], 1, Arm1),
    ([N2.A2(Rec3.One3(A), ("", 0, 2)), N2.A2(Rec3.One3(A), ("é/b", 2, 0))], 1, Arm1),
    ([N2.A2(Rec3.Nil3, ("a", 1, 18446744073709551615)), N2.A2(Rec3.Nil3, ("é/b", 255, 18446744073709551615)), N2.A2(Rec3.One3(A), ("ab", 0, 1))], 0, Arm1),
    ([N2.A2(Rec3.Nil3, ("a", 0, 0)), N2.A2(Rec3.Nil3, ("a", 255, 18446744073709551615)), N2.A2(Rec3.Two3(Rec3.One3(A), Rec3.One3(A)), ("é/b", 0, 2)), N2.A2(Rec3.One3(B), ("a", 2, 2))], 0, Arm1),
    ([N2.A2(Rec3.Nil3, ("", 2, 1)), N2.A2(Rec3.One3(A), ("", 1, 18446744073709551615))], 0, Arm1),
    ([N2.A2(Rec3.Nil3, ("", 2, 1)), N2.A2(Rec3.One3(A), ("", 1, 18446744073709551615))], 1, Arm1),
    ([N2.A2(Rec3.Nil3, ("", 255, 2)), N2.A2(Rec3.Two3(Rec3.One3(B), Rec3.Nil3), ("ab", 1, 0))], 0, Arm1),
    ([N2.A2(Rec3.Nil3, ("", 255, 2)), N2.A2(Rec3.Two3(Rec3.One3(B), Rec3.Nil3), ("ab", 1, 0))], 1, Arm1),
    ([], 1, Arm1),
    ([N2.A2(Rec3.Two3(Rec3.Nil3, Rec3.Nil3), ("", 0, 1)), N2.A2(Rec3.One3(A), ("a", 1, 1))], 1, Arm1),
    ([N2.A2(Rec3.One3(B), ("", 1, 18446744073709551615))], 0, Arm0("", 1, 18446744073709551615)),
    ([N2.A2(Rec3.Two3(Rec3.One3(A), Rec3.One3(B)), ("", 2, 0)), N2.A2(Rec3.Two3(Rec3.Nil3, Rec3.One3(B)), ("ab", 0, 0)), N2.A2(Rec3.Two3(Rec3.Nil3, Rec3.One3(B)), ("", 2, 1)), N2.A2(Rec3.One3(B), ("é/b", 0, 1))], 0, Arm1),
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

R2 : [Arm0, Arm1, Arm2, Arm3([A, B((Str, U64, Bool)), C([A(Str), B(Bool)], [A(Str)])]), Arm4, Arm5]

f2 : [A, B((Str, U64, Bool)), C([A(Str), B(Bool)], [A(Str)])], U64 -> R2
f2 = |x, k| match x {
    A => Arm0
    C(B(True), A("é")) | A | B((_, _, True)) if k == 0 => Arm1
    B(_) | B(_) | A => Arm2
    A as w25 if k == 0 => Arm3(w25)
    A => Arm4
    C(_, _) => Arm5
}

arm2 : R2 -> U64
arm2 = |r| match r {
    Arm0 => 0
    Arm1 => 1
    Arm2 => 2
    Arm3(_) => 3
    Arm4 => 4
    Arm5 => 5
}

cases2 : List(([A, B((Str, U64, Bool)), C([A(Str), B(Bool)], [A(Str)])], U64, R2))
cases2 = [
    (A, 0, Arm0),
    (C(B(True), A("é")), 0, Arm1),
    (B(("é/b", 18446744073709551615, True)), 0, Arm1),
    (B(("é/b", 2, True)), 0, Arm1),
    (B(("é", 18446744073709551615, False)), 0, Arm2),
    (B(("é", 18446744073709551615, False)), 1, Arm2),
    (B(("a", 0, False)), 1, Arm2),
    (B(("a", 18446744073709551615, False)), 0, Arm2),
    (B(("a", 18446744073709551615, False)), 1, Arm2),
    (B(("é", 2, True)), 0, Arm1),
    (C(B(False), A("")), 0, Arm5),
    (C(B(False), A("")), 1, Arm5),
    (C(B(False), A("é")), 0, Arm5),
    (C(A(""), A("ab")), 1, Arm5),
    (B(("ab", 18446744073709551615, True)), 0, Arm1),
    (B(("ab", 2, False)), 0, Arm2),
    (B(("é", 1, True)), 0, Arm1),
    (B(("é", 1, True)), 1, Arm2),
    (C(B(True), A("")), 0, Arm5),
    (B(("é/b", 18446744073709551615, False)), 1, Arm2),
    (C(A("é/b"), A("")), 1, Arm5),
    (C(A("é"), A("ab")), 1, Arm5),
]

check2 : U64 -> Str
check2 = |z| {
    var $out = ""
    var $i = 0.U64
    for (v, k, want) in cases2 {
        got = f2(v, k + z)
        $out = if got == want $out else Str.concat($out, "u2 case ${$i.to_str()} arm ${arm2(got).to_str()}\n")
        $i = $i + 1
    }
    $out
}

main! = |args| {
    z = args.len() * 0
    echo!(Str.concat(check0(z), Str.concat(check1(z), Str.concat(check2(z), "done"))))
    Ok({})
}
