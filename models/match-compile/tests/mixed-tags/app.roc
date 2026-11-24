N1 :: [A0(I64, I64), B0(Str, U8)].{ is_eq : _ }

R0 : [Arm0(U8)]

f0 : U8, U64 -> R0
f0 = |x, _k| match x {
    x1 => Arm0(x1)
}

arm0 : R0 -> U64
arm0 = |r| match r {
    Arm0(_) => 0
}

cases0 : List((U8, U64, R0))
cases0 = [
    (255, 1, Arm0(255)),
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

R1 : [Arm0(I64, I64), Arm1(N1), Arm2, Arm3]

f1 : N1, U64 -> R1
f1 = |x, k| match x {
    A0(x3, x4) => Arm0(x3, x4)
    B0("", 1) as w5 if k == 1 => Arm1(w5)
    N1.A0(_, -3) => Arm2
    B0(_, _) => Arm3
}

arm1 : R1 -> U64
arm1 = |r| match r {
    Arm0(_, _) => 0
    Arm1(_) => 1
    Arm2 => 2
    Arm3 => 3
}

cases1 : List((N1, U64, R1))
cases1 = [
    (N1.A0(-1, (-9223372036854775807 - 1)), 0, Arm0(-1, (-9223372036854775807 - 1))),
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
