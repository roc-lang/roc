
R0 : [Arm0(Str), Arm1(Str, Str), Arm2(Str)]

f0 : Str, U64 -> R0
f0 = |x, _k| match x {
    "/${s1}a" => Arm0(s1)
    "${s2}a${s3}/" => Arm1(s2, s3)
    x4 => Arm2(x4)
}

arm0 : R0 -> U64
arm0 = |r| match r {
    Arm0(_) => 0
    Arm1(_, _) => 1
    Arm2(_) => 2
}

diff0 : R0, R0 -> Str
diff0 = |g, w| match (g, w) {
    (Arm0(g0), Arm0(w0)) => (if g0 == w0 "" else " s1")
    (Arm1(g0, g1), Arm1(w0, w1)) => Str.concat((if g0 == w0 "" else " s2"), (if g1 == w1 "" else " s3"))
    (Arm2(g0), Arm2(w0)) => (if g0 == w0 "" else " x4")
    _ => ""
}

cases0 : List((Str, U64, R0))
cases0 = [
    ("/a", 1, Arm0("")),
    ("//a", 1, Arm0("/")),
    ("aa/", 1, Arm1("", "a")),
    ("a/", 1, Arm1("", "")),
    ("", 0, Arm2("")),
    ("é/b", 0, Arm2("é/b")),
    ("é/b", 1, Arm2("é/b")),
    ("ab", 1, Arm2("ab")),
    ("a", 0, Arm2("a")),
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

main! = |args| {
    z = args.len() * 0
    echo!(Str.concat(check0(z), "done"))
    Ok({})
}
