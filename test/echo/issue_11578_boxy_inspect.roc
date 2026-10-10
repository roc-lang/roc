# repro for https://github.com/roc-lang/roc/issues/11578
#
# `Str.inspect` renders every part of a value under every backend, including
# `--specialize=no`: zero-sized values (a payloadless single-tag nominal, `{}`,
# and aggregates of them), scalar nominals with a custom `to_inspect`, opaque
# types, nominal records with unnamed padding, and boxes.

Unit := [U]

Meters := I64.{
    to_inspect : Meters -> Str
    to_inspect = |_m| "meters!"
}

Secret :: I64

Padded := { a : U8, _ : U32, b : Str }

main! = |args| {
    s = args.first() ?? "x"
    m : Meters
    m = Meters.(3)
    sec : Secret
    sec = Secret.(4)
    p : Padded
    p = { a: 1, b: s }
    echo!("${Str.inspect((s, Unit.U, 3.I64))}\n")
    echo!("${Str.inspect((Unit.U, s))}\n")
    echo!("${Str.inspect((s, {}, 3.I64))}\n")
    echo!("${Str.inspect((s, (Unit.U, Unit.U)))}\n")
    echo!("${Str.inspect({ a: s, u: Unit.U })}\n")
    echo!("${Str.inspect([Unit.U, Unit.U])}\n")
    echo!("${Str.inspect([{ u: Unit.U, n: s }])}\n")
    echo!("${Str.inspect(Ok((Unit.U, {})))}\n")
    echo!("${Str.inspect(Pair(Unit.U, s))}\n")
    echo!("${Str.inspect((s, m))}\n")
    echo!("${Str.inspect([m])}\n")
    echo!("${Str.inspect((s, sec))}\n")
    echo!("${Str.inspect({ a: s, sec })}\n")
    echo!("${Str.inspect((s, p))}\n")
    echo!("${Str.inspect(Box.box(Unit.U))}\n")
    echo!("${Str.inspect(Box.box((s, m)))}\n")
    Ok({})
}
