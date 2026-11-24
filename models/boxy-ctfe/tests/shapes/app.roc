# A fixed program in the generator's shape: each probe is a top-level constant c<i> and the same expression in the
# untaken-at-compile-time branch of r<i>, printed at compile time, as the restored constant at runtime, and as the
# runtime value. Covers wrapping arithmetic at several widths, a zero-sized payload, a nominal record, Try, a
# recursive nominal built by a tail-recursive helper, and a constant closure capturing a computed value.
Pt := { x : I128, y : Str }
Chain := [More(U8, Chain), Stop]

grow : U64, Chain -> Chain
grow = |n, acc| if n == 0 { acc } else { grow(n - 1, Chain.More(n.to_u8_wrap(), acc)) }

adder : U16 -> (U16 -> U16)
adder = |k| |x| x.plus_wrap(k)

show_rt = |z, v| if z != 0 {
	crash "rt"
} else {
	Str.inspect(v)
}

apply_rt = |z, f, a| if z != 0 {
	crash "rt"
} else {
	Str.inspect(f(a))
}

c0 : { a : U8, b : List(Str), e : [Gone({}), Here(I8)] }
c0 = { a: 200.U8.plus_wrap(100), b: ["x", "y"].map(|s| Str.concat(s, "!")), e: Gone({}) }

r0 : U64 -> { a : U8, b : List(Str), e : [Gone({}), Here(I8)] }
r0 = |z| if z != 0 {
	crash "rt"
} else {
	{ a: 200.U8.plus_wrap(100), b: ["x", "y"].map(|s| Str.concat(s, "!")), e: Gone({}) }
}

c1 : Try(Pt, [Nope])
c1 = Ok(Pt.{ x: 12345678901234567890123.I128.times_wrap(1000000), y: "hi" })

r1 : U64 -> Try(Pt, [Nope])
r1 = |z| if z != 0 {
	crash "rt"
} else {
	Ok(Pt.{ x: 12345678901234567890123.I128.times_wrap(1000000), y: "hi" })
}

c2 : Chain
c2 = grow(4, Chain.Stop)

r2 : U64 -> Chain
r2 = |z| if z != 0 {
	crash "rt"
} else {
	grow(4, Chain.Stop)
}

c3 : U16 -> U16
c3 = adder(65530.U16.plus_wrap(3))

r3 : U64 -> (U16 -> U16)
r3 = |z| if z != 0 {
	crash "rt"
} else {
	adder(65530.U16.plus_wrap(3))
}

main! = |args| {
	z = args.len() * 0
	echo!(Str.concat(Str.concat("p0 ct: ", Str.inspect(c0)), "\n"))
	echo!(Str.concat(Str.concat("p0 rt-const: ", show_rt(z, c0)), "\n"))
	echo!(Str.concat(Str.concat("p0 rt: ", show_rt(z, r0(z))), "\n"))
	echo!(Str.concat(Str.concat("p1 ct: ", Str.inspect(c1)), "\n"))
	echo!(Str.concat(Str.concat("p1 rt-const: ", show_rt(z, c1)), "\n"))
	echo!(Str.concat(Str.concat("p1 rt: ", show_rt(z, r1(z))), "\n"))
	echo!(Str.concat(Str.concat("p2 ct: ", Str.inspect(c2)), "\n"))
	echo!(Str.concat(Str.concat("p2 rt-const: ", show_rt(z, c2)), "\n"))
	echo!(Str.concat(Str.concat("p2 rt: ", show_rt(z, r2(z))), "\n"))
	echo!(Str.concat(Str.concat("p3 ct: ", Str.inspect(c3(10))), "\n"))
	echo!(Str.concat(Str.concat("p3 rt-const: ", apply_rt(z, c3, 10)), "\n"))
	echo!(Str.concat(Str.concat("p3 rt: ", apply_rt(z, r3(z), 10)), "\n"))
	Ok({})
}

expect c0 == r0(0)
