app [main!] { lib: "./lib/main.roc" }

import lib.M0
import lib.Rec
import lib.Tag

main! = |args| {
	zu = args.len() * 0
	z = zu.to_i64_wrap()
	o0 : Tag
	o0 = {
		v18 : Str
		v18 = "q r"
		Tag.P(2.I64)
	}
	o1 : Bool
	o1 = M0.nt5(Tag.P(10.I64), 3.U64 + zu, Bool.False, Rec.mk(10.I64, "hello"))
	o2 : (I64 -> I64)
	o2 = M0.mk8(|p19| p19 + ((-1.I64)))
	o3 : Bool
	o3 = M0.t14(1.U64, Bool.False, [7.I64, 2.I64])
	lines = [match o0 {
		P(k) => Str.concat("P", k.to_str())
		Q(k) => Str.concat("Q", k)
		_ => "W"
	}, if o1 "T" else "F", o2(3.I64).to_str(), if o3 "T" else "F"]
	echo!(Str.concat(Str.join_with(lines, "\n"), "\n"))
	Ok({})
}
