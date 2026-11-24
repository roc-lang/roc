app [main!] { lib: "./lib/main.roc" }

import lib.M0
import M1
import M3
import lib.Rec
import lib.Tag

mk10 : I64 -> (I64 -> I64)
mk10 = |a11| |p12| {
	v13 : Str
	v13 = a11.to_str()
	if Bool.False a11 else 3.I64
}

main! = |args| {
	zu = args.len() * 0
	z = zu.to_i64_wrap()
	o0 : Rec
	o0 = M1.k25
	o1 : I64
	o1 = (if ("rc" == "rc") ({
		v40 : Bool
		v40 = Bool.False
		|p41| p41 + ((-1.I64))
	}) else mk10(z))(I64.rem_by({
		v42 : (I64 -> I64)
		v42 = |p43| p43 + 10.I64
		(-3.I64)
	}, 7.I64))
	o2 : Rec
	o2 = match ({
			tm3 : Tag
			tm3 = if (if Bool.False Bool.False else Bool.True) (Tag.P(3.I64)) else M3.t6(Rec.mk(5.I64, "zz"), 6.U64, Tag.Q("hello"), Rec.mk(2.I64, "b"), Tag.P(2.I64))
			tm3
		}) {
		P(n44) => Rec.mk((-8.I64), "rc")
		Q(s45) => Rec.mk(match ({
				tm4 : Tag
				tm4 = Tag.W
				tm4
			}) {
			P(n46) => n46
			Q(s47) => z
			_ => 7.I64
		}, M0.nt36(6.U64 + zu, s45, |p48| p48, Rec.mk(5.I64, "b")))
		_ => match M3.t7([2.I64, (-8.I64), (-3.I64)], 2.U64 + zu, Tag.Q("b"), Rec.mk((-3.I64), "rc"), Tag.P(5.I64)) {
			P(n49) => Rec.mk(z, "zz")
			Q(s50) => M0.nt36(1.U64 + zu, Rec.mk((-1.I64), "rc"), |p51| Rec.mk(1.I64, "a"), Rec.mk(42.I64, "hello"))
			_ => if Bool.True Rec.mk((-1.I64), "rc") else Rec.mk(1.I64, "")
		}
	}
	o3 : Bool
	o3 = Bool.False
	o4 : Tag
	o4 = if M0.nt36(9.U64 + zu, Bool.True, |p52| Bool.True, Rec.mk(42.I64, "q r")) M3.t7(List.repeat(0.I64, 0), 4.U64 + zu, Tag.Q("rc"), Rec.mk((-1.I64), ""), Tag.P(10.I64)) else (Tag.Q("q r"))
	o5 : List(I64)
	o5 = if M0.nt36(zu, Bool.True, |p53| p53, Rec.mk(3.I64, "hello")) (([5.I64]).append(z)) else (match ({
			tm5 : Tag
			tm5 = Tag.W
			tm5
		}) {
		P(n54) => [2.I64, (-1.I64), 10.I64]
		Q(s55) => [3.I64, 10.I64]
		_ => [(-8.I64)]
	})
	o6 : I64
	o6 = M0.k8
	o7 : I64
	o7 = M0.t18(Tag.P(42.I64), 3.U64 + zu, z, [1.I64])
	o8 : I64
	o8 = M3.h29(|p56| p56 + 42.I64)
	o9 : I64
	o9 = M0.k8
	lines = [Str.concat(o0.a.to_str(), Str.concat("/", o0.b)), o1.to_str(), Str.concat(o2.a.to_str(), Str.concat("/", o2.b)), if o3 "T" else "F", match o4 {
		P(k) => Str.concat("P", k.to_str())
		Q(k) => Str.concat("Q", k)
		_ => "W"
	}, Str.join_with(o5.map(|q| q.to_str()), ","), o6.to_str(), o7.to_str(), o8.to_str(), o9.to_str()]
	echo!(Str.concat(Str.join_with(lines, "\n"), "\n"))
	Ok({})
}
