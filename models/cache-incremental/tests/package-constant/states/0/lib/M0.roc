import Rec
import Tag

M0 :: [].{
	k8 : I64
	k8 = (|p9| p9 + 10.I64)(3.I64)

	t18 : g, U64, I64, List(I64) -> I64
	t18 = |g14, n15, acc16, a17| if (n15 == 0.U64) (a17.fold(acc16, |s21, x22| acc16)) else M0.t19(g14, n15 - 1.U64, ({
		v23 : Rec
		v23 = Rec.mk(100.I64, "zz")
		v23
	}).a, a17)

	t19 : g, U64, I64, List(I64) -> I64
	t19 = |g14, n15, acc16, a17| if (n15 == 0.U64) acc16 else (if Bool.True M0.t20(g14, n15 - 1.U64, I64.rem_by((|p24| p24 + 10.I64)(acc16), 7.I64), a17) else M0.t20(g14, n15 - 1.U64, acc16, a17))

	t20 : g, U64, I64, List(I64) -> I64
	t20 = |g14, n15, acc16, a17| if (n15 == 0.U64) (Str.count_utf8_bytes("a").to_i64_wrap() + (if Bool.True acc16 else ((-8.I64)))) else (if Bool.True M0.t18(g14, n15 - 1.U64, a17.sum(), a17) else M0.t19(g14, n15 - 1.U64, acc16, a17))

	nt36 : U64, a, (a -> a), Rec -> a
	nt36 = |n32, acc33, f34, a35| if (n32 == 0.U64) acc33 else ({
		r37 : a
		r37 = M0.nt36(n32 - 1.U64, match ({
				tm1 : Tag
				tm1 = Tag.P((-1.I64))
				tm1
			}) {
			P(n38) => acc33
			Q(s39) => acc33
			_ => acc33
		}, f34, a35)
		acc33
	})
}
