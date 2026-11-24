import lib.M0
import lib.Rec
import lib.Tag

M3 :: [].{
	t6 : g, U64, Tag, Rec, Tag -> Tag
	t6 = |g1, n2, acc3, a4, a5| if (n2 == 0.U64) (Tag.P(100.I64)) else (if (Bool.False and Bool.False) M3.t7(g1, n2 - 1.U64, a5, a4, a5) else M3.t6(g1, n2 - 1.U64, Tag.W, a4, a5))

	t7 : g, U64, Tag, Rec, Tag -> Tag
	t7 = |g1, n2, acc3, a4, a5| if (n2 == 0.U64) a5 else (if ("ok" == "") M3.t6(g1, n2 - 1.U64, Tag.Q("q r"), a4, a5) else M3.t7(g1, n2 - 1.U64, Tag.P(0.I64), a4, a5))

	h29 : (I64 -> I64) -> I64
	h29 = |fn28| match ({
			tm2 : Tag
			tm2 = Tag.W
			tm2
		}) {
		P(n30) => M0.t19(fn28, 0.U64, 1.I64, List.repeat(0.I64, 0))
		Q(s31) => if Bool.True 7.I64 else 10.I64
		_ => I64.rem_by(100.I64, 2.I64)
	}
}
