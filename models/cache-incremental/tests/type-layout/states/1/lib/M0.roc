import Rec
import Tag

M0 :: [].{
	nt5 : g, U64, Bool, Rec -> Bool
	nt5 = |g1, n2, acc3, a4| if (n2 == 0.U64) ("zz" == "") else ({
		r6 : Bool
		r6 = M0.nt5(g1, n2 - 1.U64, ({
			v7 : Tag
			v7 = Tag.W
			"ok"
		}) == (a4.b), a4)
		"" == "xy"
	})

	mk8 : (I64 -> I64) -> (I64 -> I64)
	mk8 = |a9| |p10| (-3.I64)

	t14 : U64, Bool, List(I64) -> Bool
	t14 = |n11, acc12, a13| if (n11 == 0.U64) ({
		v17 : Bool
		v17 = acc12
		v17
	}) else M0.t15(n11 - 1.U64, acc12, a13)

	t15 : U64, Bool, List(I64) -> Bool
	t15 = |n11, acc12, a13| if (n11 == 0.U64) (Bool.False or Bool.False) else M0.t16(n11 - 1.U64, acc12, a13)

	t16 : U64, Bool, List(I64) -> Bool
	t16 = |n11, acc12, a13| if (n11 == 0.U64) Bool.False else M0.t14(n11 - 1.U64, acc12 and Bool.False, a13)
}

expect (match ({
		tm1 : Tag
		tm1 = Tag.P(10.I64)
		tm1
	}) {
	P(n20) => Bool.True
	Q(s21) => Bool.True
	_ => Bool.False
}) == Bool.True
