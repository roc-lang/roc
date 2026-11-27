import Base

Mid :: [].{
	run : List(I64), I64 -> { a : I64, b : Str }
	run = |xs, k| {
		f = Base.adder(k)
		ys = xs.map(f)
		{ a: ys.fold(0, |s, x| s + x), b: Str.join_with(ys.map(|q| q.to_str()), ",") }
	}

	sum_to : U64, I64 -> I64
	sum_to = |n, acc| if n == 0 acc else Mid.sum_to(n - 1, acc + n.to_i64_wrap())
}
