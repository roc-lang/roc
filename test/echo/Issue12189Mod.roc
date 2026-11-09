Issue12189Mod :: [].{
	Matrix(w) := { a : (w, w) }

	step : Matrix(w), w -> Matrix(w)
		where [w.plus_wrap : w, w -> w]
	step = |m, x| { a: (m.a.0.plus_wrap(x), m.a.1) }
}
