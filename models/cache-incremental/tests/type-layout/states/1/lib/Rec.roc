Rec := { c : I64, a : I64, b : Str }.{
	mk : I64, Str -> Rec
	mk = |a, b| { c: 7.I64, a, b }
}
