Crate :: { n : U64 }.{
	new : U64 -> Crate
	new = |n| { n: n }

	get : Crate, U64 -> U64
	get = |c, k| c.n + k
}
