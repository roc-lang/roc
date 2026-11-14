platform "glue-generic-nominal-shapes"
	requires {
		unused : {} -> {}
	}
	exposes [Types]
	packages {}
	provides {
		"take_wraps": take_wraps,
		"take_wraps_again": take_wraps_again,
		"take_pairs": take_pairs,
		"take_unions": take_unions,
		"take_outers": take_outers,
		"take_deep": take_deep,
	}
	targets: {}

import Types

take_wraps : Box(Types.Wrap(U64)), Types.Wrap(Str) -> U64
take_wraps = |_, _| 0

take_wraps_again : Types.Wrap(U64), List(Types.Wrap(Str)) -> Types.Wrap(U64)
take_wraps_again = |wrap, _| wrap

take_pairs : Types.Pair(U8, Str), Types.Pair(Str, U8), Types.Pair(Types.Wrap(U64), Types.Wrap(Str)) -> U64
take_pairs = |_, _, _| 0

take_unions : Types.Opt(U64), Types.Opt(Str), Types.Tree(U64), Types.Tree(Str) -> U64
take_unions = |_, _, _, _| 0

take_outers : Types.Outer(U64), Types.Outer(Str), Types.Wrap(Types.Wrap(U8)) -> U64
take_outers = |_, _, _| 0

take_deep : Types.Wrap(Types.Opt(U64)), Types.Wrap(Types.Opt(I64)), { w : Types.Wrap(U64) }, { w : Types.Wrap(Str) } -> U64
take_deep = |_, _, _, _| 0
