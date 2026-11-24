platform "glue-single-tag-multi-payload"
	requires {
		step : U64 -> U64
	}
	exposes [Shapes]
	packages {}
	provides {
		"roc_step": step_for_host,
		"take_holders": take_holders,
		"take_nominals": take_nominals,
		"take_record_first": take_record_first,
		"take_steps": take_steps,
	}
	targets: {}

import Shapes

step_for_host : U64 -> [Stepped(U64, List(U64))]
step_for_host = |n| Stepped((step)(n), [n])

take_steps : [Stepped(U64, List(U64))], [Stepped(U8, Str)] -> U64
take_steps = |_, _| 0

take_nominals : Shapes.Steps, Shapes.Pair(U8), Shapes.Pair(Str) -> U64
take_nominals = |_, _, _| 0

take_holders : { step : [Stepped(U64, List(U64))], count : U8 }, List([Stepped(U64, List(U64))]), Box([Stepped(U64, List(U64))]), [Held([Stepped(U64, List(U64))]), Missing] -> U64
take_holders = |_, _, _, _| 0

take_record_first : [Wrapped({ a : U64 }, U8)] -> U64
take_record_first = |_| 0
