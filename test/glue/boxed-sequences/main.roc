platform "glue-boxed-sequences"
	requires {}
	exposes [Sequences]
	packages {}
	provides {
		"roc_iter": echo_iter,
		"roc_stream": echo_stream,
		"roc_wrapped": echo_wrapped,
	}
	hosted {
		"roc_hold_iter": Sequences.hold_iter!,
		"roc_hold_stream": Sequences.hold_stream!,
	}
	targets: {}

import Sequences

echo_iter : Box(Iter(U8)) -> Box(Iter(U8))
echo_iter = |value| value

echo_stream : Box(Stream(U8)) -> Box(Stream(U8))
echo_stream = |value| value

echo_wrapped : Sequences.Wrapped(Iter(U8)) -> Sequences.Wrapped(Iter(U8))
echo_wrapped = |value| value
