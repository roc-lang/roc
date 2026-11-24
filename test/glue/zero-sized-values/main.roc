platform "glue-zero-sized-values"
	requires {
		unused : {} -> {}
	}
	exposes [Host]
	packages {}
	provides {
		"make_hollow": make_hollow,
		"make_ready": make_ready,
		"take_args": take_args,
		"take_fields": take_fields,
		"take_half": take_half,
		"take_heap": take_heap,
		"take_only_zero_sized": take_only_zero_sized,
	}
	hosted {
		"host_args": Host.args!,
		"host_ready": Host.ready!,
		"host_record": Host.record!,
	}
	targets: {}

import Host

make_hollow : U8 -> [Hollow({}, {})]
make_hollow = |_| Hollow({}, {})

make_ready : U8 -> [Ready]
make_ready = |_| Ready

take_args : U8, [Ready], U16, [Hollow({}, {})], { z : {} }, U32 -> U64
take_args = |_, _, _, _, _, _| 0

take_fields : { a : {}, b : Str }, [A({}, Str), B], { r : [Ready], n : U8, inner : { z : {} } } -> U64
take_fields = |_, _, _| 0

take_half : [Half({}, Str)] -> [Half({}, Str)]
take_half = |half| half

take_heap : List([Ready]), Box({ z : {} }), List({ a : {}, b : Str }) -> U64
take_heap = |_, _, _| 0

take_only_zero_sized : [Ready], {}, Host.Empty -> U64
take_only_zero_sized = |_, _, _| 0
