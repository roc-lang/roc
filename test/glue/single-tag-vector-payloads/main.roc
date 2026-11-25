platform "glue-single-tag-vector-payloads"
	requires {
		unused : {} -> {}
	}
	exposes []
	packages {}
	provides {
		"take_lanes": take_lanes,
		"take_mixed": take_mixed,
		"take_nested_lanes": take_nested_lanes,
	}
	targets: {}

take_lanes : [Lanes(U8x16, U8x16)] -> U64
take_lanes = |_| 0

take_nested_lanes : { inner : [Lanes(U8x16, U8x16)], last : U8x16 } -> U64
take_nested_lanes = |_| 0

take_mixed : [Mixed(U8x16, U64)] -> U64
take_mixed = |_| 0
