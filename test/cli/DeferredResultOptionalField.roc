# A nested annotated function returns a binder destructured out of a record
# the enclosing function owns, and a later statement of the enclosing
# function makes that field optional. The body's relation to its annotation
# waits for the enclosing function's boundary (design.md "Row Subsumption"),
# where the field's kind is judged, so the binder the body returns is the
# `Try` the annotation declares.
DeferredResultOptionalField := {}

optional : { step ?: () -> [A, B] } -> U64
optional = |_| 1

optional_later : { step : () -> [A, B] } -> Try(() -> [A, B], [MissingField])
optional_later = |s| {
	r = { step: s.step }
	inner : {} -> Try(() -> [A, B], [MissingField])
	inner = |_| {
		{ step } = r
		step
	}
	_ = optional(r)
	inner({})
}

expect (match optional_later({ step: || B }) { Ok(step) => step() == B, Err(MissingField) => Bool.False })
