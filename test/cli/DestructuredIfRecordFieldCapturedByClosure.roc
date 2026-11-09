DestructuredIfRecordFieldCapturedByClosure := {}

# repro for https://github.com/roc-lang/roc/issues/12475
# A record is destructured out of an `if`, and one of its fields is passed to
# a function whose closure captures it, then read again afterwards. ARC must
# keep the field alive through its last use and release it exactly once.

make : Str -> { names : List(Str), suffix : Str }
make = |s| { names: [s], suffix: "!" }

suffixed : { names : List(Str), suffix : Str } -> List(Str)
suffixed = |rec| rec.names.map(|n| Str.concat(n, rec.suffix))

page : Str -> Str
page = |input| {
	{ flag, rec } =
		if input == "" {
			{ flag: Bool.False, rec: make("") }
		} else {
			{ flag: Bool.True, rec: make(input) }
		}
	out = suffixed(rec)
	Str.inspect({ flag, suffix: rec.suffix, out })
}

expect page("a") == "{ flag: True, out: [\"a!\"], suffix: \"!\" }"

page_branch : Str -> Str
page_branch = |input| {
	{ flag, rec } =
		if input == "" {
			{ flag: Bool.False, rec: make("") }
		} else {
			{ flag: Bool.True, rec: make(input) }
		}
	out = suffixed(rec)
	if flag {
		Str.inspect({ suffix: rec.suffix, out })
	} else {
		Str.inspect(out)
	}
}

expect page_branch("a") == "{ out: [\"a!\"], suffix: \"!\" }"

page_boxed : Str -> Str
page_boxed = |input| {
	{ flag, rec } =
		if input == "" {
			{ flag: Bool.False, rec: make("") }
		} else {
			{ flag: Bool.True, rec: make(input) }
		}
	boxed = Box.box(rec)
	Str.inspect({ flag, suffix: rec.suffix, n: Box.unbox(boxed).names })
}

expect page_boxed("a") == "{ flag: True, n: [\"a\"], suffix: \"!\" }"
