BoxyOptionalRecordFields :: [].{}

import BoxyDefaultsDep

Config := { retries : U8 ?? 3, timeout : U8 ?? 10 }

compute : U8 -> U8
compute = |n| n * 3 + 1

BlockConfig := { scaled : U64 ?? {
	base = 2
	base * 5
} }

CallConfig := { count : U8 ?? compute(4) }

NestedInner := { m : U64 ?? {
	seed = 3
	seed + 4
} }

# Lambdas inside defaults need checked nested-function sites published
# under the module's default-expression root (no owning template).
IifeConfig := { bumped : U8 ?? (|n| n + 1)(4) }

LocalFnConfig := { doubled : U8 ?? {
	double : U8 -> U8
	double = |n| n * 2
	double(6)
} }

NestedOuter := { inner : NestedInner ?? {
	x = NestedInner.{}
	x
}, n : U64 ?? 1 }

pick : { value ?: a } -> Try(a, [MissingField])
pick = |record| record.?value

expect {
	present : { age ?: U8 }
	present = { age: 30 }

	missing : { age ?: U8 }
	missing = {}

	(present.?age ?? 0) + (missing.?age ?? 5) == 35
}

expect {
	config : Config
	config = Config.{}

	config.retries + config.timeout == 13
}

expect {
	present : { value ?: Str }
	present = { value: "found" }

	missing : { value ?: Str }
	missing = {}

	if (pick(present) ?? "") == "found" {
		(pick(missing) ?? "fallback") == "fallback"
	} else {
		False
	}
}

expect {
	outer : { inner ?: { value : U8 } }
	outer = { inner: { value: 42 } }

	outer.?inner.value == Ok(42)
}

# A block default with a pattern binding, materialized twice in one
# procedure body: each materialization must get isolated binder state.
expect {
	first : BlockConfig
	first = BlockConfig.{}

	second : BlockConfig
	second = BlockConfig.{}

	first.scaled + second.scaled == 20
}

# A call default whose annotated callee needs a planned worker at the
# construction site.
expect {
	config : CallConfig
	config = CallConfig.{}

	config.count == 13
}

# A foreign module's block default with a pattern binding: the default
# lowers against the declaring module's stores and binder space.
expect {
	dep : BoxyDefaultsDep.BoxyDefaultsDep
	dep = BoxyDefaultsDep.BoxyDefaultsDep.{}

	dep.n == 10
}

# A default whose expression constructs another defaulted nominal: the
# materializations nest, so binder isolation must stack.
expect {
	outer : NestedOuter
	outer = NestedOuter.{}

	outer.inner.m + outer.n == 8
}

# An immediately-invoked lambda default: the lambda's nested-function
# site is owned by the default-expression root, not any template.
expect {
	config : IifeConfig
	config = IifeConfig.{}

	config.bumped == 5
}

# A default binding a local function and calling it: the local lambda's
# default-root site resolves per omitting construction site.
expect {
	config : LocalFnConfig
	config = LocalFnConfig.{}

	config.doubled == 12
}

# An UNANNOTATED (generalized) local function whose body dispatches a
# numeric literal: the literal's descriptor inside the generic worker
# must come from the caller's evidence descriptor argument, not an
# erased static template (which would encode the literal at the Dec
# default and box the wrong payload).
GenFnConfig := { v : U8 ?? {
	plus = |n| n + 5
	plus(6)
} }

# The same wiring class with a fractional literal at F32.
FracFnConfig := { level : F32 ?? {
	bump = |n| n + 0.5
	bump(1.5)
} }

# One generalized default-local function instantiated at two different
# numeric types inside a single default expression.
TwoUseConfig := { ok : Bool ?? {
	add5 = |n| n + 5
	small : U8
	small = add5(6)
	large : U16
	large = add5(600)
	small == 11 and large == 605
} }

expect {
	config : GenFnConfig
	config = GenFnConfig.{}

	config.v == 11
}

expect {
	config : FracFnConfig
	config = FracFnConfig.{}

	config.level == 2.0
}

expect {
	config : TwoUseConfig
	config = TwoUseConfig.{}

	config.ok
}
