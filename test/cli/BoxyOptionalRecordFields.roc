BoxyOptionalRecordFields :: [].{}

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
	config : { retries : U8 ?? 3, timeout : U8 ?? 10 }
	config = {}

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
