## Shared target-independent Roc name operations for glue scripts.
##
## Target generators are still responsible for language-specific escaping and
## reserved words, but splitting dotted names, dropping bangs, and ASCII case
## conversion should be consistent across generated languages.
RocName := { raw : Str }.{
	from_str : Str -> RocName
	from_str = |raw| RocName.{ raw }

	raw : RocName -> Str
	raw = |name| name.raw

	replace_all : Str, Str, Str -> Str
	replace_all = |s, from, to| {
		parts = Str.split_on(s, from)
		Str.join_with(parts, to)
	}

	without_bang : RocName -> Str
	without_bang = |name| replace_all(name.raw, "!", "")

	to_pascal : RocName -> Str
	to_pascal = |name| {
		parts = Str.split_on(name.raw, ".")

		var $result = ""
		for part in parts {
			capitalized = part
				->replace_all("!", "")
				->capitalize_first()
			$result = Str.concat($result, capitalized)
		}

		$result
	}

	to_camel : RocName -> Str
	to_camel = |name| {
		parts = Str.split_on(name.raw, ".")

		var $result = ""
		var $first = Bool.True
		for part in parts {
			cleaned = replace_all(part, "!", "")
			if $first {
				$result = Str.concat($result, lowercase_first(cleaned))
				$first = Bool.False
			} else {
				$result = Str.concat($result, capitalize_first(cleaned))
			}
		}

		$result
	}

	to_lower_snake : RocName -> Str
	to_lower_snake = |name|
		name.raw
			->replace_all(".", "_")
			->replace_all("!", "")
			->lower_snake_ascii()

	to_screaming_snake : RocName -> Str
	to_screaming_snake = |name|
		name.raw
			->replace_all(".", "_")
			->replace_all("!", "")
			->screaming_snake_ascii()

	to_field_name : RocName -> Str
	to_field_name = |name|
		name.raw
			->replace_all(".", "_")
			->replace_all("!", "")

	capitalize_first : Str -> Str
	capitalize_first = |s| {
		bytes = Str.to_utf8(s)
		if List.is_empty(bytes) {
			return ""
		}

		first = match List.first(bytes) {
			Ok(b) => b
			Err(_) => 0
		}
		first_is_lower = first >= 'a' and first <= 'z'
		new_first = if first_is_lower to_uppercase(first) else first
		rest = List.drop_first(bytes, 1)
		new_bytes = List.concat([new_first], rest)
		utf8_or_crash(new_bytes, "capitalize")
	}

	lowercase_first : Str -> Str
	lowercase_first = |s| {
		bytes = Str.to_utf8(s)
		if List.is_empty(bytes) {
			return ""
		}

		first = match List.first(bytes) {
			Ok(b) => b
			Err(_) => 0
		}
		first_is_upper = first >= 'A' and first <= 'Z'
		new_first = if first_is_upper to_lowercase(first) else first
		rest = List.drop_first(bytes, 1)
		new_bytes = List.concat([new_first], rest)
		utf8_or_crash(new_bytes, "lowercase")
	}

	lower_snake_ascii : Str -> Str
	lower_snake_ascii = |s| {
		bytes = Str.to_utf8(s)
		var $output = []
		var $prev_was_lower = Bool.False

		for byte in bytes {
			is_upper = byte >= 'A' and byte <= 'Z'
			is_lower = byte >= 'a' and byte <= 'z'

			new_byte = if is_upper to_lowercase(byte) else byte

			if is_upper and $prev_was_lower {
				$output = $output.append('_')
			}
			$output = $output.append(new_byte)
			$prev_was_lower = is_lower
		}

		utf8_or_crash($output, "lower snake")
	}

	screaming_snake_ascii : Str -> Str
	screaming_snake_ascii = |s| {
		bytes = Str.to_utf8(s)
		var $output = []
		var $prev_was_lower = Bool.False

		for byte in bytes {
			is_upper = byte >= 'A' and byte <= 'Z'
			is_lower = byte >= 'a' and byte <= 'z'

			if is_upper and $prev_was_lower {
				$output = $output.append('_')
			}

			new_byte = if is_lower to_uppercase(byte) else byte
			$output = $output.append(new_byte)

			$prev_was_lower = is_lower
		}

		utf8_or_crash($output, "screaming snake")
	}

	utf8_or_crash : List(U8), Str -> Str
	utf8_or_crash = |bytes, _context|
		match Str.from_utf8(bytes) {
			Ok(str) => str
			Err(_) => {
				crash "glue invariant violated: invalid UTF-8 while transforming Roc name"
			}
		}

	to_uppercase : U8 -> U8
	to_uppercase = |ch| ch - 32

	to_lowercase : U8 -> U8
	to_lowercase = |ch| ch + 32
}

expect RocName.to_pascal(RocName.from_str("Stdout.line!")) == "StdoutLine"
expect RocName.to_pascal(RocName.from_str("Foo.bar.baz!")) == "FooBarBaz"
expect RocName.to_lower_snake(RocName.from_str("PartDef.Idx.get!")) == "part_def_idx_get"
expect RocName.to_screaming_snake(RocName.from_str("Foo.barBaz!")) == "FOO_BAR_BAZ"
expect RocName.to_camel(RocName.from_str("PartDef.Idx.get!")) == "partDefIdxGet"
expect RocName.to_field_name(RocName.from_str("Stdout.line!")) == "Stdout_line"
