# Literal patterns against each kind of matched type. A literal matched
# against a builtin number or `Str` compares directly; one matched against a
# custom type, or in a function that works for any type, converts the literal
# and compares with that type's `is_eq`.

byte_name : U8 -> Str
byte_name = |byte|
	match byte {
		0 => "zero"
		'Z' => "zed"
		255 => "max"
		_ => "other"
	}

first_name : Try(U8, [Empty]) -> Str
first_name = |found|
	match found {
		Ok('Z') => "ready"
		Ok(7) => "seven"
		Ok(_) => "byte"
		Err(Empty) => "empty"
	}

word_code : Str -> I64
word_code = |word|
	match word {
		"one" => 1
		"two" => 2
		_ => -1
	}

price_name : Dec -> Str
price_name = |price|
	match price {
		1.5 => "cheap"
		_ => "dear"
	}

pair_name : (U16, Str) -> Str
pair_name = |pair|
	match pair {
		(1, "a") => "first"
		(_, "b") => "second"
		_ => "neither"
	}

Level := [Low, High].{
	from_numeral : Numeral -> Try(Level, [InvalidNumeral(Str)])
	from_numeral = |numeral| if numeral.is_negative() Ok(Level.Low) else Ok(Level.High)

	is_eq : Level, Level -> Bool
	is_eq = |a, b|
		match (a, b) {
			(Low, Low) => Bool.True
			(High, High) => Bool.True
			_ => Bool.False
		}
}

level_name : Level -> Str
level_name = |level|
	match level {
		-1 => "low"
		_ => "high"
	}

# Works for any type a `0` converts to and that has `is_eq`.
is_zero = |n|
	match n {
		0 => "yes"
		_ => "no"
	}

main! = |args| {
	# A runtime zero, so the calls below run when the program does.
	zero = args.len().to_u8_wrap() * 0
	bytes = "${byte_name(zero)} ${byte_name(zero + 90)} ${byte_name(zero + 255)} ${byte_name(zero + 3)}"
	firsts = "${first_name(Ok(zero + 90))} ${first_name(Ok(zero + 7))} ${first_name(Ok(zero))} ${first_name(Err(Empty))}"
	words = "${word_code("one").to_str()} ${word_code("two").to_str()} ${word_code("six").to_str()}"
	prices = "${price_name(1.5)} ${price_name(2.5)}"
	pairs = "${pair_name((1, "a"))} ${pair_name((9, "b"))} ${pair_name((1, "c"))}"
	levels = "${level_name(Level.Low)} ${level_name(Level.High)}"
	zeros = "${is_zero(zero)} ${is_zero(zero + 1)} ${is_zero(0.0)} ${is_zero(1.I64)}"
	echo!("${bytes} | ${firsts} | ${words} | ${prices} | ${pairs} | ${levels} | ${zeros}")
	Ok({})
}
