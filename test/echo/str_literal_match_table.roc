# Matching a Str against tables of string literals: literals of every length
# from 0 to 17 bytes and of 30 bytes, literals that differ only in their
# first, a middle, or their last byte, and literals that are prefixes of one
# another, tried with small strings, heap strings and slices of heap strings.
# spellchecker:off
keyword : Str -> Str
keyword = |name| match name {
	"" => "0"
	"a" => "1a"
	"b" => "1b"
	"if" => "2a"
	"in" => "2b"
	"for" => "3a"
	"fox" => "3b"
	"aor" => "3c"
	"else" => "4a"
	"elsa" => "4b"
	"while" => "5a"
	"where" => "5b"
	"whilf" => "5c"
	"return" => "6a"
	"returm" => "6b"
	"default" => "7a"
	"defaulx" => "7b"
	"dexault" => "7c"
	"abstract" => "8a"
	"abstracu" => "8b"
	"continues" => "9a"
	"continued" => "9b"
	"xontinued" => "9c"
	"0123456789" => "10"
	"0123456789a" => "11"
	"0123456789ab" => "12"
	"0123456789abc" => "13"
	"0123456789abcd" => "14"
	"0123456789abcde" => "15a"
	"0123456x89abcde" => "15b"
	"0123456789abcdef" => "16a"
	"0123456789abcdeg" => "16b"
	"0123456789abcdefg" => "17a"
	"0123456789abcdefh" => "17b"
	"1123456789abcdefh" => "17c"
	"01234567x9abcdefh" => "17d"
	"the quick brown fox jumps over" => "30a"
	"the quick brown fox jumps ovex" => "30b"
	"the quick brown fox jumqs over" => "30c"
	"xhe quick brown fox jumps over" => "30d"
	_ => "-"
}

# Literal arms between pattern arms keep their place in arm order.
mixed : Str -> Str
mixed = |name| match name {
	"alpha" => "A"
	"al${rest}" => "al:${rest}"
	"beta" => "B"
	"bet" => "b"
	"${head}!" => "bang:${head}"
	"gamma" => "G"
	"delta" => "D"
	"" => "empty"
	_ => "-"
}

single : Str -> Str
single = |name| match name {
	"only" => "yes"
	_ => "no"
}

main! = |args| {
	# Appended to every probe so that no probe is known before the program runs.
	tail = if (args.len() > 100) "?" else ""
	padding = "padding that is long enough to live on the heap, "

	probes = [
		"",
		"a",
		"b",
		"c",
		"if",
		"in",
		"is",
		"for",
		"fox",
		"aor",
		"foo",
		"fo",
		"else",
		"elsa",
		"elsb",
		"while",
		"where",
		"whilf",
		"whale",
		"return",
		"returm",
		"returx",
		"default",
		"defaulx",
		"dexault",
		"defaul",
		"abstract",
		"abstracu",
		"abstracv",
		"bbstract",
		"continues",
		"continued",
		"xontinued",
		"continuee",
		"continue",
		"0123456789",
		"0123456789a",
		"0123456789ab",
		"0123456789abc",
		"0123456789abcd",
		"0123456789abcde",
		"0123456x89abcde",
		"0123456y89abcde",
		"0123456789abcdef",
		"0123456789abcdeg",
		"0123456789abcdeh",
		"0123456789abcdefg",
		"0123456789abcdefh",
		"1123456789abcdefh",
		"01234567x9abcdefh",
		"01234567y9abcdefh",
		"0123456789abcdefgh",
		"the quick brown fox jumps over",
		"the quick brown fox jumps ovex",
		"the quick brown fox jumqs over",
		"xhe quick brown fox jumps over",
		"the quick brown fox jumps ovey",
		"the quick brown fox jumps over ",
		"the quick brown fox jumps ove",
	]

	var $whole = ""
	var $sliced = ""
	for probe in probes {
		runtime = "${probe}${tail}"
		$whole = "${$whole}${keyword(runtime)} "
		$sliced = "${$sliced}${keyword(Str.drop_prefix("${padding}${runtime}", padding))} "
	}
	echo!($whole)
	echo!(if ($whole == $sliced) "slices agree" else "slices differ: ${$sliced}")

	var $mixed = ""
	for probe in ["alpha", "alto", "al", "a", "beta", "bet", "beta!", "be", "gamma", "gamma!", "delta", "!", "", "x"] {
		$mixed = "${$mixed}${mixed("${probe}${tail}")} "
	}
	echo!($mixed)

	echo!("${single("only${tail}")} ${single("onlx${tail}")} ${single("only!${tail}")} ${single("onl${tail}")} ${single(tail)}")
	Ok({})
}
# spellchecker:on
