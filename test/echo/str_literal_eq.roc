# Comparing a Str with string literals through `==` and `!=`: literals of 0 to
# 65 bytes, on either side of the operator, compared with equal strings, with
# strings that differ in their first or last byte, and with strings one byte
# shorter or longer, held inline, on the heap, and as slices of heap strings.
# spellchecker:off
code : Str -> Str
code = |s|
	if s == "" {
		"0"
	}
	else if "a" == s {
		"1"
	}
	else if s == "ab" {
		"2"
	}
	else if "abc" == s {
		"3"
	}
	else if s == "abcd" {
		"4"
	}
	else if "abcde" == s {
		"5"
	}
	else if s == "abcdef" {
		"6"
	}
	else if "abcdefg" == s {
		"7"
	}
	else if s == "abcdefgh" {
		"8"
	}
	else if "abcdefghi" == s {
		"9"
	}
	else if s == "abcdefghijklmno" {
		"15"
	}
	else if "abcdefghijklmnop" == s {
		"16"
	}
	else if s == "abcdefghijklmnopq" {
		"17"
	}
	else if "abcdefghijklmnopqrstuvwxyz0123" == s {
		"30"
	}
	else if s == "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+-" {
		"64"
	}
	else if "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+-=" == s {
		"65"
	}
	else {
		"-"
	}

# A name bound to a literal compares like the literal, and a reassigned
# variable compares by the value it holds at each comparison.
other : Str -> Str
other = |s| {
	bound = "bound literal"
	var $word = "first"
	a = if s != "abc" {
		"n"
	} else {
		"e"
	}
	b = if s == bound {
		"e"
	} else {
		"n"
	}
	c = if s == $word {
		"e"
	} else {
		"n"
	}
	$word = "second"
	d = if s == $word {
		"e"
	} else {
		"n"
	}
	"${a}${b}${c}${d}"
}

main! = |args| {
	# Appended to every probe so that no probe is known before the program runs.
	tail = if (args.len() > 100) "?" else ""
	padding = "padding that is long enough to live on the heap, "

	probes = [
		"",
		"x",
		"a",
		"ax",
		"",
		"#",
		"#",
		"ab",
		"abx",
		"a",
		"#b",
		"a#",
		"abc",
		"abcx",
		"ab",
		"#bc",
		"ab#",
		"abcd",
		"abcdx",
		"abc",
		"#bcd",
		"abc#",
		"abcde",
		"abcdex",
		"abcd",
		"#bcde",
		"abcd#",
		"abcdef",
		"abcdefx",
		"abcde",
		"#bcdef",
		"abcde#",
		"abcdefg",
		"abcdefgx",
		"abcdef",
		"#bcdefg",
		"abcdef#",
		"abcdefgh",
		"abcdefghx",
		"abcdefg",
		"#bcdefgh",
		"abcdefg#",
		"abcdefghi",
		"abcdefghix",
		"abcdefgh",
		"#bcdefghi",
		"abcdefgh#",
		"abcdefghijklmno",
		"abcdefghijklmnox",
		"abcdefghijklmn",
		"#bcdefghijklmno",
		"abcdefghijklmn#",
		"abcdefghijklmnop",
		"abcdefghijklmnopx",
		"abcdefghijklmno",
		"#bcdefghijklmnop",
		"abcdefghijklmno#",
		"abcdefghijklmnopq",
		"abcdefghijklmnopqx",
		"abcdefghijklmnop",
		"#bcdefghijklmnopq",
		"abcdefghijklmnop#",
		"abcdefghijklmnopqrstuvwxyz0123",
		"abcdefghijklmnopqrstuvwxyz0123x",
		"abcdefghijklmnopqrstuvwxyz012",
		"#bcdefghijklmnopqrstuvwxyz0123",
		"abcdefghijklmnopqrstuvwxyz012#",
		"abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+-",
		"abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+-x",
		"abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+",
		"#bcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+-",
		"abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+#",
		"abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+-=",
		"abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+-=x",
		"abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+-",
		"#bcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+-=",
		"abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+-#",
	]

	var $whole = ""
	var $sliced = ""
	for probe in probes {
		runtime = "${probe}${tail}"
		$whole = "${$whole}${code(runtime)} "
		$sliced = "${$sliced}${code(Str.drop_prefix("${padding}${runtime}", padding))} "
	}
	echo!($whole)
	echo!(if ($whole == $sliced) "slices agree " else "slices differ: ${$sliced}")

	var $others = ""
	for probe in ["abc", "abd", "bound literal", "bound literam", "first", "second", ""] {
		$others = "${$others}${other("${probe}${tail}")} "
	}
	echo!($others)
	Ok({})
}
# spellchecker:on
