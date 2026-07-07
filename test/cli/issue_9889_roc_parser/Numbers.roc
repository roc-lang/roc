# Regression fixture for https://github.com/roc-lang/roc/issues/9889 —
# roc-parser's examples/numbers.roc crashed in monotype_lifted after PR #9848.
# Parser.roc/String.roc/CSV.roc are vendored from
# https://github.com/lukewilliamboswell/roc-parser (UPL-1.0) at commit
# cb0982192d2a1091ec1774a8c8e21a6864909d7a; the app example is converted to a
# platform-less module so `roc test --no-cache` exercises the same
# specializations without network access.
import Parser
import String

main! = |_args| Ok({})

# Parse a number followed by a newline
single_number : Parser(List(U8), U64)
single_number =
	Parser.const(|n| n)
		.keep(String.digits)
		.skip(String.string("\n"))

expect {
	actual = String.parse_str(single_number, "1000\n")
	actual == Ok(1000)
}

# Parse a series of numbers followed by a newline
multiple_numbers : Parser(List(U8), List(U64))
multiple_numbers =
	Parser.const(|ns| ns)
		.keep(single_number.many())
		.skip(String.string("\n"))

expect {
	actual = String.parse_str(multiple_numbers, "1000\n2000\n3000\n\n")
	actual == Ok([1000, 2000, 3000])
}

# Sum up the lists and return the largest sum
largest : List(List(U64)) -> U64
largest = |numbers|
	numbers
		.map(List.sum)
		.sort_with(|a, b| if a < b GT else if b > a LT else EQ)
		.first()
		?? 0

expect largest([[1000, 2000, 3000], [4000], [5000, 6000]]) == 11_000

# The original example's main! pipeline: parse groups of numbers and take the
# largest group sum. This drives the `.many()` specializations that crashed.
expect {
	result : Try(List(List(U64)), [ParsingFailure(Str), ParsingIncomplete(Str)])
	result = String.parse_str(multiple_numbers.many(), "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n")
	result.map_ok(largest) == Ok(11_000)
}
