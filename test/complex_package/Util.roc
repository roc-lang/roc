## Leaf utility module with no dependencies.
## Provides basic string and list helpers used throughout the package.

Util :: [].{

	## Remove leading and trailing whitespace from every string in a list.
	trim_all : List(Str) -> List(Str)
	trim_all = |strings|
		strings.map(Str.trim)

	## Wrap a string with a prefix and suffix.
	surround : Str, Str, Str -> Str
	surround = |str, prefix, suffix|
		"${prefix}${str}${suffix}"

	## Filter out empty strings from a list.
	non_empty : List(Str) -> List(Str)
	non_empty = |strings|
		strings.keep_if(|s| s != "")

	## Sum a list of integers.
	sum : List(I64) -> I64
	sum = |nums|
		nums.fold(0, |acc, n| acc + n)

	## Join strings with a separator.
	join_with : List(Str), Str -> Str
	join_with = |strings, sep|
		match strings {
			[] => ""
			[first, .. as rest] =>
				rest.fold(first, |acc, s| "${acc}${sep}${s}")
			}
}

# Tests

expect Util.trim_all(["  hi ", " there "]) == ["hi", "there"]
expect Util.trim_all([]) == []

expect Util.surround("hello", "[", "]") == "[hello]"
expect Util.surround("x", "<", "/>") == "<x/>"

expect Util.non_empty(["a", "", "b", "", "c"]) == ["a", "b", "c"]
expect Util.non_empty([""]) == []

expect Util.sum([1, 2, 3, 4]) == 10
expect Util.sum([]) == 0

expect Util.join_with(["a", "b", "c"], ", ") == "a, b, c"
expect Util.join_with([], ", ") == ""
expect Util.join_with(["solo"], "-") == "solo"
