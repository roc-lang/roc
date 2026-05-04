Builtin :: [].{
	Str :: [ProvidedByCompiler].{
		Utf8Problem := [
			InvalidStartByte,
			UnexpectedEndOfSequence,
			ExpectedContinuation,
			OverlongEncoding,
			CodepointTooLarge,
			EncodesSurrogateHalf,
		].{
			is_eq : Utf8Problem, Utf8Problem -> Bool
		}

		is_empty : Str -> Bool
		is_empty = |str| Str.count_utf8_bytes(str) == 0

		## Concatenates two strings together.
		## ```roc
		## expect "ab".concat("cd") == "abcd"
		## expect "hello".concat("") == "hello"
		## expect "".concat("") == ""
		## ```
		concat : Str, Str -> Str

		## Determines whether or not the first Str contains the second.
		## ```roc
		## expect "foobarbaz".contains("bar")
		## expect !"apple".contains("orange")
		## expect "anything".contains("")
		## ```
		contains : Str, Str -> Bool

		## Return the [Str] with all whitespace removed from both the beginning
		## as well as the end.
		## ```roc
		## expect "   Hello      \n\n".trim() == "Hello"
		## ```
		trim : Str -> Str

		## Return the [Str] with all whitespace removed from the beginning.
		## ```roc
		## expect "   Hello      \n\n".trim_start() == "Hello      \n\n"
		## ```
		trim_start : Str -> Str

		## Return the [Str] with all whitespace removed from the end.
		## ```roc
		## expect "   Hello      \n\n".trim_end() == "   Hello"
		## ```
		trim_end : Str -> Str

		## Returns `True` if all the [ASCII characters](https://en.wikipedia.org/wiki/ASCII) in both strings are the same,
		## ignoring differences in capitalization.
		## Non-ASCII characters must all be exactly the same,
		## including capitalization. For example:
		##
		## ```roc
		##  expect "café".caseless_ascii_equals("CAFé")
		##
		##  expect !"café".caseless_ascii_equals("CAFÉ")
		## ```
		##
		## The first call returns `True` because all the ASCII characters are the same
		## when ignoring differences in capitalization, and the only non-ASCII character
		## (`é`) is the same in both strings. The second call returns `False` because
		## `é` and `É` are not ASCII characters, and they are different.
		##
		## This function is useful for things like [command-line flags](https://en.wikipedia.org/wiki/Command-line_interface#Command-line_option)
		## and [environment variable names](https://en.wikipedia.org/wiki/Environment_variable)
		## where you know in advance that you're dealing with a string containing only ASCII characters.
		## It has better performance than lowercasing operations which take Unicode into account.
		##
		## That said, strings received from user input can always contain
		## non-ASCII Unicode characters, and lowercasing [Unicode](https://unicode.org) works
		## differently in different languages. For example, the string `"I"` lowercases to `"i"`
		## in English and to `"ı"` (a [dotless i](https://en.wikipedia.org/wiki/Dotless_I))
		## in Turkish. These rules can also change in each [Unicode release](https://www.unicode.org/releases/),
		## so we have separate [`unicode` package](https://github.com/roc-lang/unicode)
		## for Unicode capitalization that can be upgraded independently from the language's builtins.
		##
		## To convert a string's ASCII characters to uppercase or lowercase, you can use [Str.with_ascii_uppercased]
		## or [Str.with_ascii_lowercased].
		caseless_ascii_equals : Str, Str -> Bool

		## Returns a version of the string with all [ASCII characters](https://en.wikipedia.org/wiki/ASCII) lowercased.
		## Non-ASCII characters are left unmodified. For example:
		##
		## ```roc
		## expect "CALFÉ".with_ascii_lowercased() == "calfÉ"
		## ```
		##
		## This function is useful for things like [command-line flags](https://en.wikipedia.org/wiki/Command-line_interface#Command-line_option)
		## and [environment variable names](https://en.wikipedia.org/wiki/Environment_variable)
		## where you know in advance that you're dealing with a string containing only ASCII characters.
		## It has better performance than lowercasing operations which take Unicode into account.
		##
		## That said, strings received from user input can always contain
		## non-ASCII Unicode characters, and lowercasing [Unicode](https://unicode.org) works
		## differently in different languages. For example, the string `"I"` lowercases to `"i"`
		## in English and to `"ı"` (a [dotless i](https://en.wikipedia.org/wiki/Dotless_I))
		## in Turkish. These rules can also change in each [Unicode release](https://www.unicode.org/releases/),
		## so we have separate [`unicode` package](https://github.com/roc-lang/unicode)
		## for Unicode capitalization that can be upgraded independently from the language's builtins.
		##
		## To do a case-insensitive comparison of the ASCII characters in a string,
		## you can use [Str.caseless_ascii_equals].
		with_ascii_lowercased : Str -> Str

		## Returns a version of the string with all [ASCII characters](https://en.wikipedia.org/wiki/ASCII) uppercased.
		## Non-ASCII characters are left unmodified. For example:
		##
		## ```roc
		##  expect "café".with_ascii_uppercased() == "CAFé"
		## ```
		##
		## This function is useful for things like
		## [command-line flags](https://en.wikipedia.org/wiki/Command-line_interface#Command-line_option)
		## and [environment variable names](https://en.wikipedia.org/wiki/Environment_variable)
		## where you know in advance that you're dealing with a string containing only ASCII characters.
		## It has better performance than uppercasing operations which take Unicode into account.
		##
		## That said, strings received from user input can always contain
		## non-ASCII Unicode characters, and uppercasing [Unicode](https://unicode.org)
		## works differently in different languages.
		## For example, the string `"i"` uppercases to `"I"` in English and to `"İ"`
		## (a [dotted I](https://en.wikipedia.org/wiki/%C4%B0)) in Turkish.
		## These rules can also change in each Unicode release,
		## so we have a separate [`unicode` package](https://github.com/roc-lang/unicode) for Unicode capitalization
		## that can be upgraded independently from the language's builtins.
		##
		## To do a case-insensitive comparison of the ASCII characters in a string,
		## you can use [Str.caseless_ascii_equals].
		with_ascii_uppercased : Str -> Str

		## Check if the given [Str] starts with a value.
		## ```roc
		## expect "ABC".starts_with("A") == Bool.True
		## expect "ABC".starts_with("X") == Bool.False
		## ```
		starts_with : Str, Str -> Bool

		## Check if the given [Str] ends with a value.
		## ```roc
		## expect "ABC".ends_with("C") == Bool.True
		## expect "ABC".ends_with("X") == Bool.False
		## ```
		ends_with : Str, Str -> Bool

		## Repeats a string the given number of times.
		## ```roc
		## expect "z".repeat(3) == "zzz"
		## expect "na".repeat(8) == "nananananananana"
		## ```
		## Returns `""` when given `""` for the string or `0` for the count.
		## ```roc
		## expect "".repeat(10) == ""
		## expect "anything".repeat(0) == ""
		## ```
		repeat : Str, U64 -> Str

		## Adds a prefix to the given [Str].
		## ```roc
		## expect "Awesome".with_prefix("Roc") == "RocAwesome"
		## ```
		with_prefix : Str, Str -> Str
		with_prefix = |string, prefix| Str.concat(prefix, string)

		## Drops the given prefix [Str] from the start of a [Str]
		## If the prefix is not found, returns the original string.
		##
		## ```roc
		## expect "bar".drop_prefix("foo") == "bar"
		## expect "foobar".drop_prefix("foo") == "bar"
		## ```
		drop_prefix : Str, Str -> Str

		## Drops the given suffix [Str] from the end of a [Str]
		## If the suffix is not found, returns the original string.
		##
		## ```roc
		## expect "bar".drop_suffix("foo") == "bar"
		## expect "barfoo".drop_suffix("foo") == "bar"
		## ```
		drop_suffix : Str, Str -> Str

		## Gives the number of bytes in a [Str] value.
		## ```roc
		## expect "Hello World".count_utf8_bytes() == 11
		## ```
		count_utf8_bytes : Str -> U64

		## Returns a string of the specified capacity without any content.
		##
		## This is like calling [Str.reserve] on an empty string. It's intended for
		## building up a string incrementally, for example by calling [Str.concat] on it:
		##
		## ```roc
		## greeting = "Hello and welcome to Roc"
		## subject = "Awesome Programmer"
		##
		## # Evaluates to "Hello and welcome to Roc, Awesome Programmer!"
		## # Note that string interpolation with "${greeting}, ${subject}!" would be preferred here,
		## # this is just a simple example to demonstrate `Str.with_capacity`.
		## hello_world =
		##     Str.with_capacity(45)
		##         .concat(greeting)
		##         .concat(", ")
		##         .concat(subject)
		##         .concat("!")
		## ```
		##
		## When the final size is known up front, [Str.with_capacity] guarantees that
		## subsequent calls to [Str.concat] will not need to reallocate. Whether this
		## is faster than starting from `""` depends on the system allocator: many
		## allocators can extend an existing allocation in place, making the difference
		## small or negligible. The benefit is most pronounced when reallocation would
		## otherwise force a full copy of the string.
		##
		## If you don't know the exact capacity, passing a value larger than necessary
		## still avoids reallocation, at the cost of using more memory than is needed.
		##
		## For more details, see [Str.reserve].
		with_capacity : U64 -> Str

		## Increase a string's capacity by at least the given number of additional bytes.
		##
		## When you plan to append more bytes onto a string, [Str.reserve] can help
		## avoid intermediate reallocations during a chain of [Str.concat] calls.
		## Consider the following example, which does not use [Str.reserve]:
		##
		## ```roc
		## greeting = "Hello and welcome to Roc"
		## subject = "Awesome Programmer"
		##
		## # Evaluates to "Hello and welcome to Roc, Awesome Programmer!"
		## hello_world =
		##     greeting
		##         .concat(", ")
		##         .concat(subject)
		##         .concat("!")
		## ```
		##
		## In this example:
		## 1. We start with `greeting`, which has both a length and capacity of 24 bytes.
		## 2. `.concat(", ")` sees there isn't enough capacity for 2 more bytes, so
		##    it allocates a new 26-byte buffer, copies the existing 24 bytes into
		##    it, and writes `", "` at the end. The old allocation is deallocated.
		## 3. `.concat(subject)` repeats the process: allocate a 44-byte buffer,
		##    copy the 26 existing bytes, write `"Awesome Programmer"`.
		## 4. `.concat("!")` repeats once more: allocate 45 bytes, copy 44, write `"!"`.
		##
		## With [Str.reserve], the chain only needs one allocation up front:
		##
		## ```roc
		## hello_world =
		##     greeting
		##         .reserve(21)
		##         .concat(", ")
		##         .concat(subject)
		##         .concat("!")
		## ```
		##
		## Here, `.reserve(21)` ensures there is room for an additional 21 bytes
		## (`", "` + `"Awesome Programmer"` + `"!"`), allocating a 45-byte buffer
		## and copying `greeting` into it. The subsequent [Str.concat] calls all fit
		## in that capacity and do not need to reallocate.
		##
		## Whether this is actually faster depends on the system allocator: many
		## allocators can extend an existing allocation in place, in which case the
		## per-concat reallocation is cheap and [Str.reserve] makes little observable
		## difference. The benefit is most pronounced when reallocation would
		## otherwise force a full copy of the string.
		##
		## [Str.reserve] is not free — when more capacity is needed, it always
		## performs a heap allocation. Only use it when you actually expect to make
		## use of the extra capacity.
		##
		## When you don't know exactly how many bytes you'll need, choosing a value
		## somewhat higher than necessary is usually safe; a value that's too low
		## may force later reallocation, while a value much higher than necessary
		## just wastes memory.
		##
		## If you plan to use [Str.reserve] on an empty string, use [Str.with_capacity]
		## instead.
		reserve : Str, U64 -> Str

		## Shrink the memory footprint of a str such that its capacity and length are equal.
		## Note: This will also convert seamless slices to regular lists.
		release_excess_capacity : Str -> Str

		## Returns a [List] of the string's [U8] UTF-8 [code units](https://unicode.org/glossary/#code_unit).
		## (To split the string into a [List] of smaller [Str] values instead of [U8] values,
		## see [Str.split_on].)
		## ```roc
		## expect "Roc".to_utf8() == [82, 111, 99]
		## expect "鹏".to_utf8() == [233, 185, 143]
		## expect "சி".to_utf8() == [224, 174, 154, 224, 174, 191]
		## expect "🐦".to_utf8() == [240, 159, 144, 166]
		## ```
		to_utf8 : Str -> List(U8)

		## Converts a [List] of [U8] UTF-8 [code units](https://unicode.org/glossary/#code_unit) to a string.
		## Any grouping of invalid byte sequences are replaced with a single unicode replacement character '�'.
		##
		## An invalid byte sequence is defined as
		## - a 2-byte-sequence starting byte, followed by less than 1 continuation byte
		## - a 3-byte-sequence starting byte, followed by less than 2 continuation bytes
		## - a 4-byte-sequence starting byte, followed by less than 3 continuation bytes
		## - an invalid codepoint from the surrogate pair block
		## - an invalid codepoint greater than 0x110000 encoded as a 4-byte sequence
		## - any valid codepoint encoded as an incorrect sequence, for instance a codepoint that should be a 2-byte sequence encoded as a 3- or 4-byte sequence
		##
		## ```roc
		## expect Str.from_utf8_lossy([82, 111, 99, 240, 159, 144, 166]) == "Roc🐦"
		## expect Str.from_utf8_lossy([82, 255, 99]) == "R�c"
		## expect Str.from_utf8_lossy([82, 0xED, 0xA0, 0xBD, 99]) == "R�c"
		## ```
		from_utf8_lossy : List(U8) -> Str

		## Converts a [List] of [U8] UTF-8 [code units](https://unicode.org/glossary/#code_unit) to a string.
		##
		## Returns `Err` if the given bytes are invalid UTF-8, and returns `Ok("")` when given `[]`.
		## ```roc
		## expect Str.from_utf8([82, 111, 99]) == Ok("Roc")
		## expect Str.from_utf8([233, 185, 143]) == Ok("鹏")
		## expect Str.from_utf8([224, 174, 154, 224, 174, 191]) == Ok("சி")
		## expect Str.from_utf8([240, 159, 144, 166]) == Ok("🐦")
		## expect Str.from_utf8([]) == Ok("")
		## expect Str.from_utf8([255]).is_err()
		## ```
		from_utf8 : List(U8) -> Try(Str, [BadUtf8({ problem : Str.Utf8Problem, index : U64 }), ..])

		## Split a string around a separator.
		##
		## Passing `""` for the separator is not useful;
		## it returns the original string wrapped in a [List].
		## ```roc
		## expect "1,2,3".split_on(",") == ["1","2","3"]
		## expect "1,2,3".split_on("") == ["1,2,3"]
		## ```
		split_on : Str, Str -> List(Str)

		## Combines a [List] of strings into a single string, with a separator
		## string in between each.
		## ```roc
		## expect Str.join_with(["one", "two", "three"], ", ") == "one, two, three"
		## expect Str.join_with(["1", "2", "3", "4"], ".") == "1.2.3.4"
		## ```
		join_with : List(Str), Str -> Str

		## Returns `True` if the two strings are exactly the same.
		is_eq : Str, Str -> Bool

		## Returns a human-readable representation of a value, useful for debugging.
		inspect : _val -> Str

		## Encode a string using a format that provides encode_str
		encode : Str, fmt -> Try(encoded, err)
			where [fmt.encode_str : fmt, Str -> Try(encoded, err)]
		encode = |self, format| {
			format.encode_str(self)
		}

		## Decode a string using a format that provides decode_str
		decode : src, fmt -> (Try(Str, err), src)
			where [fmt.decode_str : fmt, src -> (Try(Str, err), src)]
		decode = |source, format| {
			Fmt : fmt
			Fmt.decode_str(format, source)
		}
	}

	List(_item) :: [ProvidedByCompiler].{

		## Returns the length of the list, which is equal to the number of elements it contains.
		##
		## One [List] can store up to `Num.max_i64` (TODO update for current max function) elements on 64-bit targets and `Num.max_i32` (TODO update for current max function) on 32-bit targets like wasm.
		## This means the #U64 this function returns can always be safely converted to #I64 or #I32, depending on the target.
		len : List(_item) -> U64

		##  Check if the list is empty.
		## ```roc
		## [1, 2, 3].is_empty()
		##
		## [].is_empty()
		## ```
		is_empty : List(_item) -> Bool
		is_empty = |list| List.len(list) == 0

		## Put two lists together.
		## ```roc
		## [1, 2, 3].concat([4, 5])
		##
		## [0, 1, 2].concat([3, 4])
		## ```
		concat : List(item), List(item) -> List(item)

		## Create a list with space for at least capacity elements
		with_capacity : U64 -> List(item)

		## Sort a list using a custom comparison function. The comparator receives two
		## elements and returns `LT`, `EQ`, or `GT` to indicate their relative order.
		## ```roc
		## expect [3, 1, 2].sort_with(|a, b| if a < b LT else if a > b GT else EQ) == [1, 2, 3]
		##
		## # Sort in descending order by swapping the LT and GT
		## expect [3, 1, 2].sort_with(|a, b| if a > b LT else if a < b GT else EQ) == [3, 2, 1]
		## ```
		sort_with : List(item), (item, item -> [LT, EQ, GT]) -> List(item)

		## Returns `True` if the two lists have the same length and their elements are pairwise equal.
		is_eq : List(item), List(item) -> Bool
			where [item.is_eq : item, item -> Bool]
		is_eq = |self, other| {
			if self.len() != other.len() {
				return False
			}

			var $index = 0

			while $index < self.len() {
				if list_get_unsafe(self, $index) != list_get_unsafe(other, $index) {
					return False
				}

				$index = $index + 1
			}

			True
		}

		## Add a single element to the end of a list.
		## ```roc
		## [1, 2, 3].append(4)
		##
		## [0, 1, 2].append(3)
		## ```
		append : List(a), a -> List(a)
		append = |list, item| list_append_unsafe(list, item)

		## Returns the first element in the list, or `ListWasEmpty` if it was empty.
		## ```roc
		## expect [1, 2, 3].first() == Ok(1)
		## expect [].first() == Err(ListWasEmpty)
		## ```
		first : List(item) -> Try(item, [ListWasEmpty, ..])
		first = |list| if List.is_empty(list) {
			Try.Err(ListWasEmpty)
		} else {
			Try.Ok(list_get_unsafe(list, 0))
		}

		## Returns an element from a list at the given index.
		##
		## Returns `Err OutOfBounds` if the given index exceeds the List's length
		## ```roc
		## expect [100, 200, 300].get(1) == Ok(200)
		## expect [100, 200, 300].get(5) == Err(OutOfBounds)
		## ```
		get : List(item), U64 -> Try(item, [OutOfBounds, ..])
		get = |list, index| if index < List.len(list) {
			Try.Ok(list_get_unsafe(list, index))
		} else {
			Try.Err(OutOfBounds)
		}

		## Returns the reversed list.
		## ```roc
		## expect [1, 2, 3].rev() == [3, 2, 1]
		## expect [].rev() == []
		## ```
		rev : List(item) -> List(item)
		rev = |list| {
			# TODO: Optimize with in-place update when list is unique
			empty_list = List.with_capacity(list.len())
			list.fold_rev(empty_list, |item, newlist| list_append_unsafe(newlist, item))
		}

		for_each! : List(item), (item => {}) => {}
		for_each! = |items, fun!| for item in items {
			fun!(item)
		}

		## Convert each element in the list to something new, by calling a conversion
		## function on each of them. Then return a new list of the converted values.
		## ```roc
		## expect [1, 2, 3].map(|num| num + 1) == [2, 3, 4]
		##
		## expect ["", "a", "bc"].map(Str.is_empty) == [Bool.True, Bool.False, Bool.False]
		## ```
		map : List(a), (a -> b) -> List(b)
		map = |list, transform| {
			# TODO: Optimize with in-place update when list is unique and element sizes match
			var $new_list = List.with_capacity(list.len())
			for item in list {
				$new_list = list_append_unsafe($new_list, transform(item))
			}
			$new_list
		}

		## Run the given function on each element of a list, and return all the
		## elements for which the function returned `Bool.True`.
		## ```roc
		## [1, 2, 3, 4].keep_if(|num| num > 2)
		## ```
		## ## Performance Details
		##
		## [List.keep_if] always returns a list that takes up exactly the same amount
		## of memory as the original, even if its length decreases. This is because it
		## can't know in advance exactly how much space it will need, and if it guesses a
		## length that's too low, it would have to re-allocate.
		##
		## (If you want to do an operation like this which reduces the memory footprint
		## of the resulting list, you can do two passes over the list with [List.fold] - one
		## to calculate the precise new size, and another to populate the new list.)
		##
		## If given a unique list, [List.keep_if] will mutate it in place to assemble the appropriate list.
		## If that happens, this function will not allocate any new memory on the heap.
		## If all elements in the list end up being kept, Roc will return the original
		## list unaltered.
		##
		keep_if : List(a), (a -> Bool) -> List(a)
		keep_if = |list, predicate|
			List.fold(
				list,
				[],
				|acc, elem|
					if predicate(elem) {
						List.concat(acc, [elem])
					} else {
						acc
					},
			)

		## Run the given function on each element of a list, and return all the
		## elements for which the function returned `Bool.False`.
		## ```roc
		## [1, 2, 3, 4].drop_if(|num| num > 2)
		## ```
		## ## Performance Details
		##
		## `List.drop_if` has the same performance characteristics as [List.keep_if].
		## See its documentation for details on those characteristics!
		drop_if : List(a), (a -> Bool) -> List(a)
		drop_if = |list, predicate|
			List.fold(
				list,
				[],
				|acc, elem|
					if predicate(elem) {
						acc
					} else {
						List.concat(acc, [elem])
					},
			)

		## Run the given function on each element of a list, and return the
		## number of elements for which the function returned `Bool.True`.
		## ```roc
		## expect [1, -2, -3].count_if(I64.is_negative) == 2
		## expect [1, 2, 3].count_if(|num| num > 1) == 2
		## ```
		count_if : List(a), (a -> Bool) -> U64
		count_if = |list, predicate|
			List.fold(
				list,
				0,
				|acc, elem|
					if predicate(elem) {
						acc + 1
					} else {
						acc
					},
			)

		## Build a value using each element in the list.
		##
		## Starting with a given `state` value, this folds through each element in the
		## list from first to last, running a given `step` function on that element
		## which updates the `state`. It returns the final `state` at the end.
		## ```roc
		## [2, 4, 8].fold(0, U64.plus)
		## ```
		## This returns 14 because:
		## * `state` starts at 0
		## * Each `step` runs `state.plus(elem)`, and the return value becomes the new `state`.
		##
		## Here is a table of how `state` changes as [List.fold] folds over the elements
		## `[2, 4, 8]` using [U64.plus] as its `step` function to determine the next `state`.
		##
		## state | elem  | U64.plus(state, elem)
		## :---: | :---: | :----------------:
		## 0     |       |
		## 0     | 2     | 2
		## 2     | 4     | 6
		## 6     | 8     | 14
		##
		## The following returns -6:
		## ```roc
		## [1, 2, 3].fold(0, I64.minus)
		## ```
		## Note that in other languages, `fold` is sometimes called `reduce`,
		## `fold_left`, or `foldl`.
		fold : List(item), state, (state, item -> state) -> state
		fold = |list, init, step| {
			var $state = init

			for item in list {
				$state = step($state, item)
			}

			$state
		}

		## Like [List.fold], but walks the list from last to first. The `step` function
		## receives the element first and the current `state` second.
		##
		## ```roc
		## expect [1, 2, 3].fold_rev(0, I64.minus) == 2
		## ```
		##
		## Here is a table of how `state` changes as [List.fold_rev] folds over the
		## elements `[1, 2, 3]` using [I64.minus] as its `step` function.
		##
		## state | elem  | I64.minus(elem, state)
		## :---: | :---: | :-------------------:
		## 0     |       |
		## 0     | 3     | 3
		## 3     | 2     | -1
		## -1    | 1     | 2
		##
		## Note that in other languages, `fold_rev` is sometimes called `reduce_right`,
		## `fold_right`, or `foldr`.
		fold_rev : List(item), state, (item, state -> state) -> state
		fold_rev = |list, init, step| {
			var $state = init
			var $index = list.len()

			while $index > 0 {
				$index = $index - 1
				item = list_get_unsafe(list, $index)
				$state = step(item, $state)
			}

			$state
		}

		## Run the given predicate on each element of the list, returning `Bool.True` if
		## any of the elements satisfy it.
		## ```roc
		## expect [1, 2, 3].any(|n| n % 2 == 0)
		##
		## expect ![1, 2, 3].any(|n| n < 0)
		## ```
		any : List(a), (a -> Bool) -> Bool
		any = |list, predicate| {
			for item in list {
				if predicate(item) {
					return True
				}
			}
			False
		}

		## Returns `Bool.True` if the list contains an element equal to the given value.
		## ```roc
		## expect [1, 2, 3].contains(2)
		##
		## expect ![1, 2, 3].contains(4)
		## ```
		contains : List(a), a -> Bool where [a.is_eq : a, a -> Bool]
		contains = |list, elt| {
			List.any(list, |x| x == elt)
		}

		## Run the given predicate on each element of the list, returning `Bool.True` if
		## all of the elements satisfy it.
		## ```roc
		## expect [2, 4, 6].all(|n| n % 2 == 0)
		##
		## expect ![1, 2, 3].all(|n| n % 2 == 0)
		## ```
		all : List(a), (a -> Bool) -> Bool
		all = |list, predicate| {
			for item in list {
				if Bool.not(predicate(item)) {
					return False
				}
			}
			True
		}

		## Returns the last element in the list, or `ListWasEmpty` if it was empty.
		## ```roc
		## expect [1, 2, 3].last() == Ok(3.0)
		## expect [].last() == Err(ListWasEmpty)
		## ```
		last : List(item) -> Try(item, [ListWasEmpty, ..])
		last = |list| if List.is_empty(list) {
			Try.Err(ListWasEmpty)
		} else {
			Try.Ok(list_get_unsafe(list, List.len(list) - 1))
		}

		## Create a list with a single element in it.
		##
		## ```roc
		## expect List.single(42) == [42.0]
		## ```
		single : item -> List(item)
		single = |x| [x]

		## Remove the element at the given index. If the index is out of bounds, the
		## list is returned unchanged.
		## ```roc
		## expect [10, 20, 30, 40].drop_at(1) == [10, 30, 40]
		##
		## expect [10, 20, 30].drop_at(5) == [10, 20, 30]
		## ```
		drop_at : List(a), U64 -> List(a)

		## Return a sublist of the list starting at `start` and containing up to `len`
		## elements. Out-of-bounds ranges are clamped, producing a shorter or empty list.
		## ```roc
		## expect [1, 2, 3, 4, 5].sublist({ start: 1, len: 3 }) == [2, 3, 4]
		##
		## expect [1, 2, 3].sublist({ start: 1, len: 10 }) == [2, 3]
		##
		## expect [1, 2, 3].sublist({ start: 10, len: 2 }) == []
		## ```
		sublist : List(a), { start : U64, len : U64 } -> List(a)

		## Return the first `n` elements of the list. If the list has fewer than `n`
		## elements, the entire list is returned.
		## ```roc
		## expect [1, 2, 3, 4, 5].take_first(3) == [1, 2, 3]
		##
		## expect [1, 2].take_first(10) == [1, 2]
		## ```
		take_first : List(a), U64 -> List(a)
		take_first = |list, n| {
			List.sublist(list, { len: n, start: 0 })
		}

		## Returns the given number of elements from the end of the list.
		## ```roc
		## expect [1, 2, 3, 4, 5, 6, 7, 8].take_last(4) == [5, 6, 7, 8]
		## ```
		## If there are fewer elements in the list than the requested number,
		## the entire list is returned.
		## ```roc
		## expect [1, 2].take_last(5) == [1, 2]
		## ```
		## To *remove* elements from the end of the list, use `List.take_first`.
		##
		## To remove elements from both the beginning and end of the list,
		## use `List.sublist`.
		##
		take_last : List(a), U64 -> List(a)
		take_last = |list, n| {
			len = List.len(list)
			start = if (len <= n) 0 else len - n
			List.sublist(list, { start: start, len: len })
		}

		## Drops n elements from the beginning of the list. If `n` is larger than the
		## list length, an empty list is returned.
		## ```roc
		## expect [1, 2, 3, 4, 5].drop_first(2) == [3, 4, 5]
		##
		## expect [1, 2, 3].drop_first(10) == []
		## ```
		drop_first : List(a), U64 -> List(a)
		drop_first = |list, n| {
			len = List.len(list)
			List.sublist(list, { start: n, len: len })
		}

		## Drops n elements from the end of the list. If `n` is larger than the
		## list length, an empty list is returned.
		## ```roc
		## expect [1, 2, 3, 4, 5].drop_last(2) == [1, 2, 3]
		##
		## expect [1, 2, 3].drop_last(10) == []
		## ```
		drop_last : List(a), U64 -> List(a)
		drop_last = |list, n| {
			len = List.len(list)
			take_len = if (len <= n) 0 else len - n
			List.sublist(list, { start: 0, len: take_len })
		}

		## Join a list of items into a single item, inserting the given separator between
		## each pair. Works for any type that implements a `join_with` method, such as [Str].
		## ```roc
		## expect ["a", "b", "c"].join_with(", ") == "a, b, c"
		##
		## expect [].join_with(", ") == ""
		## ```
		join_with : List(item), item -> item
			where [item.join_with : List(item), item -> item]
		join_with = |list, joiner| {
			Item : item
			Item.join_with(list, joiner)
		}

		## Build a list by repeating the given value `n` times.
		## ```roc
		## expect List.repeat(0, 3) == [0, 0, 0]
		##
		## expect List.repeat("hi", 0) == []
		## ```
		repeat : a, U64 -> List(a)
		repeat = |item, n| {
			var $list = List.with_capacity(n)
			var $count = 0
			while $count < n {
				$list = List.append($list, item)
				$count = $count + 1
			}
			$list
		}

		## Sum the elements of a list. Works for any type that implements `plus` and
		## `default` methods, such as the numeric types.
		## ```roc
		## expect List.sum([1.I64, 2, 3, 4]) == 10
		##
		## expect List.sum([]) == 0.I64
		## ```
		sum : List(item) -> item
			where [item.plus : item, item -> item, item.default : item]
		sum = |list| {
			Item : item
			List.fold(list, Item.default(), |acc, elem| acc + elem)
		}

		## Encode a list using a format that provides encode_list
		encode : List(item), fmt -> Try(encoded, err)
			where [
				fmt.encode_list : fmt, List(item), (item, fmt -> Try(encoded, err)) -> Try(encoded, err),
				item.encode : item, fmt -> Try(encoded, err),
			]
		encode = |self, format| {
			format.encode_list(self, |elem, f| elem.encode(f))
		}

		## Decode a list using a format that provides decode_list
		decode : src, fmt -> (Try(List(item), err), src)
			where [
				fmt.decode_list : fmt, src, (src, fmt -> (Try(item, err), src)) -> (Try(List(item), err), src),
				item.decode : src, fmt -> (Try(item, err), src),
			]
		decode = |source, format| {
			Fmt : fmt
			Item : item
			Fmt.decode_list(format, source, |s, f| Item.decode(s, f))
		}

	}

	Bool := [False, True].{

		## Returns `Bool.False` when given `Bool.True`, and vice versa. This is
		## equivalent to the logic [NOT](https://en.wikipedia.org/wiki/Negation)
		## gate. The operator `!` can also be used as shorthand for `Bool.not`.
		## ```roc
		## expect Bool.not(Bool.False) == Bool.True
		## expect !Bool.False == Bool.True
		## ```
		not : Bool -> Bool
		not = |bool| match bool {
			Bool.True => Bool.False
			Bool.False => Bool.True
		}

		## Returns `Bool.True` if the two booleans are the same, and `Bool.False` if they are different.
		is_eq : Bool, Bool -> Bool
		is_eq = |a, b| match a {
			Bool.True => match b {
				Bool.True => Bool.True
				Bool.False => Bool.False
			}
			Bool.False => match b {
				Bool.True => Bool.False
				Bool.False => Bool.True
			}
		}

		## Encode a bool using a format that provides encode_bool
		encode : Bool, fmt -> Try(encoded, err)
			where [fmt.encode_bool : fmt, Bool -> Try(encoded, err)]
		encode = |self, format| {
			format.encode_bool(self)
		}

		## Decode a bool using a format that provides decode_bool
		decode : src, fmt -> (Try(Bool, err), src)
			where [fmt.decode_bool : fmt, src -> (Try(Bool, err), src)]
		decode = |source, format| {
			Fmt : fmt
			Fmt.decode_bool(format, source)
		}
	}

	Box(item) :: [ProvidedByCompiler].{

		## Wraps a value in a generic, opaque representation (box) that can easily be passed to the platform.
		## Boxing is an expensive process because it copies the value from the stack to the heap.
		## This may provide a performance optimization for advanced use cases with large values.
		box : item -> Box(item)

		## Unwraps a value from a box. This is the inverse of `Box.box`, and is also an expensive operation.
		unbox : Box(item) -> item
	}

	Try(ok, err) := [Ok(ok), Err(err)].{

		## Returns `Bool.True` if the result indicates a success, else returns `Bool.False`.
		## ```roc
		## expect Try.Ok(5).is_ok()
		## ```
		is_ok : Try(_ok, _err) -> Bool
		is_ok = |try| match try {
			Ok(_) => True
			Err(_) => False
		}

		## Returns `Bool.True` if the result indicates a failure, else returns `Bool.False`.
		## ```roc
		## expect Try.Err("uh oh").is_err()
		## ```
		is_err : Try(_ok, _err) -> Bool
		is_err = |try| match try {
			Ok(_) => False
			Err(_) => True
		}

		## If the result is `Ok`, returns the value it holds. Otherwise, returns
		## the given default value.
		##
		## Note: This function should be used sparingly, because it hides that an error
		## happened, which will make debugging harder. Prefer using `?` to forward errors or
		## handle them explicitly with `match`.
		## ```roc
		## expect Try.Err("uh oh").ok_or(42) == 42
		##
		## expect Try.Ok(7).ok_or(42) == 7
		## ```
		ok_or : Try(ok, _err), ok -> ok
		ok_or = |try, fallback| match try {
			Ok(val) => val
			Err(_) => fallback
		}

		## If the result is `Err`, returns the value it holds. Otherwise, returns
		## the given default value.
		## ```roc
		## expect Try.Err("uh oh").err_or("fallback") == "uh oh"
		##
		## expect Try.Ok(7).err_or("fallback") == "fallback"
		## ```
		err_or : Try(_ok, err), err -> err
		err_or = |try, fallback| match try {
			Err(val) => val
			Ok(_) => fallback
		}

		## If the result is `Ok`, transforms the value it holds by running a conversion
		## function on it. Then returns a new `Ok` holding the transformed value. If the
		## result is `Err`, this has no effect. Use [map_err] to transform an `Err`.
		## ```roc
		## expect Try.Ok(12.I64).map_ok(|n| -n) == Ok(-12)
		##
		## expect {
		## 	err : Try(I64, Str)
		## 	err = Err("yipes!")
		## 	err.map_ok(|n| -n) == Err("yipes!")
		## }
		## ```
		## Functions like `map` are common in Roc; see for example [List.map] and [Set.map].
		map_ok : Try(a, err), (a -> b) -> Try(b, err)
		map_ok = |try, transform| match try {
			Err(err) => Err(err)
			Ok(a) => Ok(transform(a))
		}

		## Like [map_ok], but the transform function is effectful. If the argument is
		## an `Ok`, the effect is run and its return value is wrapped in a new `Ok`. If
		## the result is `Err`, the effect is not run and the `Err` is returned unchanged.
		## ```roc
		## artist_try.map_ok!(|a| SQL.query!("SELECT * FROM albums WHERE artist_id = ?", [a.id]))
		## ```
		map_ok! : Try(a, err), (a => b) => Try(b, err)
		map_ok! = |try, transform!| match try {
			Err(err) => Err(err)
			Ok(a) => Ok(transform!(a))
		}

		## If the result is `Err`, transforms the value it holds by running a conversion
		## function on it. Then returns a new `Err` holding the transformed value. If
		## the result is `Ok`, this has no effect. Use [map_ok] to transform an `Ok`.
		## ```roc
		## expect [].last().map_err(|_| ProvidedListIsEmpty) == Err(ProvidedListIsEmpty)
		##
		## expect [4].last().map_err(|_| ProvidedListIsEmpty) == Ok(4.0)
		## ```
		map_err : Try(ok, a), (a -> b) -> Try(ok, b)
		map_err = |try, transform| match try {
			Err(a) => Err(transform(a))
			Ok(ok) => Ok(ok)
		}

		## Like [map_err], but the transform function is effectful. If the argument is
		## an `Err`, the effect is run and its return value is wrapped in a new `Err`. If
		## the result is `Ok`, the effect is not run and the `Ok` is returned unchanged.
		## ```roc
		## # Log the failure to the database only when the request errored.
		## request.map_err!(|e| SQL.execute!("INSERT INTO errors (message) VALUES (?)", [e.message]))
		## ```
		map_err! : Try(ok, a), (a => b) => Try(ok, b)
		map_err! = |try, transform!| match try {
			Err(a) => Err(transform!(a))
			Ok(ok) => Ok(ok)
		}

		## Returns `Bool.True` if the two `Try` values are the same variant (`Ok` or `Err`) and their contents are pairwise equal. Otherwise, returns `Bool.False`.
		is_eq : Try(ok, err), Try(ok, err) -> Bool
			where [
				ok.is_eq : ok, ok -> Bool,
				err.is_eq : err, err -> Bool,
			]
		is_eq = |a, b| match a {
			Ok(a_val) => {
				match b {
					Ok(b_val) => a_val.is_eq(b_val)
					Err(_) => False
				}
			}
			Err(a_val) => {
				match b {
					Ok(_) => False
					Err(b_val) => a_val.is_eq(b_val)
				}
			}
		}
	}

	## TODO actually implement Dict
	Dict :: [EmptyDict].{}

	Set(item) :: [Items(List(item))].{

		## Returns `Bool.True` if the two sets contain the same values, and `Bool.False` otherwise.
		is_eq : Set(a), Set(a) -> Bool
			where [a.is_eq : a, a -> Bool]
		is_eq = |set_a, set_b| {
			list_a = Set.to_list(set_a)
			list_b = Set.to_list(set_b)

			if List.len(list_a) != List.len(list_b) {
				False
			} else {
				state = List.fold(
					list_a,
					{ all_found: Bool.True, check: list_b },
					|st, elem|
						if List.contains(st.check, elem) {
							st
						} else {
							{ all_found: Bool.False, check: st.check }
						},
				)
				state.all_found
			}
		}

		## Creates a new empty `Set`.
		empty : () -> Set(_item)
		empty = || Items([])

		## Creates a new `Set` with a single value.
		## ```roc
		## Set.single(42)
		## ```
		single : item -> Set(item)
		single = |elem| Items([elem])

		## Counts the number of values in a given `Set`.
		## ```roc
		## expect Set.single(42).len() == 1
		## ```
		len : Set(_item) -> U64
		len = |set| match set {
			Items(list) => List.len(list)
		}

		## Check if the set is empty.
		is_empty : Set(_item) -> Bool
		is_empty = |set| match set {
			Items(list) => List.is_empty(list)
		}

		## Test if a value is in the `Set`.
		contains : Set(a), a -> Bool
			where [a.is_eq : a, a -> Bool]
		contains = |set, elem| match set {
			Items(list) => List.contains(list, elem)
		}

		## Insert a value into a `Set`.
		insert : Set(a), a -> Set(a)
			where [a.is_eq : a, a -> Bool]
		insert = |set, elem| match set {
			Items(list) => Items(
				List.append(
					List.keep_if(list, |x| x != elem),
					elem,
				),
			)
		}

		## Removes the value from the given `Set`.
		remove : Set(a), a -> Set(a)
			where [a.is_eq : a, a -> Bool]
		remove = |set, elem| match set {
			Items(list) => Items(List.keep_if(list, |x| x != elem))
		}

		## Retrieve the values in a `Set` as a `List`.
		to_list : Set(a) -> List(a)
		to_list = |set| match set {
			Items(list) => list
		}

		## Create a `Set` from a `List` of values.
		from_list : List(a) -> Set(a)
			where [a.is_eq : a, a -> Bool]
		from_list = |list| {
			Items(
				List.fold(
					list,
					[],
					|acc, elem|
						if List.contains(acc, elem) {
							acc
						} else {
							List.append(acc, elem)
						},
				),
			)
		}

		## Run the given function on each element in the `Set`, and return
		## a `Set` with just the elements for which the function returned `Bool.True`.
		## ```roc
		## expect Set.from_list([1, 2, 3, 4]).keep_if(|num| num > 2) == Set.from_list([3, 4])
		## ```
		keep_if : Set(a), (a -> Bool) -> Set(a)
		keep_if = |set, predicate| match set {
			Items(list) => Items(List.keep_if(list, predicate))
		}

		## Run the given function on each element in the `Set`, and return
		## a `Set` with just the elements for which the function returned `Bool.False`.
		## ```roc
		## expect Set.from_list([1, 2, 3, 4]).drop_if(|num| num > 2) == Set.from_list([1, 2])
		## ```
		drop_if : Set(a), (a -> Bool) -> Set(a)
		drop_if = |set, predicate| match set {
			Items(list) => Items(List.drop_if(list, predicate))
		}

		## Combine two `Set`s by keeping the
		## [union](https://en.wikipedia.org/wiki/Union_(set_theory))
		## of all the values.
		## ```roc
		## expect {
		## 	  a = Set.from_list([1, 2, 3])
		## 	  b = Set.from_list([3, 4, 5])
		## 	  Set.union(a, b) == Set.from_list([1, 2, 3, 4, 5])
		## }
		## ```
		union : Set(a), Set(a) -> Set(a)
			where [a.is_eq : a, a -> Bool]
		union = |set_a, set_b|
			List.fold(Set.to_list(set_b), set_a, |acc, elem| Set.insert(acc, elem))

		## Combine two `Set`s by keeping the
		## [intersection](https://en.wikipedia.org/wiki/Intersection_(set_theory))
		## of all the values.
		## ```roc
		## expect {
		## 	  a = Set.from_list([1, 2, 3])
		## 	  b = Set.from_list([2, 3, 4])
		## 	  Set.intersection(a, b) == Set.from_list([2, 3])
		## }
		## ```
		intersection : Set(a), Set(a) -> Set(a)
			where [a.is_eq : a, a -> Bool]
		intersection = |set_a, set_b| {
			list_a = Set.to_list(set_a)
			list_b = Set.to_list(set_b)

			state = List.fold(
				list_a,
				{ result: [], check: list_b },
				|st, elem|
					if List.contains(st.check, elem) {
						{ result: List.append(st.result, elem), check: st.check }
					} else {
						st
					},
			)
			Items(state.result)
		}

		## Remove the values in the first `Set` that are also in the second `Set`
		## using the [set difference](https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement).
		## ```roc
		## expect {
		## 	a = Set.from_list([1, 2, 3])
		## 	b = Set.from_list([2, 3, 4])
		## 	Set.difference(a, b) == Set.from_list([1])
		## }
		## ```
		difference : Set(a), Set(a) -> Set(a)
			where [a.is_eq : a, a -> Bool]
		difference = |set_a, set_b| {
			list_a = Set.to_list(set_a)
			list_b = Set.to_list(set_b)

			state = List.fold(
				list_a,
				{ result: [], check: list_b },
				|st, elem|
					if List.contains(st.check, elem) {
						st
					} else {
						{ result: List.append(st.result, elem), check: st.check }
					},
			)
			Items(state.result)
		}

		## Convert each value in the set to something new, by calling a conversion
		## function on each of them. Then return a new set containing the unique
		## converted values.
		## ```roc
		## expect Set.from_list([1, 2, 3]).map(|n| n * 2) == Set.from_list([2, 4, 6])
		##
		## # Duplicates in the mapped output are collapsed — the result is a Set.
		## expect Set.from_list([1, -1, 2, -2]).map(|n| n * n) == Set.from_list([1, 4])
		## ```
		map : Set(a), (a -> b) -> Set(b)
			where [b.is_eq : b, b -> Bool]
		map = |set, transform| match set {
			Items(list) =>
				Items(
					List.fold(
						list,
						[],
						|acc, elem| {
							new_elem = transform(elem)
							if List.contains(acc, new_elem) {
								acc
							} else {
								List.append(acc, new_elem)
							}
						},
					),
				)
			}
	}

	Num :: {}.{
		Numeral :: [
			Self(
				{ # TODO get rid of the "Self" wrapper once we have nominal records"
					# True iff there was a minus sign in front of the literal
					is_negative : Bool,
					# Base-256 digits before and after the decimal point, with any underscores
					# and leading/trailing zeros removed from the source code.
					#
					# Example: If I write "0356.5170" in the source file, that will be:
					# - [1, 100] before the pt, because in base-256, 356 = (1 * 256^1) + (100 * 256^0)
					# - [2, 5] after the pt, because in base-256, 517 = (2 * 256^1) + (5 * 256^0)
					#
					# This design compactly represents the digits without wasting any memory
					# (because base-256 stores each digit using every single bit of the U8), and also
					# allows arbitrary digit length so that userspace custom number types can work with
					# arbitrarily long number literals as long as the number types can support them.
					digits_before_pt : List(U8),
					digits_after_pt : List(U8),
				},
			),
		].{
			is_negative : Numeral -> Bool
			is_negative = |self| match self {
				# TODO make this a nominal record once we have those
				Self({ is_negative: neg, digits_before_pt: _, digits_after_pt: _ }) => neg
			}
		}

		U8 :: [].{

			## Returns the default [U8] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect U8.default() == 0
			## ```
			default : () -> U8
			default = || 0

			## The highest value representable by a [U8], which is `255`.
			## ```roc
			## expect U8.highest == 255
			## ```
			highest : U8
			highest = 255

			## The lowest value representable by a [U8], which is `0`.
			## ```roc
			## expect U8.lowest == 0
			## ```
			lowest : U8
			lowest = 0

			## Convert a [U8] to its decimal string representation.
			## ```roc
			## expect U8.to_str(42) == "42"
			## ```
			to_str : U8 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect U8.is_zero(0)
			##
			## expect !U8.is_zero(7)
			## ```
			is_zero : U8 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect U8.is_eq(3, 3)
			##
			## expect !U8.is_eq(3, 4)
			## ```
			is_eq : U8, U8 -> Bool

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect U8.is_gt(5, 3)
			##
			## expect !U8.is_gt(3, 3)
			## ```

			is_gt : U8, U8 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect U8.is_gte(3, 3)
			##
			## expect !U8.is_gte(2, 3)
			## ```
			is_gte : U8, U8 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect U8.is_lt(3, 5)
			##
			## expect !U8.is_lt(3, 3)
			## ```
			is_lt : U8, U8 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect U8.is_lte(3, 3)
			##
			## expect !U8.is_lte(5, 3)
			## ```
			is_lte : U8, U8 -> Bool

			## Returns the greater of two [U8] values.
			## ```roc
			## expect U8.max(5, 3) == 5
			## ```
			max : U8, U8 -> U8
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [U8] values.
			## ```roc
			## expect U8.min(5, 3) == 3
			## ```
			min : U8, U8 -> U8
			min = |a, b|
				if a < b
					a
				else
					b

			## Add two [U8] values.
			## ```roc
			## expect U8.plus(2, 3) == 5
			## ```
			plus : U8, U8 -> U8

			## Subtract the second [U8] from the first.
			## ```roc
			## expect U8.minus(5, 3) == 2
			## ```
			minus : U8, U8 -> U8

			## Multiply two [U8] values.
			## ```roc
			## expect U8.times(4, 3) == 12
			## ```
			times : U8, U8 -> U8

			## Divide the first [U8] by the second, discarding any remainder. Crashes if the second [U8] is zero.
			## ```roc
			## expect U8.div_by(10, 2) == 5
			##
			## expect U8.div_by(11, 2) == 5
			## ```
			div_by : U8, U8 -> U8

			## Divide the first [U8] by the second, truncating down (toward zero). For unsigned
			## integers this behaves the same as [div_by].
			## ```roc
			## expect U8.div_trunc_by(7, 2) == 3
			## ```
			div_trunc_by : U8, U8 -> U8

			## Return the remainder of dividing the first [U8] by the second.
			## ```roc
			## expect U8.rem_by(7, 3) == 1
			## ```
			rem_by : U8, U8 -> U8

			## Return the modulus of the first [U8] by the second. The modulus is the
			## remainder left after dividing one number by another, and is always in
			## the range `0` up to (but not including) the divisor. For unsigned
			## integers this behaves the same as [rem_by].
			## ```roc
			## expect U8.mod_by(7, 3) == 1
			## ```
			mod_by : U8, U8 -> U8

			## Return the absolute difference between two [U8] values.
			## ```roc
			## expect U8.abs_diff(2, 5) == 3
			##
			## expect U8.abs_diff(5, 2) == 3
			## ```
			abs_diff : U8, U8 -> U8

			## Shift the bits of a [U8] to the left by the given number of positions.
			## Bits shifted past the most significant bit are discarded, and zeros
			## are shifted in on the right. Each left shift by one is equivalent to
			## multiplying by 2 (modulo 256).
			## ```roc
			## expect U8.shift_left_by(1, 3) == 8
			##
			## expect U8.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : U8, U8 -> U8

			## Shift the bits of a [U8] to the right by the given number of positions.
			## Bits shifted past the least significant bit are discarded, and zeros
			## are shifted in on the left. Each right shift by one is equivalent to
			## integer division by 2. For unsigned integers this behaves the same as
			## [shift_right_zf_by].
			## ```roc
			## expect U8.shift_right_by(32, 2) == 8
			##
			## expect U8.shift_right_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_by : U8, U8 -> U8

			## Shift the bits of a [U8] to the right by the given number of positions,
			## filling the vacated high bits with zeros ("zero-fill"). For unsigned
			## integers this behaves the same as [shift_right_by].
			## ```roc
			## expect U8.shift_right_zf_by(32, 2) == 8
			##
			## expect U8.shift_right_zf_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_zf_by : U8, U8 -> U8

			## Build a [U8] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in a
			## [U8] (`0` to `255`), or if any element is not a valid digit.
			## ```roc
			## expect U8.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(U8, [OutOfRange, ..])

			from_numeral : Numeral -> Try(U8, [InvalidNumeral(Str), ..])

			## Parse a [U8] from a [Str]. Returns `Err(BadNumStr)` if the string is
			## not a valid non-negative integer, or if the parsed value does not fit
			## in a [U8] (`0` to `255`).
			## ```roc
			## expect U8.from_str("42") == Ok(42)
			##
			## expect U8.from_str("-1") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(U8, [BadNumStr, ..])

			## List of integers beginning with this `U8` and ending with the other `U8`.
			## (Use [until] instead to end with the other `U8` minus one.)
			## Returns an empty list if this `U8` is greater than the other.
			## ```roc
			## expect U8.to(1, 4) == [1, 2, 3, 4]
			##
			## expect U8.to(3, 3) == [3]
			##
			## expect U8.to(5, 2) == []
			## ```
			to : U8, U8 -> List(U8)
			to = |start, end| range_to(start, end)

			## List of integers beginning with this `U8` and ending with the other `U8` minus one.
			## (Use [to] instead to end with the other `U8` exactly, instead of minus one.)
			## Returns an empty list if this `U8` is greater than or equal to the other.
			## ```roc
			## expect U8.until(1, 4) == [1, 2, 3]
			##
			## expect U8.until(3, 3) == []
			##
			## expect U8.until(5, 2) == []
			## ```
			until : U8, U8 -> List(U8)
			until = |start, end| range_until(start, end)

			# Conversions to signed integers (I8 is lossy, others are safe)

			## Convert a [U8] to an [I8], wrapping on overflow. Values from `0` to
			## `127` are preserved; values from `128` to `255` wrap into the negative
			## range `-128` to `-1` (two's complement reinterpretation of the bits).
			## ```roc
			## expect U8.to_i8_wrap(42) == 42
			##
			## expect U8.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : U8 -> I8

			## Convert a [U8] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `127` succeed; values from `128` to
			## `255` return `Err(OutOfRange)`.
			## ```roc
			## expect U8.to_i8_try(42) == Ok(42)
			##
			## expect U8.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : U8 -> Try(I8, [OutOfRange, ..])
			to_i16 : U8 -> I16
			to_i32 : U8 -> I32
			to_i64 : U8 -> I64
			to_i128 : U8 -> I128

			# Conversions to unsigned integers (all safe widening)
			to_u16 : U8 -> U16
			to_u32 : U8 -> U32
			to_u64 : U8 -> U64
			to_u128 : U8 -> U128

			# Conversions to floating point (all safe)
			to_f32 : U8 -> F32
			to_f64 : U8 -> F64
			to_dec : U8 -> Dec

			## Encode a U8 using a format that provides encode_u8
			encode : U8, fmt -> Try(encoded, err)
				where [fmt.encode_u8 : fmt, U8 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_u8(self)
			}

			## Decode a U8 using a format that provides decode_u8
			decode : src, fmt -> (Try(U8, err), src)
				where [fmt.decode_u8 : fmt, src -> (Try(U8, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_u8(format, source)
			}
		}

		I8 :: [].{

			## Returns the default [I8] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect I8.default() == 0
			## ```
			default : () -> I8
			default = || 0

			## The highest value representable by an [I8], which is `127`.
			## ```roc
			## expect I8.highest == 127
			## ```
			highest : I8
			highest = 127

			## The lowest value representable by an [I8], which is `-128`.
			## ```roc
			## expect I8.lowest == -128
			## ```
			lowest : I8
			lowest = -128

			## Convert an [I8] to its decimal string representation.
			## ```roc
			## expect I8.to_str(42) == "42"
			##
			## expect I8.to_str(-42) == "-42"
			## ```
			to_str : I8 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect I8.is_zero(0)
			##
			## expect !I8.is_zero(7)
			## ```
			is_zero : I8 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the value is less than `0`.
			## ```roc
			## expect I8.is_negative(-3)
			##
			## expect !I8.is_negative(0)
			## ```
			is_negative : I8 -> Bool
			is_negative = |self| self < 0

			## Returns `Bool.True` if the value is greater than `0`.
			## ```roc
			## expect I8.is_positive(3)
			##
			## expect !I8.is_positive(0)
			## ```
			is_positive : I8 -> Bool
			is_positive = |self| self > 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect I8.is_eq(3, 3)
			##
			## expect !I8.is_eq(3, 4)
			## ```
			is_eq : I8, I8 -> Bool

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect I8.is_gt(5, 3)
			##
			## expect !I8.is_gt(3, 3)
			## ```
			is_gt : I8, I8 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect I8.is_gte(3, 3)
			##
			## expect !I8.is_gte(2, 3)
			## ```
			is_gte : I8, I8 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect I8.is_lt(3, 5)
			##
			## expect !I8.is_lt(3, 3)
			## ```
			is_lt : I8, I8 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect I8.is_lte(3, 3)
			##
			## expect !I8.is_lte(5, 3)
			## ```
			is_lte : I8, I8 -> Bool

			## Returns the greater of two [I8] values.
			## ```roc
			## expect I8.max(5, 3) == 5
			##
			## expect I8.max(-3, -1) == -1
			## ```
			max : I8, I8 -> I8
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [I8] values.
			## ```roc
			## expect I8.min(5, 3) == 3
			##
			## expect I8.min(-3, -1) == -3
			## ```
			min : I8, I8 -> I8
			min = |a, b|
				if a < b
					a
				else
					b

			## Negate an [I8]. Crashes on `-128`, since `128` does not fit in an [I8].
			## ```roc
			## expect I8.negate(3) == -3
			##
			## expect I8.negate(-3) == 3
			## ```
			negate : I8 -> I8

			## Return the absolute value of an [I8]. Crashes on `-128`, since `128`
			## does not fit in an [I8].
			## ```roc
			## expect I8.abs(3) == 3
			##
			## expect I8.abs(-3) == 3
			## ```
			abs : I8 -> I8

			## Add two [I8] values.
			## ```roc
			## expect I8.plus(2, 3) == 5
			## ```
			plus : I8, I8 -> I8

			## Subtract the second [I8] from the first.
			## ```roc
			## expect I8.minus(5, 3) == 2
			## ```
			minus : I8, I8 -> I8

			## Multiply two [I8] values.
			## ```roc
			## expect I8.times(4, 3) == 12
			## ```
			times : I8, I8 -> I8

			## Divide the first [I8] by the second, discarding any remainder. Crashes if the second [I8] is zero.
			## ```roc
			## expect I8.div_by(10, 2) == 5
			##
			## expect I8.div_by(11, 2) == 5
			## ```
			div_by : I8, I8 -> I8

			## Divide the first [I8] by the second, truncating toward zero.
			## ```roc
			## expect I8.div_trunc_by(7, 2) == 3
			##
			## expect I8.div_trunc_by(-7, 2) == -3
			## ```
			div_trunc_by : I8, I8 -> I8

			## Return the remainder of dividing the first [I8] by the second. The
			## sign of the result matches the sign of the dividend.
			## ```roc
			## expect I8.rem_by(7, 3) == 1
			##
			## expect I8.rem_by(-7, 3) == -1
			## ```
			rem_by : I8, I8 -> I8

			## Return the modulus of the first [I8] by the second. The modulus is
			## the remainder left after dividing one number by another, and is
			## always in the range `0` up to (but not including) the absolute value
			## of the divisor. Unlike [rem_by], the sign of the result matches the
			## sign of the divisor.
			## ```roc
			## expect I8.mod_by(7, 3) == 1
			##
			## expect I8.mod_by(-7, 3) == 2
			## ```
			mod_by : I8, I8 -> I8

			## Return the absolute difference between two [I8] values as a [U8].
			## The result is a [U8] because the difference between two [I8] values
			## can be as large as `255`, which does not fit in an [I8].
			## ```roc
			## expect I8.abs_diff(2, 5) == 3
			##
			## expect I8.abs_diff(-1, 5) == 6
			## ```
			abs_diff : I8, I8 -> U8

			## Shift the bits of an [I8] to the left by the given number of
			## positions. Bits shifted past the most significant bit are discarded,
			## and zeros are shifted in on the right.
			## ```roc
			## expect I8.shift_left_by(1, 3) == 8
			##
			## expect I8.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : I8, U8 -> I8

			## Shift the bits of an [I8] to the right by the given number of
			## positions, preserving the sign ("arithmetic shift"). The sign bit
			## is shifted in on the left, so negative values remain negative. Each
			## right shift by one is equivalent to integer division by 2 (rounding
			## toward negative infinity).
			## ```roc
			## expect I8.shift_right_by(32, 2) == 8
			##
			## expect I8.shift_right_by(-32, 2) == -8
			## ```
			shift_right_by : I8, U8 -> I8

			## Shift the bits of an [I8] to the right by the given number of
			## positions.
			## ```roc
			## expect I8.shift_right_zf_by(32, 2) == 8
			##
			## expect I8.shift_right_zf_by(0b0101_0000, 3) == 0b0000_1010
			## ```
			shift_right_zf_by : I8, U8 -> I8

			## List of integers beginning with this `I8` and ending with the other `I8`.
			## (Use [until] instead to end with the other `I8` minus one.)
			## Returns an empty list if this `I8` is greater than the other.
			## ```roc
			## expect I8.to(1, 4) == [1, 2, 3, 4]
			##
			## expect I8.to(-2, 1) == [-2, -1, 0, 1]
			##
			## expect I8.to(5, 2) == []
			## ```
			to : I8, I8 -> List(I8)
			to = |start, end| range_to(start, end)

			## List of integers beginning with this `I8` and ending with the other `I8` minus one.
			## (Use [to] instead to end with the other `I8` exactly, instead of minus one.)
			## Returns an empty list if this `I8` is greater than or equal to the other.
			## ```roc
			## expect I8.until(1, 4) == [1, 2, 3]
			##
			## expect I8.until(-2, 1) == [-2, -1, 0]
			##
			## expect I8.until(5, 2) == []
			## ```
			until : I8, I8 -> List(I8)
			until = |start, end| range_until(start, end)

			## Build an [I8] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in an
			## [I8] (`-128` to `127`), or if any element is not a valid digit. The
			## result is always non-negative; to build a negative value, [negate]
			## the result.
			## ```roc
			## expect I8.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(I8, [OutOfRange, ..])

			from_numeral : Numeral -> Try(I8, [InvalidNumeral(Str), ..])

			## Parse an [I8] from a [Str]. Returns `Err(BadNumStr)` if the string
			## is not a valid integer, or if the parsed value does not fit in an
			## [I8] (`-128` to `127`).
			## ```roc
			## expect I8.from_str("42") == Ok(42)
			##
			## expect I8.from_str("-1") == Ok(-1)
			##
			## expect I8.from_str("200") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(I8, [BadNumStr, ..])

			# Conversions to signed integers (all safe widening)
			to_i16 : I8 -> I16
			to_i32 : I8 -> I32
			to_i64 : I8 -> I64
			to_i128 : I8 -> I128

			# Conversions to unsigned integers (all lossy for negative values)

			## Convert an [I8] to a [U8], wrapping on overflow. Values from `0` to
			## `127` are preserved; values from `-128` to `-1` wrap into the range
			## `128` to `255` (two's complement reinterpretation of the bits).
			## ```roc
			## expect I8.to_u8_wrap(42) == 42
			##
			## expect I8.to_u8_wrap(-56) == 200
			## ```
			to_u8_wrap : I8 -> U8

			## Convert an [I8] to a [U8], returning `Err(OutOfRange)` if the value
			## is negative. Values from `0` to `127` succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I8.to_u8_try(42) == Ok(42)
			##
			## expect I8.to_u8_try(-1) == Err(OutOfRange)
			## ```
			to_u8_try : I8 -> Try(U8, [OutOfRange, ..])

			## Convert an [I8] to a [U16], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U16] range (two's complement reinterpretation of
			## the sign-extended bits).
			## ```roc
			## expect I8.to_u16_wrap(42) == 42
			##
			## expect I8.to_u16_wrap(-1) == 65535
			## ```
			to_u16_wrap : I8 -> U16

			## Convert an [I8] to a [U16], returning `Err(OutOfRange)` if the
			## value is negative. Values from `0` to `127` succeed; negative
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect I8.to_u16_try(42) == Ok(42)
			##
			## expect I8.to_u16_try(-1) == Err(OutOfRange)
			## ```
			to_u16_try : I8 -> Try(U16, [OutOfRange, ..])

			## Convert an [I8] to a [U32], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U32] range (two's complement reinterpretation of
			## the sign-extended bits).
			## ```roc
			## expect I8.to_u32_wrap(42) == 42
			##
			## expect I8.to_u32_wrap(-1) == 4294967295
			## ```
			to_u32_wrap : I8 -> U32

			## Convert an [I8] to a [U32], returning `Err(OutOfRange)` if the
			## value is negative. Values from `0` to `127` succeed; negative
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect I8.to_u32_try(42) == Ok(42)
			##
			## expect I8.to_u32_try(-1) == Err(OutOfRange)
			## ```
			to_u32_try : I8 -> Try(U32, [OutOfRange, ..])

			## Convert an [I8] to a [U64], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U64] range (two's complement reinterpretation of
			## the sign-extended bits).
			## ```roc
			## expect I8.to_u64_wrap(42) == 42
			##
			## expect I8.to_u64_wrap(-1) == 18446744073709551615
			## ```
			to_u64_wrap : I8 -> U64

			## Convert an [I8] to a [U64], returning `Err(OutOfRange)` if the
			## value is negative. Values from `0` to `127` succeed; negative
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect I8.to_u64_try(42) == Ok(42)
			##
			## expect I8.to_u64_try(-1) == Err(OutOfRange)
			## ```
			to_u64_try : I8 -> Try(U64, [OutOfRange, ..])

			## Convert an [I8] to a [U128], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U128] range (two's complement reinterpretation
			## of the sign-extended bits).
			## ```roc
			## expect I8.to_u128_wrap(42) == 42
			## ```
			to_u128_wrap : I8 -> U128

			## Convert an [I8] to a [U128], returning `Err(OutOfRange)` if the
			## value is negative. Values from `0` to `127` succeed; negative
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect I8.to_u128_try(42) == Ok(42)
			##
			## expect I8.to_u128_try(-1) == Err(OutOfRange)
			## ```
			to_u128_try : I8 -> Try(U128, [OutOfRange, ..])

			# Conversions to floating point (all safe)
			to_f32 : I8 -> F32
			to_f64 : I8 -> F64
			to_dec : I8 -> Dec

			## Encode an I8 using a format that provides encode_i8
			encode : I8, fmt -> Try(encoded, err)
				where [fmt.encode_i8 : fmt, I8 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_i8(self)
			}

			## Decode an I8 using a format that provides decode_i8
			decode : src, fmt -> (Try(I8, err), src)
				where [fmt.decode_i8 : fmt, src -> (Try(I8, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_i8(format, source)
			}
		}

		U16 :: [].{

			## Returns the default [U16] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect U16.default() == 0
			## ```
			default : () -> U16
			default = || 0

			## The highest value representable by a [U16], which is `65535`.
			## ```roc
			## expect U16.highest == 65535
			## ```
			highest : U16
			highest = 65535

			## The lowest value representable by a [U16], which is `0`.
			## ```roc
			## expect U16.lowest == 0
			## ```
			lowest : U16
			lowest = 0

			## Convert a [U16] to its decimal string representation.
			## ```roc
			## expect U16.to_str(42) == "42"
			## ```
			to_str : U16 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect U16.is_zero(0)
			##
			## expect !U16.is_zero(7)
			## ```
			is_zero : U16 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect U16.is_eq(3, 3)
			##
			## expect !U16.is_eq(3, 4)
			## ```
			is_eq : U16, U16 -> Bool

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect U16.is_gt(5, 3)
			##
			## expect !U16.is_gt(3, 3)
			## ```
			is_gt : U16, U16 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect U16.is_gte(3, 3)
			##
			## expect !U16.is_gte(2, 3)
			## ```
			is_gte : U16, U16 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect U16.is_lt(3, 5)
			##
			## expect !U16.is_lt(3, 3)
			## ```
			is_lt : U16, U16 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect U16.is_lte(3, 3)
			##
			## expect !U16.is_lte(5, 3)
			## ```
			is_lte : U16, U16 -> Bool

			## Returns the greater of two [U16] values.
			## ```roc
			## expect U16.max(5, 3) == 5
			## ```
			max : U16, U16 -> U16
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [U16] values.
			## ```roc
			## expect U16.min(5, 3) == 3
			## ```
			min : U16, U16 -> U16
			min = |a, b|
				if a < b
					a
				else
					b

			## Add two [U16] values.
			## ```roc
			## expect U16.plus(2, 3) == 5
			## ```
			plus : U16, U16 -> U16

			## Subtract the second [U16] from the first.
			## ```roc
			## expect U16.minus(5, 3) == 2
			## ```
			minus : U16, U16 -> U16

			## Multiply two [U16] values.
			## ```roc
			## expect U16.times(4, 3) == 12
			## ```
			times : U16, U16 -> U16

			## Divide the first [U16] by the second, discarding any remainder. Crashes if the second [U16] is zero.
			## ```roc
			## expect U16.div_by(10, 2) == 5
			##
			## expect U16.div_by(11, 2) == 5
			## ```
			div_by : U16, U16 -> U16

			## Divide the first [U16] by the second, truncating down (toward zero). For unsigned
			## integers this behaves the same as [div_by].
			## ```roc
			## expect U16.div_trunc_by(7, 2) == 3
			## ```
			div_trunc_by : U16, U16 -> U16

			## Return the remainder of dividing the first [U16] by the second.
			## ```roc
			## expect U16.rem_by(7, 3) == 1
			## ```
			rem_by : U16, U16 -> U16

			## Return the modulus of the first [U16] by the second. The modulus is the
			## remainder left after dividing one number by another, and is always in
			## the range `0` up to (but not including) the divisor. For unsigned
			## integers this behaves the same as [rem_by].
			## ```roc
			## expect U16.mod_by(7, 3) == 1
			## ```
			mod_by : U16, U16 -> U16

			## Return the absolute difference between two [U16] values.
			## ```roc
			## expect U16.abs_diff(2, 5) == 3
			##
			## expect U16.abs_diff(5, 2) == 3
			## ```
			abs_diff : U16, U16 -> U16

			## Shift the bits of a [U16] to the left by the given number of positions.
			## Bits shifted past the most significant bit are discarded, and zeros
			## are shifted in on the right. Each left shift by one is equivalent to
			## multiplying by 2 (modulo 65536).
			## ```roc
			## expect U16.shift_left_by(1, 3) == 8
			##
			## expect U16.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : U16, U8 -> U16

			## Shift the bits of a [U16] to the right by the given number of positions.
			## Bits shifted past the least significant bit are discarded, and zeros
			## are shifted in on the left. Each right shift by one is equivalent to
			## integer division by 2. For unsigned integers this behaves the same as
			## [shift_right_zf_by].
			## ```roc
			## expect U16.shift_right_by(32, 2) == 8
			##
			## expect U16.shift_right_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_by : U16, U8 -> U16

			## Shift the bits of a [U16] to the right by the given number of positions,
			## filling the vacated high bits with zeros ("zero-fill"). For unsigned
			## integers this behaves the same as [shift_right_by].
			## ```roc
			## expect U16.shift_right_zf_by(32, 2) == 8
			##
			## expect U16.shift_right_zf_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_zf_by : U16, U8 -> U16

			## List of integers beginning with this `U16` and ending with the other `U16`.
			## (Use [until] instead to end with the other `U16` minus one.)
			## Returns an empty list if this `U16` is greater than the other.
			## ```roc
			## expect U16.to(1, 4) == [1, 2, 3, 4]
			##
			## expect U16.to(3, 3) == [3]
			##
			## expect U16.to(5, 2) == []
			## ```
			to : U16, U16 -> List(U16)
			to = |start, end| range_to(start, end)

			## List of integers beginning with this `U16` and ending with the other `U16` minus one.
			## (Use [to] instead to end with the other `U16` exactly, instead of minus one.)
			## Returns an empty list if this `U16` is greater than or equal to the other.
			## ```roc
			## expect U16.until(1, 4) == [1, 2, 3]
			##
			## expect U16.until(3, 3) == []
			##
			## expect U16.until(5, 2) == []
			## ```
			until : U16, U16 -> List(U16)
			until = |start, end| range_until(start, end)

			## Build a [U16] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in a
			## [U16] (`0` to `65535`), or if any element is not a valid digit.
			## ```roc
			## expect U16.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(U16, [OutOfRange, ..])

			from_numeral : Numeral -> Try(U16, [InvalidNumeral(Str), ..])

			## Parse a [U16] from a [Str]. Returns `Err(BadNumStr)` if the string is
			## not a valid non-negative integer, or if the parsed value does not fit
			## in a [U16] (`0` to `65535`).
			## ```roc
			## expect U16.from_str("42") == Ok(42)
			##
			## expect U16.from_str("-1") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(U16, [BadNumStr, ..])

			# Conversions to signed integers

			## Convert a [U16] to an [I8], wrapping on overflow. Values from `0` to
			## `127` are preserved; larger values wrap by truncating to the low 8
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect U16.to_i8_wrap(42) == 42
			##
			## expect U16.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : U16 -> I8

			## Convert a [U16] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `127` succeed; values from `128` to
			## `65535` return `Err(OutOfRange)`.
			## ```roc
			## expect U16.to_i8_try(42) == Ok(42)
			##
			## expect U16.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : U16 -> Try(I8, [OutOfRange, ..])

			## Convert a [U16] to an [I16], wrapping on overflow. Values from `0` to
			## `32767` are preserved; values from `32768` to `65535` wrap into the
			## negative range `-32768` to `-1` (two's complement reinterpretation of
			## the bits).
			## ```roc
			## expect U16.to_i16_wrap(42) == 42
			##
			## expect U16.to_i16_wrap(40000) == -25536
			## ```
			to_i16_wrap : U16 -> I16

			## Convert a [U16] to an [I16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `32767` succeed; values from `32768`
			## to `65535` return `Err(OutOfRange)`.
			## ```roc
			## expect U16.to_i16_try(42) == Ok(42)
			##
			## expect U16.to_i16_try(40000) == Err(OutOfRange)
			## ```
			to_i16_try : U16 -> Try(I16, [OutOfRange, ..])
			to_i32 : U16 -> I32
			to_i64 : U16 -> I64
			to_i128 : U16 -> I128

			# Conversions to unsigned integers

			## Convert a [U16] to a [U8], wrapping on overflow. Values from `0` to
			## `255` are preserved; larger values wrap by truncating to the low 8
			## bits.
			## ```roc
			## expect U16.to_u8_wrap(42) == 42
			##
			## expect U16.to_u8_wrap(300) == 44
			## ```
			to_u8_wrap : U16 -> U8

			## Convert a [U16] to a [U8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `255` succeed; values from `256` to
			## `65535` return `Err(OutOfRange)`.
			## ```roc
			## expect U16.to_u8_try(42) == Ok(42)
			##
			## expect U16.to_u8_try(300) == Err(OutOfRange)
			## ```
			to_u8_try : U16 -> Try(U8, [OutOfRange, ..])
			to_u32 : U16 -> U32
			to_u64 : U16 -> U64
			to_u128 : U16 -> U128

			# Conversions to floating point (all safe)
			to_f32 : U16 -> F32
			to_f64 : U16 -> F64
			to_dec : U16 -> Dec

			# Encode a U16 using a format that provides encode_u16
			encode : U16, fmt -> Try(encoded, err)
				where [fmt.encode_u16 : fmt, U16 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_u16(self)
			}

			decode : src, fmt -> (Try(U16, err), src)
				where [fmt.decode_u16 : fmt, src -> (Try(U16, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_u16(format, source)
			}
		}

		I16 :: [].{

			## Returns the default [I16] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect I16.default() == 0
			## ```
			default : () -> I16
			default = || 0

			## The highest value representable by an [I16], which is `32767`.
			## ```roc
			## expect I16.highest == 32767
			## ```
			highest : I16
			highest = 32767

			## The lowest value representable by an [I16], which is `-32768`.
			## ```roc
			## expect I16.lowest == -32768
			## ```
			lowest : I16
			lowest = -32768

			## Convert an [I16] to its decimal string representation.
			## ```roc
			## expect I16.to_str(42) == "42"
			##
			## expect I16.to_str(-42) == "-42"
			## ```
			to_str : I16 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect I16.is_zero(0)
			##
			## expect !I16.is_zero(7)
			## ```
			is_zero : I16 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the value is less than `0`.
			## ```roc
			## expect I16.is_negative(-3)
			##
			## expect !I16.is_negative(0)
			## ```
			is_negative : I16 -> Bool
			is_negative = |self| self < 0

			## Returns `Bool.True` if the value is greater than `0`.
			## ```roc
			## expect I16.is_positive(3)
			##
			## expect !I16.is_positive(0)
			## ```
			is_positive : I16 -> Bool
			is_positive = |self| self > 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect I16.is_eq(3, 3)
			##
			## expect !I16.is_eq(3, 4)
			## ```
			is_eq : I16, I16 -> Bool

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect I16.is_gt(5, 3)
			##
			## expect !I16.is_gt(3, 3)
			## ```
			is_gt : I16, I16 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect I16.is_gte(3, 3)
			##
			## expect !I16.is_gte(2, 3)
			## ```
			is_gte : I16, I16 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect I16.is_lt(3, 5)
			##
			## expect !I16.is_lt(3, 3)
			## ```
			is_lt : I16, I16 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect I16.is_lte(3, 3)
			##
			## expect !I16.is_lte(5, 3)
			## ```
			is_lte : I16, I16 -> Bool

			## Returns the greater of two [I16] values.
			## ```roc
			## expect I16.max(5, 3) == 5
			##
			## expect I16.max(-3, -1) == -1
			## ```
			max : I16, I16 -> I16
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [I16] values.
			## ```roc
			## expect I16.min(5, 3) == 3
			##
			## expect I16.min(-3, -1) == -3
			## ```
			min : I16, I16 -> I16
			min = |a, b|
				if a < b
					a
				else
					b

			## Negate an [I16]. Crashes on `-32768`, since `32768` does not fit in an [I16].
			## ```roc
			## expect I16.negate(3) == -3
			##
			## expect I16.negate(-3) == 3
			## ```
			negate : I16 -> I16

			## Return the absolute value of an [I16]. Crashes on `-32768`, since `32768`
			## does not fit in an [I16].
			## ```roc
			## expect I16.abs(3) == 3
			##
			## expect I16.abs(-3) == 3
			## ```
			abs : I16 -> I16

			## Add two [I16] values.
			## ```roc
			## expect I16.plus(2, 3) == 5
			## ```
			plus : I16, I16 -> I16

			## Subtract the second [I16] from the first.
			## ```roc
			## expect I16.minus(5, 3) == 2
			## ```
			minus : I16, I16 -> I16

			## Multiply two [I16] values.
			## ```roc
			## expect I16.times(4, 3) == 12
			## ```
			times : I16, I16 -> I16

			## Divide the first [I16] by the second, discarding any remainder. Crashes if the second [I16] is zero.
			## ```roc
			## expect I16.div_by(10, 2) == 5
			##
			## expect I16.div_by(11, 2) == 5
			## ```
			div_by : I16, I16 -> I16

			## Divide the first [I16] by the second, truncating toward zero.
			## ```roc
			## expect I16.div_trunc_by(7, 2) == 3
			##
			## expect I16.div_trunc_by(-7, 2) == -3
			## ```
			div_trunc_by : I16, I16 -> I16

			## Return the remainder of dividing the first [I16] by the second. The
			## sign of the result matches the sign of the dividend.
			## ```roc
			## expect I16.rem_by(7, 3) == 1
			##
			## expect I16.rem_by(-7, 3) == -1
			## ```
			rem_by : I16, I16 -> I16

			## Return the modulus of the first [I16] by the second. The modulus is
			## the remainder left after dividing one number by another, and is
			## always in the range `0` up to (but not including) the absolute value
			## of the divisor. Unlike [rem_by], the sign of the result matches the
			## sign of the divisor.
			## ```roc
			## expect I16.mod_by(7, 3) == 1
			##
			## expect I16.mod_by(-7, 3) == 2
			## ```
			mod_by : I16, I16 -> I16

			## Return the absolute difference between two [I16] values as a [U16].
			## The result is a [U16] because the difference between two [I16] values
			## can be as large as `65535`, which does not fit in an [I16].
			## ```roc
			## expect I16.abs_diff(2, 5) == 3
			##
			## expect I16.abs_diff(-1, 5) == 6
			## ```
			abs_diff : I16, I16 -> U16

			## Shift the bits of an [I16] to the left by the given number of
			## positions. Bits shifted past the most significant bit are discarded,
			## and zeros are shifted in on the right.
			## ```roc
			## expect I16.shift_left_by(1, 3) == 8
			##
			## expect I16.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : I16, U8 -> I16

			## Shift the bits of an [I16] to the right by the given number of
			## positions, preserving the sign ("arithmetic shift"). The sign bit
			## is shifted in on the left, so negative values remain negative. Each
			## right shift by one is equivalent to integer division by 2 (rounding
			## toward negative infinity).
			## ```roc
			## expect I16.shift_right_by(32, 2) == 8
			##
			## expect I16.shift_right_by(-32, 2) == -8
			## ```
			shift_right_by : I16, U8 -> I16

			## Shift the bits of an [I16] to the right by the given number of
			## positions.
			## ```roc
			## expect I16.shift_right_zf_by(32, 2) == 8
			##
			## expect I16.shift_right_zf_by(0b0101_0000, 3) == 0b0000_1010
			## ```
			shift_right_zf_by : I16, U8 -> I16

			## List of integers beginning with this `I16` and ending with the other `I16`.
			## (Use [until] instead to end with the other `I16` minus one.)
			## Returns an empty list if this `I16` is greater than the other.
			## ```roc
			## expect I16.to(1, 4) == [1, 2, 3, 4]
			##
			## expect I16.to(-2, 1) == [-2, -1, 0, 1]
			##
			## expect I16.to(5, 2) == []
			## ```
			to : I16, I16 -> List(I16)
			to = |start, end| range_to(start, end)

			## List of integers beginning with this `I16` and ending with the other `I16` minus one.
			## (Use [to] instead to end with the other `I16` exactly, instead of minus one.)
			## Returns an empty list if this `I16` is greater than or equal to the other.
			## ```roc
			## expect I16.until(1, 4) == [1, 2, 3]
			##
			## expect I16.until(-2, 1) == [-2, -1, 0]
			##
			## expect I16.until(5, 2) == []
			## ```
			until : I16, I16 -> List(I16)
			until = |start, end| range_until(start, end)

			## Build an [I16] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in an
			## [I16] (`-32768` to `32767`), or if any element is not a valid digit.
			## The result is always non-negative; to build a negative value, [negate]
			## the result.
			## ```roc
			## expect I16.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(I16, [OutOfRange, ..])

			from_numeral : Numeral -> Try(I16, [InvalidNumeral(Str), ..])

			## Parse an [I16] from a [Str]. Returns `Err(BadNumStr)` if the string
			## is not a valid integer, or if the parsed value does not fit in an
			## [I16] (`-32768` to `32767`).
			## ```roc
			## expect I16.from_str("42") == Ok(42)
			##
			## expect I16.from_str("-1") == Ok(-1)
			##
			## expect I16.from_str("40000") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(I16, [BadNumStr, ..])

			# Conversions to signed integers

			## Convert an [I16] to an [I8], wrapping on overflow. Values from `-128`
			## to `127` are preserved; other values wrap by truncating to the low 8
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect I16.to_i8_wrap(42) == 42
			##
			## expect I16.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : I16 -> I8

			## Convert an [I16] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-128` to `127` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I16.to_i8_try(42) == Ok(42)
			##
			## expect I16.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : I16 -> Try(I8, [OutOfRange, ..])
			to_i32 : I16 -> I32
			to_i64 : I16 -> I64
			to_i128 : I16 -> I128

			# Conversions to unsigned integers (all lossy for negative values)

			## Convert an [I16] to a [U8], wrapping on overflow. Values from `0` to
			## `255` are preserved; other values wrap by truncating to the low 8
			## bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I16.to_u8_wrap(42) == 42
			##
			## expect I16.to_u8_wrap(-1) == 255
			## ```
			to_u8_wrap : I16 -> U8

			## Convert an [I16] to a [U8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `255` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I16.to_u8_try(42) == Ok(42)
			##
			## expect I16.to_u8_try(-1) == Err(OutOfRange)
			## ```
			to_u8_try : I16 -> Try(U8, [OutOfRange, ..])

			## Convert an [I16] to a [U16], wrapping on overflow. Non-negative
			## values are preserved; negative values wrap into the upper end of the
			## [U16] range (two's complement reinterpretation of the bits).
			## ```roc
			## expect I16.to_u16_wrap(42) == 42
			##
			## expect I16.to_u16_wrap(-1) == 65535
			## ```
			to_u16_wrap : I16 -> U16

			## Convert an [I16] to a [U16], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I16.to_u16_try(42) == Ok(42)
			##
			## expect I16.to_u16_try(-1) == Err(OutOfRange)
			## ```
			to_u16_try : I16 -> Try(U16, [OutOfRange, ..])

			## Convert an [I16] to a [U32], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U32] range (two's complement reinterpretation of
			## the sign-extended bits).
			## ```roc
			## expect I16.to_u32_wrap(42) == 42
			##
			## expect I16.to_u32_wrap(-1) == 4294967295
			## ```
			to_u32_wrap : I16 -> U32

			## Convert an [I16] to a [U32], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I16.to_u32_try(42) == Ok(42)
			##
			## expect I16.to_u32_try(-1) == Err(OutOfRange)
			## ```
			to_u32_try : I16 -> Try(U32, [OutOfRange, ..])

			## Convert an [I16] to a [U64], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U64] range (two's complement reinterpretation of
			## the sign-extended bits).
			## ```roc
			## expect I16.to_u64_wrap(42) == 42
			##
			## expect I16.to_u64_wrap(-1) == 18446744073709551615
			## ```
			to_u64_wrap : I16 -> U64

			## Convert an [I16] to a [U64], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I16.to_u64_try(42) == Ok(42)
			##
			## expect I16.to_u64_try(-1) == Err(OutOfRange)
			## ```
			to_u64_try : I16 -> Try(U64, [OutOfRange, ..])

			## Convert an [I16] to a [U128], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U128] range (two's complement reinterpretation
			## of the sign-extended bits).
			## ```roc
			## expect I16.to_u128_wrap(42) == 42
			## ```
			to_u128_wrap : I16 -> U128

			## Convert an [I16] to a [U128], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I16.to_u128_try(42) == Ok(42)
			##
			## expect I16.to_u128_try(-1) == Err(OutOfRange)
			## ```
			to_u128_try : I16 -> Try(U128, [OutOfRange, ..])

			# Conversions to floating point (all safe)
			to_f32 : I16 -> F32
			to_f64 : I16 -> F64
			to_dec : I16 -> Dec

			# Encode an I16 using a format that provides encode_i16
			encode : I16, fmt -> Try(encoded, err)
				where [fmt.encode_i16 : fmt, I16 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_i16(self)
			}

			decode : src, fmt -> (Try(I16, err), src)
				where [fmt.decode_i16 : fmt, src -> (Try(I16, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_i16(format, source)
			}
		}

		U32 :: [].{

			## Returns the default [U32] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect U32.default() == 0
			## ```
			default : () -> U32
			default = || 0

			## The highest value representable by a [U32], which is `4294967295`.
			## ```roc
			## expect U32.highest == 4294967295
			## ```
			highest : U32
			highest = 4294967295

			## The lowest value representable by a [U32], which is `0`.
			## ```roc
			## expect U32.lowest == 0
			## ```
			lowest : U32
			lowest = 0

			## Convert a [U32] to its decimal string representation.
			## ```roc
			## expect U32.to_str(42) == "42"
			## ```
			to_str : U32 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect U32.is_zero(0)
			##
			## expect !U32.is_zero(7)
			## ```
			is_zero : U32 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect U32.is_eq(3, 3)
			##
			## expect !U32.is_eq(3, 4)
			## ```
			is_eq : U32, U32 -> Bool

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect U32.is_gt(5, 3)
			##
			## expect !U32.is_gt(3, 3)
			## ```
			is_gt : U32, U32 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect U32.is_gte(3, 3)
			##
			## expect !U32.is_gte(2, 3)
			## ```
			is_gte : U32, U32 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect U32.is_lt(3, 5)
			##
			## expect !U32.is_lt(3, 3)
			## ```
			is_lt : U32, U32 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect U32.is_lte(3, 3)
			##
			## expect !U32.is_lte(5, 3)
			## ```
			is_lte : U32, U32 -> Bool

			## Returns the greater of two [U32] values.
			## ```roc
			## expect U32.max(5, 3) == 5
			## ```
			max : U32, U32 -> U32
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [U32] values.
			## ```roc
			## expect U32.min(5, 3) == 3
			## ```
			min : U32, U32 -> U32
			min = |a, b|
				if a < b
					a
				else
					b

			## Add two [U32] values.
			## ```roc
			## expect U32.plus(2, 3) == 5
			## ```
			plus : U32, U32 -> U32

			## Subtract the second [U32] from the first.
			## ```roc
			## expect U32.minus(5, 3) == 2
			## ```
			minus : U32, U32 -> U32

			## Multiply two [U32] values.
			## ```roc
			## expect U32.times(4, 3) == 12
			## ```
			times : U32, U32 -> U32

			## Divide the first [U32] by the second, discarding any remainder. Crashes if the second [U32] is zero.
			## ```roc
			## expect U32.div_by(10, 2) == 5
			##
			## expect U32.div_by(11, 2) == 5
			## ```
			div_by : U32, U32 -> U32

			## Divide the first [U32] by the second, truncating down (toward zero). For unsigned
			## integers this behaves the same as [div_by].
			## ```roc
			## expect U32.div_trunc_by(7, 2) == 3
			## ```
			div_trunc_by : U32, U32 -> U32

			## Return the remainder of dividing the first [U32] by the second.
			## ```roc
			## expect U32.rem_by(7, 3) == 1
			## ```
			rem_by : U32, U32 -> U32

			## Return the modulus of the first [U32] by the second. The modulus is the
			## remainder left after dividing one number by another, and is always in
			## the range `0` up to (but not including) the divisor. For unsigned
			## integers this behaves the same as [rem_by].
			## ```roc
			## expect U32.mod_by(7, 3) == 1
			## ```
			mod_by : U32, U32 -> U32

			## Return the absolute difference between two [U32] values.
			## ```roc
			## expect U32.abs_diff(2, 5) == 3
			##
			## expect U32.abs_diff(5, 2) == 3
			## ```
			abs_diff : U32, U32 -> U32

			## Shift the bits of a [U32] to the left by the given number of positions.
			## Bits shifted past the most significant bit are discarded, and zeros
			## are shifted in on the right. Each left shift by one is equivalent to
			## multiplying by 2 (modulo 4294967296).
			## ```roc
			## expect U32.shift_left_by(1, 3) == 8
			##
			## expect U32.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : U32, U8 -> U32

			## Shift the bits of a [U32] to the right by the given number of positions.
			## Bits shifted past the least significant bit are discarded, and zeros
			## are shifted in on the left. Each right shift by one is equivalent to
			## integer division by 2. For unsigned integers this behaves the same as
			## [shift_right_zf_by].
			## ```roc
			## expect U32.shift_right_by(32, 2) == 8
			##
			## expect U32.shift_right_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_by : U32, U8 -> U32

			## Shift the bits of a [U32] to the right by the given number of positions,
			## filling the vacated high bits with zeros ("zero-fill"). For unsigned
			## integers this behaves the same as [shift_right_by].
			## ```roc
			## expect U32.shift_right_zf_by(32, 2) == 8
			##
			## expect U32.shift_right_zf_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_zf_by : U32, U8 -> U32

			## List of integers beginning with this `U32` and ending with the other `U32`.
			## (Use [until] instead to end with the other `U32` minus one.)
			## Returns an empty list if this `U32` is greater than the other.
			## ```roc
			## expect U32.to(1, 4) == [1, 2, 3, 4]
			##
			## expect U32.to(3, 3) == [3]
			##
			## expect U32.to(5, 2) == []
			## ```
			to : U32, U32 -> List(U32)
			to = |start, end| range_to(start, end)

			## List of integers beginning with this `U32` and ending with the other `U32` minus one.
			## (Use [to] instead to end with the other `U32` exactly, instead of minus one.)
			## Returns an empty list if this `U32` is greater than or equal to the other.
			## ```roc
			## expect U32.until(1, 4) == [1, 2, 3]
			##
			## expect U32.until(3, 3) == []
			##
			## expect U32.until(5, 2) == []
			## ```
			until : U32, U32 -> List(U32)
			until = |start, end| range_until(start, end)

			## Build a [U32] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in a
			## [U32] (`0` to `4294967295`), or if any element is not a valid digit.
			## ```roc
			## expect U32.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(U32, [OutOfRange, ..])

			from_numeral : Numeral -> Try(U32, [InvalidNumeral(Str), ..])

			## Parse a [U32] from a [Str]. Returns `Err(BadNumStr)` if the string is
			## not a valid non-negative integer, or if the parsed value does not fit
			## in a [U32] (`0` to `4294967295`).
			## ```roc
			## expect U32.from_str("42") == Ok(42)
			##
			## expect U32.from_str("-1") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(U32, [BadNumStr, ..])

			# Conversions to signed integers

			## Convert a [U32] to an [I8], wrapping on overflow. Values from `0` to
			## `127` are preserved; larger values wrap by truncating to the low 8
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect U32.to_i8_wrap(42) == 42
			##
			## expect U32.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : U32 -> I8

			## Convert a [U32] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `127` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U32.to_i8_try(42) == Ok(42)
			##
			## expect U32.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : U32 -> Try(I8, [OutOfRange, ..])

			## Convert a [U32] to an [I16], wrapping on overflow. Values from `0` to
			## `32767` are preserved; larger values wrap by truncating to the low 16
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect U32.to_i16_wrap(42) == 42
			##
			## expect U32.to_i16_wrap(40000) == -25536
			## ```
			to_i16_wrap : U32 -> I16

			## Convert a [U32] to an [I16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `32767` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U32.to_i16_try(42) == Ok(42)
			##
			## expect U32.to_i16_try(40000) == Err(OutOfRange)
			## ```
			to_i16_try : U32 -> Try(I16, [OutOfRange, ..])

			## Convert a [U32] to an [I32], wrapping on overflow. Values from `0` to
			## `2147483647` are preserved; values from `2147483648` to `4294967295`
			## wrap into the negative range `-2147483648` to `-1` (two's complement
			## reinterpretation of the bits).
			## ```roc
			## expect U32.to_i32_wrap(42) == 42
			##
			## expect U32.to_i32_wrap(3000000000) == -1294967296
			## ```
			to_i32_wrap : U32 -> I32

			## Convert a [U32] to an [I32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `2147483647` succeed; values from
			## `2147483648` to `4294967295` return `Err(OutOfRange)`.
			## ```roc
			## expect U32.to_i32_try(42) == Ok(42)
			##
			## expect U32.to_i32_try(3000000000) == Err(OutOfRange)
			## ```
			to_i32_try : U32 -> Try(I32, [OutOfRange, ..])
			to_i64 : U32 -> I64
			to_i128 : U32 -> I128

			# Conversions to unsigned integers

			## Convert a [U32] to a [U8], wrapping on overflow. Values from `0` to
			## `255` are preserved; larger values wrap by truncating to the low 8
			## bits.
			## ```roc
			## expect U32.to_u8_wrap(42) == 42
			##
			## expect U32.to_u8_wrap(300) == 44
			## ```
			to_u8_wrap : U32 -> U8

			## Convert a [U32] to a [U8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `255` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U32.to_u8_try(42) == Ok(42)
			##
			## expect U32.to_u8_try(300) == Err(OutOfRange)
			## ```
			to_u8_try : U32 -> Try(U8, [OutOfRange, ..])

			## Convert a [U32] to a [U16], wrapping on overflow. Values from `0` to
			## `65535` are preserved; larger values wrap by truncating to the low 16
			## bits.
			## ```roc
			## expect U32.to_u16_wrap(42) == 42
			##
			## expect U32.to_u16_wrap(70000) == 4464
			## ```
			to_u16_wrap : U32 -> U16

			## Convert a [U32] to a [U16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `65535` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U32.to_u16_try(42) == Ok(42)
			##
			## expect U32.to_u16_try(70000) == Err(OutOfRange)
			## ```
			to_u16_try : U32 -> Try(U16, [OutOfRange, ..])
			to_u64 : U32 -> U64
			to_u128 : U32 -> U128

			# Conversions to floating point (all safe)
			to_f32 : U32 -> F32
			to_f64 : U32 -> F64
			to_dec : U32 -> Dec

			# Encode a U32 using a format that provides encode_u32
			encode : U32, fmt -> Try(encoded, err)
				where [fmt.encode_u32 : fmt, U32 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_u32(self)
			}

			decode : src, fmt -> (Try(U32, err), src)
				where [fmt.decode_u32 : fmt, src -> (Try(U32, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_u32(format, source)
			}
		}

		I32 :: [].{

			## Returns the default [I32] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect I32.default() == 0
			## ```
			default : () -> I32
			default = || 0

			## The highest value representable by an [I32], which is `2147483647`.
			## ```roc
			## expect I32.highest == 2147483647
			## ```
			highest : I32
			highest = 2147483647

			## The lowest value representable by an [I32], which is `-2147483648`.
			## ```roc
			## expect I32.lowest == -2147483648
			## ```
			lowest : I32
			lowest = -2147483648

			## Convert an [I32] to its decimal string representation.
			## ```roc
			## expect I32.to_str(42) == "42"
			##
			## expect I32.to_str(-42) == "-42"
			## ```
			to_str : I32 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect I32.is_zero(0)
			##
			## expect !I32.is_zero(7)
			## ```
			is_zero : I32 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the value is less than `0`.
			## ```roc
			## expect I32.is_negative(-3)
			##
			## expect !I32.is_negative(0)
			## ```
			is_negative : I32 -> Bool
			is_negative = |self| self < 0

			## Returns `Bool.True` if the value is greater than `0`.
			## ```roc
			## expect I32.is_positive(3)
			##
			## expect !I32.is_positive(0)
			## ```
			is_positive : I32 -> Bool
			is_positive = |self| self > 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect I32.is_eq(3, 3)
			##
			## expect !I32.is_eq(3, 4)
			## ```
			is_eq : I32, I32 -> Bool

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect I32.is_gt(5, 3)
			##
			## expect !I32.is_gt(3, 3)
			## ```
			is_gt : I32, I32 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect I32.is_gte(3, 3)
			##
			## expect !I32.is_gte(2, 3)
			## ```
			is_gte : I32, I32 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect I32.is_lt(3, 5)
			##
			## expect !I32.is_lt(3, 3)
			## ```
			is_lt : I32, I32 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect I32.is_lte(3, 3)
			##
			## expect !I32.is_lte(5, 3)
			## ```
			is_lte : I32, I32 -> Bool

			## Returns the greater of two [I32] values.
			## ```roc
			## expect I32.max(5, 3) == 5
			##
			## expect I32.max(-3, -1) == -1
			## ```
			max : I32, I32 -> I32
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [I32] values.
			## ```roc
			## expect I32.min(5, 3) == 3
			##
			## expect I32.min(-3, -1) == -3
			## ```
			min : I32, I32 -> I32
			min = |a, b|
				if a < b
					a
				else
					b

			## Negate an [I32]. Crashes on `-2147483648`, since `2147483648` does not fit in an [I32].
			## ```roc
			## expect I32.negate(3) == -3
			##
			## expect I32.negate(-3) == 3
			## ```
			negate : I32 -> I32

			## Return the absolute value of an [I32]. Crashes on `-2147483648`, since
			## `2147483648` does not fit in an [I32].
			## ```roc
			## expect I32.abs(3) == 3
			##
			## expect I32.abs(-3) == 3
			## ```
			abs : I32 -> I32

			## Add two [I32] values.
			## ```roc
			## expect I32.plus(2, 3) == 5
			## ```
			plus : I32, I32 -> I32

			## Subtract the second [I32] from the first.
			## ```roc
			## expect I32.minus(5, 3) == 2
			## ```
			minus : I32, I32 -> I32

			## Multiply two [I32] values.
			## ```roc
			## expect I32.times(4, 3) == 12
			## ```
			times : I32, I32 -> I32

			## Divide the first [I32] by the second, discarding any remainder. Crashes if the second [I32] is zero.
			## ```roc
			## expect I32.div_by(10, 2) == 5
			##
			## expect I32.div_by(11, 2) == 5
			## ```
			div_by : I32, I32 -> I32

			## Divide the first [I32] by the second, truncating toward zero.
			## ```roc
			## expect I32.div_trunc_by(7, 2) == 3
			##
			## expect I32.div_trunc_by(-7, 2) == -3
			## ```
			div_trunc_by : I32, I32 -> I32

			## Return the remainder of dividing the first [I32] by the second. The
			## sign of the result matches the sign of the dividend.
			## ```roc
			## expect I32.rem_by(7, 3) == 1
			##
			## expect I32.rem_by(-7, 3) == -1
			## ```
			rem_by : I32, I32 -> I32

			## Return the modulus of the first [I32] by the second. The modulus is
			## the remainder left after dividing one number by another, and is
			## always in the range `0` up to (but not including) the absolute value
			## of the divisor. Unlike [rem_by], the sign of the result matches the
			## sign of the divisor.
			## ```roc
			## expect I32.mod_by(7, 3) == 1
			##
			## expect I32.mod_by(-7, 3) == 2
			## ```
			mod_by : I32, I32 -> I32

			## Return the absolute difference between two [I32] values as a [U32].
			## The result is a [U32] because the difference between two [I32] values
			## can be as large as `4294967295`, which does not fit in an [I32].
			## ```roc
			## expect I32.abs_diff(2, 5) == 3
			##
			## expect I32.abs_diff(-1, 5) == 6
			## ```
			abs_diff : I32, I32 -> U32

			## Shift the bits of an [I32] to the left by the given number of
			## positions. Bits shifted past the most significant bit are discarded,
			## and zeros are shifted in on the right.
			## ```roc
			## expect I32.shift_left_by(1, 3) == 8
			##
			## expect I32.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : I32, U8 -> I32

			## Shift the bits of an [I32] to the right by the given number of
			## positions, preserving the sign ("arithmetic shift"). The sign bit
			## is shifted in on the left, so negative values remain negative. Each
			## right shift by one is equivalent to integer division by 2 (rounding
			## toward negative infinity).
			## ```roc
			## expect I32.shift_right_by(32, 2) == 8
			##
			## expect I32.shift_right_by(-32, 2) == -8
			## ```
			shift_right_by : I32, U8 -> I32

			## Shift the bits of an [I32] to the right by the given number of
			## positions.
			## ```roc
			## expect I32.shift_right_zf_by(32, 2) == 8
			##
			## expect I32.shift_right_zf_by(0b0101_0000, 3) == 0b0000_1010
			## ```
			shift_right_zf_by : I32, U8 -> I32

			## List of integers beginning with this `I32` and ending with the other `I32`.
			## (Use [until] instead to end with the other `I32` minus one.)
			## Returns an empty list if this `I32` is greater than the other.
			## ```roc
			## expect I32.to(1, 4) == [1, 2, 3, 4]
			##
			## expect I32.to(-2, 1) == [-2, -1, 0, 1]
			##
			## expect I32.to(5, 2) == []
			## ```
			to : I32, I32 -> List(I32)
			to = |start, end| range_to(start, end)

			## List of integers beginning with this `I32` and ending with the other `I32` minus one.
			## (Use [to] instead to end with the other `I32` exactly, instead of minus one.)
			## Returns an empty list if this `I32` is greater than or equal to the other.
			## ```roc
			## expect I32.until(1, 4) == [1, 2, 3]
			##
			## expect I32.until(-2, 1) == [-2, -1, 0]
			##
			## expect I32.until(5, 2) == []
			## ```
			until : I32, I32 -> List(I32)
			until = |start, end| range_until(start, end)

			## Build an [I32] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in an
			## [I32] (`-2147483648` to `2147483647`), or if any element is not a valid digit.
			## The result is always non-negative; to build a negative value, [negate]
			## the result.
			## ```roc
			## expect I32.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(I32, [OutOfRange, ..])

			from_numeral : Numeral -> Try(I32, [InvalidNumeral(Str), ..])

			## Parse an [I32] from a [Str]. Returns `Err(BadNumStr)` if the string
			## is not a valid integer, or if the parsed value does not fit in an
			## [I32] (`-2147483648` to `2147483647`).
			## ```roc
			## expect I32.from_str("42") == Ok(42)
			##
			## expect I32.from_str("-1") == Ok(-1)
			##
			## expect I32.from_str("3000000000") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(I32, [BadNumStr, ..])

			# Conversions to signed integers

			## Convert an [I32] to an [I8], wrapping on overflow. Values from `-128`
			## to `127` are preserved; other values wrap by truncating to the low 8
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect I32.to_i8_wrap(42) == 42
			##
			## expect I32.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : I32 -> I8

			## Convert an [I32] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-128` to `127` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I32.to_i8_try(42) == Ok(42)
			##
			## expect I32.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : I32 -> Try(I8, [OutOfRange, ..])

			## Convert an [I32] to an [I16], wrapping on overflow. Values from
			## `-32768` to `32767` are preserved; other values wrap by truncating
			## to the low 16 bits and reinterpreting them in two's complement.
			## ```roc
			## expect I32.to_i16_wrap(42) == 42
			##
			## expect I32.to_i16_wrap(40000) == -25536
			## ```
			to_i16_wrap : I32 -> I16

			## Convert an [I32] to an [I16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-32768` to `32767` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I32.to_i16_try(42) == Ok(42)
			##
			## expect I32.to_i16_try(40000) == Err(OutOfRange)
			## ```
			to_i16_try : I32 -> Try(I16, [OutOfRange, ..])
			to_i64 : I32 -> I64
			to_i128 : I32 -> I128

			# Conversions to unsigned integers (all lossy for negative values)

			## Convert an [I32] to a [U8], wrapping on overflow. Values from `0` to
			## `255` are preserved; other values wrap by truncating to the low 8
			## bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I32.to_u8_wrap(42) == 42
			##
			## expect I32.to_u8_wrap(-1) == 255
			## ```
			to_u8_wrap : I32 -> U8

			## Convert an [I32] to a [U8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `255` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I32.to_u8_try(42) == Ok(42)
			##
			## expect I32.to_u8_try(-1) == Err(OutOfRange)
			## ```
			to_u8_try : I32 -> Try(U8, [OutOfRange, ..])

			## Convert an [I32] to a [U16], wrapping on overflow. Values from `0` to
			## `65535` are preserved; other values wrap by truncating to the low 16
			## bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I32.to_u16_wrap(42) == 42
			##
			## expect I32.to_u16_wrap(-1) == 65535
			## ```
			to_u16_wrap : I32 -> U16

			## Convert an [I32] to a [U16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `65535` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I32.to_u16_try(42) == Ok(42)
			##
			## expect I32.to_u16_try(-1) == Err(OutOfRange)
			## ```
			to_u16_try : I32 -> Try(U16, [OutOfRange, ..])

			## Convert an [I32] to a [U32], wrapping on overflow. Non-negative
			## values are preserved; negative values wrap into the upper end of the
			## [U32] range (two's complement reinterpretation of the bits).
			## ```roc
			## expect I32.to_u32_wrap(42) == 42
			##
			## expect I32.to_u32_wrap(-1) == 4294967295
			## ```
			to_u32_wrap : I32 -> U32

			## Convert an [I32] to a [U32], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I32.to_u32_try(42) == Ok(42)
			##
			## expect I32.to_u32_try(-1) == Err(OutOfRange)
			## ```
			to_u32_try : I32 -> Try(U32, [OutOfRange, ..])

			## Convert an [I32] to a [U64], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U64] range (two's complement reinterpretation of
			## the sign-extended bits).
			## ```roc
			## expect I32.to_u64_wrap(42) == 42
			##
			## expect I32.to_u64_wrap(-1) == 18446744073709551615
			## ```
			to_u64_wrap : I32 -> U64

			## Convert an [I32] to a [U64], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I32.to_u64_try(42) == Ok(42)
			##
			## expect I32.to_u64_try(-1) == Err(OutOfRange)
			## ```
			to_u64_try : I32 -> Try(U64, [OutOfRange, ..])

			## Convert an [I32] to a [U128], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U128] range (two's complement reinterpretation
			## of the sign-extended bits).
			## ```roc
			## expect I32.to_u128_wrap(42) == 42
			## ```
			to_u128_wrap : I32 -> U128

			## Convert an [I32] to a [U128], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I32.to_u128_try(42) == Ok(42)
			##
			## expect I32.to_u128_try(-1) == Err(OutOfRange)
			## ```
			to_u128_try : I32 -> Try(U128, [OutOfRange, ..])

			# Conversions to floating point (all safe)
			to_f32 : I32 -> F32
			to_f64 : I32 -> F64
			to_dec : I32 -> Dec

			# Encode an I32 using a format that provides encode_i32
			encode : I32, fmt -> Try(encoded, err)
				where [fmt.encode_i32 : fmt, I32 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_i32(self)
			}

			# Decode an I32 using a format that provides decode_i32
			decode : src, fmt -> (Try(I32, err), src)
				where [fmt.decode_i32 : fmt, src -> (Try(I32, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_i32(format, source)
			}
		}

		U64 :: [].{

			## Returns the default [U64] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect U64.default() == 0
			## ```
			default : () -> U64
			default = || 0

			## The highest value representable by a [U64], which is
			## `18446744073709551615`.
			## ```roc
			## expect U64.highest == 18446744073709551615
			## ```
			highest : U64
			highest = 18446744073709551615

			## The lowest value representable by a [U64], which is `0`.
			## ```roc
			## expect U64.lowest == 0
			## ```
			lowest : U64
			lowest = 0

			## Convert a [U64] to its decimal string representation.
			## ```roc
			## expect U64.to_str(42) == "42"
			## ```
			to_str : U64 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect U64.is_zero(0)
			##
			## expect !U64.is_zero(7)
			## ```
			is_zero : U64 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect U64.is_eq(3, 3)
			##
			## expect !U64.is_eq(3, 4)
			## ```
			is_eq : U64, U64 -> Bool

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect U64.is_gt(5, 3)
			##
			## expect !U64.is_gt(3, 3)
			## ```
			is_gt : U64, U64 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect U64.is_gte(3, 3)
			##
			## expect !U64.is_gte(2, 3)
			## ```
			is_gte : U64, U64 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect U64.is_lt(3, 5)
			##
			## expect !U64.is_lt(3, 3)
			## ```
			is_lt : U64, U64 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect U64.is_lte(3, 3)
			##
			## expect !U64.is_lte(5, 3)
			## ```
			is_lte : U64, U64 -> Bool

			## Returns the greater of two [U64] values.
			## ```roc
			## expect U64.max(5, 3) == 5
			## ```
			max : U64, U64 -> U64
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [U64] values.
			## ```roc
			## expect U64.min(5, 3) == 3
			## ```
			min : U64, U64 -> U64
			min = |a, b|
				if a < b
					a
				else
					b

			## Add two [U64] values.
			## ```roc
			## expect U64.plus(2, 3) == 5
			## ```
			plus : U64, U64 -> U64

			## Subtract the second [U64] from the first.
			## ```roc
			## expect U64.minus(5, 3) == 2
			## ```
			minus : U64, U64 -> U64

			## Multiply two [U64] values.
			## ```roc
			## expect U64.times(4, 3) == 12
			## ```
			times : U64, U64 -> U64

			## Divide the first [U64] by the second, discarding any remainder. Crashes if the second [U64] is zero.
			## ```roc
			## expect U64.div_by(10, 2) == 5
			##
			## expect U64.div_by(11, 2) == 5
			## ```
			div_by : U64, U64 -> U64

			## Divide the first [U64] by the second, truncating down (toward zero). For unsigned
			## integers this behaves the same as [div_by].
			## ```roc
			## expect U64.div_trunc_by(7, 2) == 3
			## ```
			div_trunc_by : U64, U64 -> U64

			## Return the remainder of dividing the first [U64] by the second.
			## ```roc
			## expect U64.rem_by(7, 3) == 1
			## ```
			rem_by : U64, U64 -> U64

			## Return the modulus of the first [U64] by the second. The modulus is the
			## remainder left after dividing one number by another, and is always in
			## the range `0` up to (but not including) the divisor. For unsigned
			## integers this behaves the same as [rem_by].
			## ```roc
			## expect U64.mod_by(7, 3) == 1
			## ```
			mod_by : U64, U64 -> U64

			## Return the absolute difference between two [U64] values.
			## ```roc
			## expect U64.abs_diff(2, 5) == 3
			##
			## expect U64.abs_diff(5, 2) == 3
			## ```
			abs_diff : U64, U64 -> U64

			## Shift the bits of a [U64] to the left by the given number of positions.
			## Bits shifted past the most significant bit are discarded, and zeros
			## are shifted in on the right. Each left shift by one is equivalent to
			## multiplying by 2 (modulo 2^64).
			## ```roc
			## expect U64.shift_left_by(1, 3) == 8
			##
			## expect U64.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : U64, U8 -> U64

			## Shift the bits of a [U64] to the right by the given number of positions.
			## Bits shifted past the least significant bit are discarded, and zeros
			## are shifted in on the left. Each right shift by one is equivalent to
			## integer division by 2. For unsigned integers this behaves the same as
			## [shift_right_zf_by].
			## ```roc
			## expect U64.shift_right_by(32, 2) == 8
			##
			## expect U64.shift_right_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_by : U64, U8 -> U64

			## Shift the bits of a [U64] to the right by the given number of positions,
			## filling the vacated high bits with zeros ("zero-fill"). For unsigned
			## integers this behaves the same as [shift_right_by].
			## ```roc
			## expect U64.shift_right_zf_by(32, 2) == 8
			##
			## expect U64.shift_right_zf_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_zf_by : U64, U8 -> U64

			## List of integers beginning with this `U64` and ending with the other `U64`.
			## (Use [until] instead to end with the other `U64` minus one.)
			## Returns an empty list if this `U64` is greater than the other.
			## ```roc
			## expect U64.to(1, 4) == [1, 2, 3, 4]
			##
			## expect U64.to(3, 3) == [3]
			##
			## expect U64.to(5, 2) == []
			## ```
			to : U64, U64 -> List(U64)
			to = |start, end| range_to(start, end)

			## List of integers beginning with this `U64` and ending with the other `U64` minus one.
			## (Use [to] instead to end with the other `U64` exactly, instead of minus one.)
			## Returns an empty list if this `U64` is greater than or equal to the other.
			## ```roc
			## expect U64.until(1, 4) == [1, 2, 3]
			##
			## expect U64.until(3, 3) == []
			##
			## expect U64.until(5, 2) == []
			## ```
			until : U64, U64 -> List(U64)
			until = |start, end| range_until(start, end)

			## Build a [U64] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in a
			## [U64] (`0` to `18446744073709551615`), or if any element is not a valid digit.
			## ```roc
			## expect U64.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(U64, [OutOfRange, ..])

			from_numeral : Numeral -> Try(U64, [InvalidNumeral(Str), ..])

			## Parse a [U64] from a [Str]. Returns `Err(BadNumStr)` if the string is
			## not a valid non-negative integer, or if the parsed value does not fit
			## in a [U64] (`0` to `18446744073709551615`).
			## ```roc
			## expect U64.from_str("42") == Ok(42)
			##
			## expect U64.from_str("-1") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(U64, [BadNumStr, ..])

			# Conversions to signed integers

			## Convert a [U64] to an [I8], wrapping on overflow. Values from `0` to
			## `127` are preserved; larger values wrap by truncating to the low 8
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect U64.to_i8_wrap(42) == 42
			##
			## expect U64.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : U64 -> I8

			## Convert a [U64] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `127` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U64.to_i8_try(42) == Ok(42)
			##
			## expect U64.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : U64 -> Try(I8, [OutOfRange, ..])

			## Convert a [U64] to an [I16], wrapping on overflow. Values from `0` to
			## `32767` are preserved; larger values wrap by truncating to the low 16
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect U64.to_i16_wrap(42) == 42
			##
			## expect U64.to_i16_wrap(40000) == -25536
			## ```
			to_i16_wrap : U64 -> I16

			## Convert a [U64] to an [I16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `32767` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U64.to_i16_try(42) == Ok(42)
			##
			## expect U64.to_i16_try(40000) == Err(OutOfRange)
			## ```
			to_i16_try : U64 -> Try(I16, [OutOfRange, ..])

			## Convert a [U64] to an [I32], wrapping on overflow. Values from `0` to
			## `2147483647` are preserved; larger values wrap by truncating to the
			## low 32 bits and reinterpreting them in two's complement.
			## ```roc
			## expect U64.to_i32_wrap(42) == 42
			##
			## expect U64.to_i32_wrap(3000000000) == -1294967296
			## ```
			to_i32_wrap : U64 -> I32

			## Convert a [U64] to an [I32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `2147483647` succeed; larger values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect U64.to_i32_try(42) == Ok(42)
			##
			## expect U64.to_i32_try(3000000000) == Err(OutOfRange)
			## ```
			to_i32_try : U64 -> Try(I32, [OutOfRange, ..])

			## Convert a [U64] to an [I64], wrapping on overflow. Values from `0` to
			## `9223372036854775807` are preserved; values from `9223372036854775808`
			## to `18446744073709551615` wrap into the negative range
			## `-9223372036854775808` to `-1` (two's complement reinterpretation of
			## the bits).
			## ```roc
			## expect U64.to_i64_wrap(42) == 42
			##
			## expect U64.to_i64_wrap(10000000000000000000) == -8446744073709551616
			## ```
			to_i64_wrap : U64 -> I64

			## Convert a [U64] to an [I64], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `9223372036854775807` succeed; larger
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect U64.to_i64_try(42) == Ok(42)
			##
			## expect U64.to_i64_try(10000000000000000000) == Err(OutOfRange)
			## ```
			to_i64_try : U64 -> Try(I64, [OutOfRange, ..])
			to_i128 : U64 -> I128

			# Conversions to unsigned integers

			## Convert a [U64] to a [U8], wrapping on overflow. Values from `0` to
			## `255` are preserved; larger values wrap by truncating to the low 8
			## bits.
			## ```roc
			## expect U64.to_u8_wrap(42) == 42
			##
			## expect U64.to_u8_wrap(300) == 44
			## ```
			to_u8_wrap : U64 -> U8

			## Convert a [U64] to a [U8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `255` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U64.to_u8_try(42) == Ok(42)
			##
			## expect U64.to_u8_try(300) == Err(OutOfRange)
			## ```
			to_u8_try : U64 -> Try(U8, [OutOfRange, ..])

			## Convert a [U64] to a [U16], wrapping on overflow. Values from `0` to
			## `65535` are preserved; larger values wrap by truncating to the low 16
			## bits.
			## ```roc
			## expect U64.to_u16_wrap(42) == 42
			##
			## expect U64.to_u16_wrap(70000) == 4464
			## ```
			to_u16_wrap : U64 -> U16

			## Convert a [U64] to a [U16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `65535` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U64.to_u16_try(42) == Ok(42)
			##
			## expect U64.to_u16_try(70000) == Err(OutOfRange)
			## ```
			to_u16_try : U64 -> Try(U16, [OutOfRange, ..])

			## Convert a [U64] to a [U32], wrapping on overflow. Values from `0` to
			## `4294967295` are preserved; larger values wrap by truncating to the
			## low 32 bits.
			## ```roc
			## expect U64.to_u32_wrap(42) == 42
			##
			## expect U64.to_u32_wrap(5000000000) == 705032704
			## ```
			to_u32_wrap : U64 -> U32

			## Convert a [U64] to a [U32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `4294967295` succeed; larger values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect U64.to_u32_try(42) == Ok(42)
			##
			## expect U64.to_u32_try(5000000000) == Err(OutOfRange)
			## ```
			to_u32_try : U64 -> Try(U32, [OutOfRange, ..])
			to_u128 : U64 -> U128

			# Conversions to floating point (all safe)
			to_f32 : U64 -> F32
			to_f64 : U64 -> F64
			to_dec : U64 -> Dec

			# Encode a U64 using a format that provides encode_u64
			encode : U64, fmt -> Try(encoded, err)
				where [fmt.encode_u64 : fmt, U64 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_u64(self)
			}

			decode : src, fmt -> (Try(U64, err), src)
				where [fmt.decode_u64 : fmt, src -> (Try(U64, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_u64(format, source)
			}
		}

		I64 :: [].{

			## Returns the default [I64] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect I64.default() == 0
			## ```
			default : () -> I64
			default = || 0

			## The highest value representable by an [I64], which is
			## `9223372036854775807`.
			## ```roc
			## expect I64.highest == 9223372036854775807
			## ```
			highest : I64
			highest = 9223372036854775807

			## The lowest value representable by an [I64], which is
			## `-9223372036854775808`.
			## ```roc
			## expect I64.lowest == -9223372036854775808
			## ```
			lowest : I64
			lowest = -9223372036854775808

			## Convert an [I64] to its decimal string representation.
			## ```roc
			## expect I64.to_str(42) == "42"
			##
			## expect I64.to_str(-42) == "-42"
			## ```
			to_str : I64 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect I64.is_zero(0)
			##
			## expect !I64.is_zero(7)
			## ```
			is_zero : I64 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the value is less than `0`.
			## ```roc
			## expect I64.is_negative(-3)
			##
			## expect !I64.is_negative(0)
			## ```
			is_negative : I64 -> Bool
			is_negative = |self| self < 0

			## Returns `Bool.True` if the value is greater than `0`.
			## ```roc
			## expect I64.is_positive(3)
			##
			## expect !I64.is_positive(0)
			## ```
			is_positive : I64 -> Bool
			is_positive = |self| self > 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect I64.is_eq(3, 3)
			##
			## expect !I64.is_eq(3, 4)
			## ```
			is_eq : I64, I64 -> Bool

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect I64.is_gt(5, 3)
			##
			## expect !I64.is_gt(3, 3)
			## ```
			is_gt : I64, I64 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect I64.is_gte(3, 3)
			##
			## expect !I64.is_gte(2, 3)
			## ```
			is_gte : I64, I64 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect I64.is_lt(3, 5)
			##
			## expect !I64.is_lt(3, 3)
			## ```
			is_lt : I64, I64 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect I64.is_lte(3, 3)
			##
			## expect !I64.is_lte(5, 3)
			## ```
			is_lte : I64, I64 -> Bool

			## Returns the greater of two [I64] values.
			## ```roc
			## expect I64.max(5, 3) == 5
			##
			## expect I64.max(-3, -1) == -1
			## ```
			max : I64, I64 -> I64
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [I64] values.
			## ```roc
			## expect I64.min(5, 3) == 3
			##
			## expect I64.min(-3, -1) == -3
			## ```
			min : I64, I64 -> I64
			min = |a, b|
				if a < b
					a
				else
					b

			## Negate an [I64]. Crashes on `-9223372036854775808`, since
			## `9223372036854775808` does not fit in an [I64].
			## ```roc
			## expect I64.negate(3) == -3
			##
			## expect I64.negate(-3) == 3
			## ```
			negate : I64 -> I64

			## Return the absolute value of an [I64]. Crashes on `-9223372036854775808`,
			## since `9223372036854775808` does not fit in an [I64].
			## ```roc
			## expect I64.abs(3) == 3
			##
			## expect I64.abs(-3) == 3
			## ```
			abs : I64 -> I64

			## Add two [I64] values.
			## ```roc
			## expect I64.plus(2, 3) == 5
			## ```
			plus : I64, I64 -> I64

			## Subtract the second [I64] from the first.
			## ```roc
			## expect I64.minus(5, 3) == 2
			## ```
			minus : I64, I64 -> I64

			## Multiply two [I64] values.
			## ```roc
			## expect I64.times(4, 3) == 12
			## ```
			times : I64, I64 -> I64

			## Divide the first [I64] by the second, discarding any remainder. Crashes if the second [I64] is zero.
			## ```roc
			## expect I64.div_by(10, 2) == 5
			##
			## expect I64.div_by(11, 2) == 5
			## ```
			div_by : I64, I64 -> I64

			## Divide the first [I64] by the second, truncating toward zero.
			## ```roc
			## expect I64.div_trunc_by(7, 2) == 3
			##
			## expect I64.div_trunc_by(-7, 2) == -3
			## ```
			div_trunc_by : I64, I64 -> I64

			## Return the remainder of dividing the first [I64] by the second. The
			## sign of the result matches the sign of the dividend.
			## ```roc
			## expect I64.rem_by(7, 3) == 1
			##
			## expect I64.rem_by(-7, 3) == -1
			## ```
			rem_by : I64, I64 -> I64

			## Return the modulus of the first [I64] by the second. The modulus is
			## the remainder left after dividing one number by another, and is
			## always in the range `0` up to (but not including) the absolute value
			## of the divisor. Unlike [rem_by], the sign of the result matches the
			## sign of the divisor.
			## ```roc
			## expect I64.mod_by(7, 3) == 1
			##
			## expect I64.mod_by(-7, 3) == 2
			## ```
			mod_by : I64, I64 -> I64

			## Return the absolute difference between two [I64] values as a [U64].
			## The result is a [U64] because the difference between two [I64] values
			## can be as large as `18446744073709551615`, which does not fit in an [I64].
			## ```roc
			## expect I64.abs_diff(2, 5) == 3
			##
			## expect I64.abs_diff(-1, 5) == 6
			## ```
			abs_diff : I64, I64 -> U64

			## Shift the bits of an [I64] to the left by the given number of
			## positions. Bits shifted past the most significant bit are discarded,
			## and zeros are shifted in on the right.
			## ```roc
			## expect I64.shift_left_by(1, 3) == 8
			##
			## expect I64.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : I64, U8 -> I64

			## Shift the bits of an [I64] to the right by the given number of
			## positions, preserving the sign ("arithmetic shift"). The sign bit
			## is shifted in on the left, so negative values remain negative. Each
			## right shift by one is equivalent to integer division by 2 (rounding
			## toward negative infinity).
			## ```roc
			## expect I64.shift_right_by(32, 2) == 8
			##
			## expect I64.shift_right_by(-32, 2) == -8
			## ```
			shift_right_by : I64, U8 -> I64

			## Shift the bits of an [I64] to the right by the given number of
			## positions.
			## ```roc
			## expect I64.shift_right_zf_by(32, 2) == 8
			##
			## expect I64.shift_right_zf_by(0b0101_0000, 3) == 0b0000_1010
			## ```
			shift_right_zf_by : I64, U8 -> I64

			## List of integers beginning with this `I64` and ending with the other `I64`.
			## (Use [until] instead to end with the other `I64` minus one.)
			## Returns an empty list if this `I64` is greater than the other.
			## ```roc
			## expect I64.to(1, 4) == [1, 2, 3, 4]
			##
			## expect I64.to(-2, 1) == [-2, -1, 0, 1]
			##
			## expect I64.to(5, 2) == []
			## ```
			to : I64, I64 -> List(I64)
			to = |start, end| range_to(start, end)

			## List of integers beginning with this `I64` and ending with the other `I64` minus one.
			## (Use [to] instead to end with the other `I64` exactly, instead of minus one.)
			## Returns an empty list if this `I64` is greater than or equal to the other.
			## ```roc
			## expect I64.until(1, 4) == [1, 2, 3]
			##
			## expect I64.until(-2, 1) == [-2, -1, 0]
			##
			## expect I64.until(5, 2) == []
			## ```
			until : I64, I64 -> List(I64)
			until = |start, end| range_until(start, end)

			## Build an [I64] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in an
			## [I64] (`-9223372036854775808` to `9223372036854775807`), or if any
			## element is not a valid digit. The result is always non-negative; to
			## build a negative value, [negate] the result.
			## ```roc
			## expect I64.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(I64, [OutOfRange, ..])

			from_numeral : Numeral -> Try(I64, [InvalidNumeral(Str), ..])

			## Parse an [I64] from a [Str]. Returns `Err(BadNumStr)` if the string
			## is not a valid integer, or if the parsed value does not fit in an
			## [I64] (`-9223372036854775808` to `9223372036854775807`).
			## ```roc
			## expect I64.from_str("42") == Ok(42)
			##
			## expect I64.from_str("-1") == Ok(-1)
			## ```
			from_str : Str -> Try(I64, [BadNumStr, ..])

			# Conversions to signed integers

			## Convert an [I64] to an [I8], wrapping on overflow. Values from `-128`
			## to `127` are preserved; other values wrap by truncating to the low 8
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect I64.to_i8_wrap(42) == 42
			##
			## expect I64.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : I64 -> I8

			## Convert an [I64] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-128` to `127` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I64.to_i8_try(42) == Ok(42)
			##
			## expect I64.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : I64 -> Try(I8, [OutOfRange, ..])

			## Convert an [I64] to an [I16], wrapping on overflow. Values from
			## `-32768` to `32767` are preserved; other values wrap by truncating
			## to the low 16 bits and reinterpreting them in two's complement.
			## ```roc
			## expect I64.to_i16_wrap(42) == 42
			##
			## expect I64.to_i16_wrap(40000) == -25536
			## ```
			to_i16_wrap : I64 -> I16

			## Convert an [I64] to an [I16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-32768` to `32767` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I64.to_i16_try(42) == Ok(42)
			##
			## expect I64.to_i16_try(40000) == Err(OutOfRange)
			## ```
			to_i16_try : I64 -> Try(I16, [OutOfRange, ..])

			## Convert an [I64] to an [I32], wrapping on overflow. Values from
			## `-2147483648` to `2147483647` are preserved; other values wrap by
			## truncating to the low 32 bits and reinterpreting them in two's
			## complement.
			## ```roc
			## expect I64.to_i32_wrap(42) == 42
			##
			## expect I64.to_i32_wrap(3000000000) == -1294967296
			## ```
			to_i32_wrap : I64 -> I32

			## Convert an [I64] to an [I32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-2147483648` to `2147483647` succeed; other
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect I64.to_i32_try(42) == Ok(42)
			##
			## expect I64.to_i32_try(3000000000) == Err(OutOfRange)
			## ```
			to_i32_try : I64 -> Try(I32, [OutOfRange, ..])
			to_i128 : I64 -> I128

			# Conversions to unsigned integers (all lossy for negative values)

			## Convert an [I64] to a [U8], wrapping on overflow. Values from `0` to
			## `255` are preserved; other values wrap by truncating to the low 8
			## bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I64.to_u8_wrap(42) == 42
			##
			## expect I64.to_u8_wrap(-1) == 255
			## ```
			to_u8_wrap : I64 -> U8

			## Convert an [I64] to a [U8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `255` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I64.to_u8_try(42) == Ok(42)
			##
			## expect I64.to_u8_try(-1) == Err(OutOfRange)
			## ```
			to_u8_try : I64 -> Try(U8, [OutOfRange, ..])

			## Convert an [I64] to a [U16], wrapping on overflow. Values from `0` to
			## `65535` are preserved; other values wrap by truncating to the low 16
			## bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I64.to_u16_wrap(42) == 42
			##
			## expect I64.to_u16_wrap(-1) == 65535
			## ```
			to_u16_wrap : I64 -> U16

			## Convert an [I64] to a [U16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `65535` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I64.to_u16_try(42) == Ok(42)
			##
			## expect I64.to_u16_try(-1) == Err(OutOfRange)
			## ```
			to_u16_try : I64 -> Try(U16, [OutOfRange, ..])

			## Convert an [I64] to a [U32], wrapping on overflow. Values from `0` to
			## `4294967295` are preserved; other values wrap by truncating to the
			## low 32 bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I64.to_u32_wrap(42) == 42
			##
			## expect I64.to_u32_wrap(-1) == 4294967295
			## ```
			to_u32_wrap : I64 -> U32

			## Convert an [I64] to a [U32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `4294967295` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I64.to_u32_try(42) == Ok(42)
			##
			## expect I64.to_u32_try(-1) == Err(OutOfRange)
			## ```
			to_u32_try : I64 -> Try(U32, [OutOfRange, ..])

			## Convert an [I64] to a [U64], wrapping on overflow. Non-negative
			## values are preserved; negative values wrap into the upper end of the
			## [U64] range (two's complement reinterpretation of the bits).
			## ```roc
			## expect I64.to_u64_wrap(42) == 42
			##
			## expect I64.to_u64_wrap(-1) == 18446744073709551615
			## ```
			to_u64_wrap : I64 -> U64

			## Convert an [I64] to a [U64], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I64.to_u64_try(42) == Ok(42)
			##
			## expect I64.to_u64_try(-1) == Err(OutOfRange)
			## ```
			to_u64_try : I64 -> Try(U64, [OutOfRange, ..])

			## Convert an [I64] to a [U128], sign-extending the bits on overflow.
			## Non-negative values are preserved; negative values wrap into the
			## upper end of the [U128] range (two's complement reinterpretation
			## of the sign-extended bits).
			## ```roc
			## expect I64.to_u128_wrap(42) == 42
			## ```
			to_u128_wrap : I64 -> U128

			## Convert an [I64] to a [U128], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I64.to_u128_try(42) == Ok(42)
			##
			## expect I64.to_u128_try(-1) == Err(OutOfRange)
			## ```
			to_u128_try : I64 -> Try(U128, [OutOfRange, ..])

			# Conversions to floating point (all safe)
			to_f32 : I64 -> F32
			to_f64 : I64 -> F64
			to_dec : I64 -> Dec

			# Encode an I64 using a format that provides encode_i64
			encode : I64, fmt -> Try(encoded, err)
				where [fmt.encode_i64 : fmt, I64 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_i64(self)
			}

			decode : src, fmt -> (Try(I64, err), src)
				where [fmt.decode_i64 : fmt, src -> (Try(I64, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_i64(format, source)
			}
		}

		U128 :: [].{

			## Returns the default [U128] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect U128.default() == 0
			## ```
			default : () -> U128
			default = || 0

			## The highest value representable by a [U128], which is
			## `340282366920938463463374607431768211455`.
			## ```roc
			## expect U128.highest == 340282366920938463463374607431768211455
			## ```
			highest : U128
			highest = 340282366920938463463374607431768211455

			## The lowest value representable by a [U128], which is `0`.
			## ```roc
			## expect U128.lowest == 0
			## ```
			lowest : U128
			lowest = 0

			## Convert a [U128] to its decimal string representation.
			## ```roc
			## expect U128.to_str(42) == "42"
			## ```
			to_str : U128 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect U128.is_zero(0)
			##
			## expect !U128.is_zero(7)
			## ```
			is_zero : U128 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect U128.is_eq(3, 3)
			##
			## expect !U128.is_eq(3, 4)
			## ```
			is_eq : U128, U128 -> Bool

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect U128.is_gt(5, 3)
			##
			## expect !U128.is_gt(3, 3)
			## ```
			is_gt : U128, U128 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect U128.is_gte(3, 3)
			##
			## expect !U128.is_gte(2, 3)
			## ```
			is_gte : U128, U128 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect U128.is_lt(3, 5)
			##
			## expect !U128.is_lt(3, 3)
			## ```
			is_lt : U128, U128 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect U128.is_lte(3, 3)
			##
			## expect !U128.is_lte(5, 3)
			## ```
			is_lte : U128, U128 -> Bool

			## Returns the greater of two [U128] values.
			## ```roc
			## expect U128.max(5, 3) == 5
			## ```
			max : U128, U128 -> U128
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [U128] values.
			## ```roc
			## expect U128.min(5, 3) == 3
			## ```
			min : U128, U128 -> U128
			min = |a, b|
				if a < b
					a
				else
					b

			## Add two [U128] values.
			## ```roc
			## expect U128.plus(2, 3) == 5
			## ```
			plus : U128, U128 -> U128

			## Subtract the second [U128] from the first.
			## ```roc
			## expect U128.minus(5, 3) == 2
			## ```
			minus : U128, U128 -> U128

			## Multiply two [U128] values.
			## ```roc
			## expect U128.times(4, 3) == 12
			## ```
			times : U128, U128 -> U128

			## Divide the first [U128] by the second, discarding any remainder. Crashes if the second [U128] is zero.
			## ```roc
			## expect U128.div_by(10, 2) == 5
			##
			## expect U128.div_by(11, 2) == 5
			## ```
			div_by : U128, U128 -> U128

			## Divide the first [U128] by the second, truncating down (toward zero). For unsigned
			## integers this behaves the same as [div_by].
			## ```roc
			## expect U128.div_trunc_by(7, 2) == 3
			## ```
			div_trunc_by : U128, U128 -> U128

			## Return the remainder of dividing the first [U128] by the second.
			## ```roc
			## expect U128.rem_by(7, 3) == 1
			## ```
			rem_by : U128, U128 -> U128

			## Return the modulus of the first [U128] by the second. The modulus is the
			## remainder left after dividing one number by another, and is always in
			## the range `0` up to (but not including) the divisor. For unsigned
			## integers this behaves the same as [rem_by].
			## ```roc
			## expect U128.mod_by(7, 3) == 1
			## ```
			mod_by : U128, U128 -> U128

			## Return the absolute difference between two [U128] values.
			## ```roc
			## expect U128.abs_diff(2, 5) == 3
			##
			## expect U128.abs_diff(5, 2) == 3
			## ```
			abs_diff : U128, U128 -> U128

			## Shift the bits of a [U128] to the left by the given number of positions.
			## Bits shifted past the most significant bit are discarded, and zeros
			## are shifted in on the right. Each left shift by one is equivalent to
			## multiplying by 2 (modulo 2^128).
			## ```roc
			## expect U128.shift_left_by(1, 3) == 8
			##
			## expect U128.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : U128, U8 -> U128

			## Shift the bits of a [U128] to the right by the given number of positions.
			## Bits shifted past the least significant bit are discarded, and zeros
			## are shifted in on the left. Each right shift by one is equivalent to
			## integer division by 2. For unsigned integers this behaves the same as
			## [shift_right_zf_by].
			## ```roc
			## expect U128.shift_right_by(32, 2) == 8
			##
			## expect U128.shift_right_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_by : U128, U8 -> U128

			## Shift the bits of a [U128] to the right by the given number of positions,
			## filling the vacated high bits with zeros ("zero-fill"). For unsigned
			## integers this behaves the same as [shift_right_by].
			## ```roc
			## expect U128.shift_right_zf_by(32, 2) == 8
			##
			## expect U128.shift_right_zf_by(0b1010_0000, 3) == 0b0001_0100
			## ```
			shift_right_zf_by : U128, U8 -> U128

			## List of integers beginning with this `U128` and ending with the other `U128`.
			## (Use [until] instead to end with the other `U128` minus one.)
			## Returns an empty list if this `U128` is greater than the other.
			## ```roc
			## expect U128.to(1, 4) == [1, 2, 3, 4]
			##
			## expect U128.to(3, 3) == [3]
			##
			## expect U128.to(5, 2) == []
			## ```
			to : U128, U128 -> List(U128)
			to = |start, end| range_to(start, end)

			## List of integers beginning with this `U128` and ending with the other `U128` minus one.
			## (Use [to] instead to end with the other `U128` exactly, instead of minus one.)
			## Returns an empty list if this `U128` is greater than or equal to the other.
			## ```roc
			## expect U128.until(1, 4) == [1, 2, 3]
			##
			## expect U128.until(3, 3) == []
			##
			## expect U128.until(5, 2) == []
			## ```
			until : U128, U128 -> List(U128)
			until = |start, end| range_until(start, end)

			## Build a [U128] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in a
			## [U128] (`0` to `340282366920938463463374607431768211455`), or if any
			## element is not a valid digit.
			## ```roc
			## expect U128.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(U128, [OutOfRange, ..])

			from_numeral : Numeral -> Try(U128, [InvalidNumeral(Str), ..])

			## Parse a [U128] from a [Str]. Returns `Err(BadNumStr)` if the string is
			## not a valid non-negative integer, or if the parsed value does not fit
			## in a [U128] (`0` to `340282366920938463463374607431768211455`).
			## ```roc
			## expect U128.from_str("42") == Ok(42)
			##
			## expect U128.from_str("-1") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(U128, [BadNumStr, ..])

			# Conversions to signed integers

			## Convert a [U128] to an [I8], wrapping on overflow. Values from `0` to
			## `127` are preserved; larger values wrap by truncating to the low 8
			## bits and reinterpreting them in two's complement.
			## ```roc
			## expect U128.to_i8_wrap(42) == 42
			##
			## expect U128.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : U128 -> I8

			## Convert a [U128] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `127` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_i8_try(42) == Ok(42)
			##
			## expect U128.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : U128 -> Try(I8, [OutOfRange, ..])

			## Convert a [U128] to an [I16], wrapping on overflow. Values from `0`
			## to `32767` are preserved; larger values wrap by truncating to the
			## low 16 bits and reinterpreting them in two's complement.
			## ```roc
			## expect U128.to_i16_wrap(42) == 42
			##
			## expect U128.to_i16_wrap(40000) == -25536
			## ```
			to_i16_wrap : U128 -> I16

			## Convert a [U128] to an [I16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `32767` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_i16_try(42) == Ok(42)
			##
			## expect U128.to_i16_try(40000) == Err(OutOfRange)
			## ```
			to_i16_try : U128 -> Try(I16, [OutOfRange, ..])

			## Convert a [U128] to an [I32], wrapping on overflow. Values from `0`
			## to `2147483647` are preserved; larger values wrap by truncating to
			## the low 32 bits and reinterpreting them in two's complement.
			## ```roc
			## expect U128.to_i32_wrap(42) == 42
			##
			## expect U128.to_i32_wrap(3000000000) == -1294967296
			## ```
			to_i32_wrap : U128 -> I32

			## Convert a [U128] to an [I32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `2147483647` succeed; larger values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_i32_try(42) == Ok(42)
			##
			## expect U128.to_i32_try(3000000000) == Err(OutOfRange)
			## ```
			to_i32_try : U128 -> Try(I32, [OutOfRange, ..])

			## Convert a [U128] to an [I64], wrapping on overflow. Values from `0`
			## to `9223372036854775807` are preserved; larger values wrap by
			## truncating to the low 64 bits and reinterpreting them in two's
			## complement.
			## ```roc
			## expect U128.to_i64_wrap(42) == 42
			##
			## expect U128.to_i64_wrap(10000000000000000000) == -8446744073709551616
			## ```
			to_i64_wrap : U128 -> I64

			## Convert a [U128] to an [I64], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `9223372036854775807` succeed; larger
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_i64_try(42) == Ok(42)
			##
			## expect U128.to_i64_try(10000000000000000000) == Err(OutOfRange)
			## ```
			to_i64_try : U128 -> Try(I64, [OutOfRange, ..])

			## Convert a [U128] to an [I128], wrapping on overflow. Values from `0`
			## to `170141183460469231731687303715884105727` are preserved; larger
			## values wrap into the negative range (two's complement reinterpretation
			## of the bits).
			## ```roc
			## expect U128.to_i128_wrap(42) == 42
			##
			## expect U128.to_i128_wrap(200000000000000000000000000000000000000) == -140282366920938463463374607431768211456
			## ```
			to_i128_wrap : U128 -> I128

			## Convert a [U128] to an [I128], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `170141183460469231731687303715884105727`
			## succeed; larger values return `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_i128_try(42) == Ok(42)
			##
			## expect U128.to_i128_try(200000000000000000000000000000000000000) == Err(OutOfRange)
			## ```
			to_i128_try : U128 -> Try(I128, [OutOfRange, ..])

			# Conversions to unsigned integers

			## Convert a [U128] to a [U8], wrapping on overflow. Values from `0` to
			## `255` are preserved; larger values wrap by truncating to the low 8
			## bits.
			## ```roc
			## expect U128.to_u8_wrap(42) == 42
			##
			## expect U128.to_u8_wrap(300) == 44
			## ```
			to_u8_wrap : U128 -> U8

			## Convert a [U128] to a [U8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `255` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_u8_try(42) == Ok(42)
			##
			## expect U128.to_u8_try(300) == Err(OutOfRange)
			## ```
			to_u8_try : U128 -> Try(U8, [OutOfRange, ..])

			## Convert a [U128] to a [U16], wrapping on overflow. Values from `0` to
			## `65535` are preserved; larger values wrap by truncating to the low 16
			## bits.
			## ```roc
			## expect U128.to_u16_wrap(42) == 42
			##
			## expect U128.to_u16_wrap(70000) == 4464
			## ```
			to_u16_wrap : U128 -> U16

			## Convert a [U128] to a [U16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `65535` succeed; larger values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_u16_try(42) == Ok(42)
			##
			## expect U128.to_u16_try(70000) == Err(OutOfRange)
			## ```
			to_u16_try : U128 -> Try(U16, [OutOfRange, ..])

			## Convert a [U128] to a [U32], wrapping on overflow. Values from `0` to
			## `4294967295` are preserved; larger values wrap by truncating to the
			## low 32 bits.
			## ```roc
			## expect U128.to_u32_wrap(42) == 42
			##
			## expect U128.to_u32_wrap(5000000000) == 705032704
			## ```
			to_u32_wrap : U128 -> U32

			## Convert a [U128] to a [U32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `4294967295` succeed; larger values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_u32_try(42) == Ok(42)
			##
			## expect U128.to_u32_try(5000000000) == Err(OutOfRange)
			## ```
			to_u32_try : U128 -> Try(U32, [OutOfRange, ..])

			## Convert a [U128] to a [U64], wrapping on overflow. Values from `0` to
			## `18446744073709551615` are preserved; larger values wrap by truncating
			## to the low 64 bits.
			## ```roc
			## expect U128.to_u64_wrap(42) == 42
			## ```
			to_u64_wrap : U128 -> U64

			## Convert a [U128] to a [U64], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `18446744073709551615` succeed;
			## larger values return `Err(OutOfRange)`.
			## ```roc
			## expect U128.to_u64_try(42) == Ok(42)
			## ```
			to_u64_try : U128 -> Try(U64, [OutOfRange, ..])

			# Conversions to floating point (all safe)
			to_f32 : U128 -> F32
			to_f64 : U128 -> F64

			# Conversion to Dec (can overflow)
			to_dec_try : U128 -> Try(Dec, [OutOfRange, ..])

			# Encode a U128 using a format that provides encode_u128
			encode : U128, fmt -> Try(encoded, err)
				where [fmt.encode_u128 : fmt, U128 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_u128(self)
			}

			decode : src, fmt -> (Try(U128, err), src)
				where [fmt.decode_u128 : fmt, src -> (Try(U128, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_u128(format, source)
			}
		}

		I128 :: [].{

			## Returns the default [I128] value, which is `0`. Functions like [List.sum]
			## use this as the starting value when adding up a list of numbers.
			## ```roc
			## expect I128.default() == 0
			## ```
			default : () -> I128
			default = || 0

			## The highest value representable by an [I128], which is
			## `170141183460469231731687303715884105727`.
			## ```roc
			## expect I128.highest == 170141183460469231731687303715884105727
			## ```
			highest : I128
			highest = 170141183460469231731687303715884105727

			## The lowest value representable by an [I128], which is
			## `-170141183460469231731687303715884105728`.
			## ```roc
			## expect I128.lowest == -170141183460469231731687303715884105728
			## ```
			lowest : I128
			lowest = -170141183460469231731687303715884105728

			## Convert an [I128] to its decimal string representation.
			## ```roc
			## expect I128.to_str(42) == "42"
			##
			## expect I128.to_str(-42) == "-42"
			## ```
			to_str : I128 -> Str

			## Returns `Bool.True` if the value is `0`.
			## ```roc
			## expect I128.is_zero(0)
			##
			## expect !I128.is_zero(7)
			## ```
			is_zero : I128 -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the value is less than `0`.
			## ```roc
			## expect I128.is_negative(-3)
			##
			## expect !I128.is_negative(0)
			## ```
			is_negative : I128 -> Bool
			is_negative = |self| self < 0

			## Returns `Bool.True` if the value is greater than `0`.
			## ```roc
			## expect I128.is_positive(3)
			##
			## expect !I128.is_positive(0)
			## ```
			is_positive : I128 -> Bool
			is_positive = |self| self > 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect I128.is_eq(3, 3)
			##
			## expect !I128.is_eq(3, 4)
			## ```
			is_eq : I128, I128 -> Bool

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect I128.is_gt(5, 3)
			##
			## expect !I128.is_gt(3, 3)
			## ```
			is_gt : I128, I128 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect I128.is_gte(3, 3)
			##
			## expect !I128.is_gte(2, 3)
			## ```
			is_gte : I128, I128 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect I128.is_lt(3, 5)
			##
			## expect !I128.is_lt(3, 3)
			## ```
			is_lt : I128, I128 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect I128.is_lte(3, 3)
			##
			## expect !I128.is_lte(5, 3)
			## ```
			is_lte : I128, I128 -> Bool

			## Returns the greater of two [I128] values.
			## ```roc
			## expect I128.max(5, 3) == 5
			##
			## expect I128.max(-3, -1) == -1
			## ```
			max : I128, I128 -> I128
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [I128] values.
			## ```roc
			## expect I128.min(5, 3) == 3
			##
			## expect I128.min(-3, -1) == -3
			## ```
			min : I128, I128 -> I128
			min = |a, b|
				if a < b
					a
				else
					b

			## Negate an [I128]. Crashes on `-170141183460469231731687303715884105728`,
			## since `170141183460469231731687303715884105728` does not fit in an [I128].
			## ```roc
			## expect I128.negate(3) == -3
			##
			## expect I128.negate(-3) == 3
			## ```
			negate : I128 -> I128

			## Return the absolute value of an [I128]. Crashes on
			## `-170141183460469231731687303715884105728`, since
			## `170141183460469231731687303715884105728` does not fit in an [I128].
			## ```roc
			## expect I128.abs(3) == 3
			##
			## expect I128.abs(-3) == 3
			## ```
			abs : I128 -> I128

			## Add two [I128] values.
			## ```roc
			## expect I128.plus(2, 3) == 5
			## ```
			plus : I128, I128 -> I128

			## Subtract the second [I128] from the first.
			## ```roc
			## expect I128.minus(5, 3) == 2
			## ```
			minus : I128, I128 -> I128

			## Multiply two [I128] values.
			## ```roc
			## expect I128.times(4, 3) == 12
			## ```
			times : I128, I128 -> I128

			## Divide the first [I128] by the second, discarding any remainder. Crashes if the second [I128] is zero.
			## ```roc
			## expect I128.div_by(10, 2) == 5
			##
			## expect I128.div_by(11, 2) == 5
			## ```
			div_by : I128, I128 -> I128

			## Divide the first [I128] by the second, truncating toward zero.
			## ```roc
			## expect I128.div_trunc_by(7, 2) == 3
			##
			## expect I128.div_trunc_by(-7, 2) == -3
			## ```
			div_trunc_by : I128, I128 -> I128

			## Return the remainder of dividing the first [I128] by the second. The
			## sign of the result matches the sign of the dividend.
			## ```roc
			## expect I128.rem_by(7, 3) == 1
			##
			## expect I128.rem_by(-7, 3) == -1
			## ```
			rem_by : I128, I128 -> I128

			## Return the modulus of the first [I128] by the second. The modulus is
			## the remainder left after dividing one number by another, and is
			## always in the range `0` up to (but not including) the absolute value
			## of the divisor. Unlike [rem_by], the sign of the result matches the
			## sign of the divisor.
			## ```roc
			## expect I128.mod_by(7, 3) == 1
			##
			## expect I128.mod_by(-7, 3) == 2
			## ```
			mod_by : I128, I128 -> I128

			## Return the absolute difference between two [I128] values as a [U128].
			## The result is a [U128] because the difference between two [I128] values
			## can be as large as `340282366920938463463374607431768211455`, which does
			## not fit in an [I128].
			## ```roc
			## expect I128.abs_diff(2, 5) == 3
			##
			## expect I128.abs_diff(-1, 5) == 6
			## ```
			abs_diff : I128, I128 -> U128

			## Shift the bits of an [I128] to the left by the given number of
			## positions. Bits shifted past the most significant bit are discarded,
			## and zeros are shifted in on the right.
			## ```roc
			## expect I128.shift_left_by(1, 3) == 8
			##
			## expect I128.shift_left_by(0b0000_0101, 2) == 0b0001_0100
			## ```
			shift_left_by : I128, U8 -> I128

			## Shift the bits of an [I128] to the right by the given number of
			## positions, preserving the sign ("arithmetic shift"). The sign bit
			## is shifted in on the left, so negative values remain negative. Each
			## right shift by one is equivalent to integer division by 2 (rounding
			## toward negative infinity).
			## ```roc
			## expect I128.shift_right_by(32, 2) == 8
			##
			## expect I128.shift_right_by(-32, 2) == -8
			## ```
			shift_right_by : I128, U8 -> I128

			## Shift the bits of an [I128] to the right by the given number of
			## positions.
			## ```roc
			## expect I128.shift_right_zf_by(32, 2) == 8
			##
			## expect I128.shift_right_zf_by(0b0101_0000, 3) == 0b0000_1010
			## ```
			shift_right_zf_by : I128, U8 -> I128

			## List of integers beginning with this `I128` and ending with the other `I128`.
			## (Use [until] instead to end with the other `I128` minus one.)
			## Returns an empty list if this `I128` is greater than the other.
			## ```roc
			## expect I128.to(1, 4) == [1, 2, 3, 4]
			##
			## expect I128.to(-2, 1) == [-2, -1, 0, 1]
			##
			## expect I128.to(5, 2) == []
			## ```
			to : I128, I128 -> List(I128)
			to = |start, end| range_to(start, end)

			## List of integers beginning with this `I128` and ending with the other `I128` minus one.
			## (Use [to] instead to end with the other `I128` exactly, instead of minus one.)
			## Returns an empty list if this `I128` is greater than or equal to the other.
			## ```roc
			## expect I128.until(1, 4) == [1, 2, 3]
			##
			## expect I128.until(-2, 1) == [-2, -1, 0]
			##
			## expect I128.until(5, 2) == []
			## ```
			until : I128, I128 -> List(I128)
			until = |start, end| range_until(start, end)

			## Build an [I128] from a list of base-10 digits, most significant first.
			## Each element of the list must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in an
			## [I128] (`-170141183460469231731687303715884105728` to
			## `170141183460469231731687303715884105727`), or if any element is not
			## a valid digit. The result is always non-negative; to build a negative
			## value, [negate] the result.
			## ```roc
			## expect I128.from_int_digits([1, 2, 3]) == Ok(123)
			## ```
			from_int_digits : List(U8) -> Try(I128, [OutOfRange, ..])

			from_numeral : Numeral -> Try(I128, [InvalidNumeral(Str), ..])

			## Parse an [I128] from a [Str]. Returns `Err(BadNumStr)` if the string
			## is not a valid integer, or if the parsed value does not fit in an
			## [I128] (`-170141183460469231731687303715884105728` to
			## `170141183460469231731687303715884105727`).
			## ```roc
			## expect I128.from_str("42") == Ok(42)
			##
			## expect I128.from_str("-1") == Ok(-1)
			## ```
			from_str : Str -> Try(I128, [BadNumStr, ..])

			# Conversions to signed integers

			## Convert an [I128] to an [I8], wrapping on overflow. Values from
			## `-128` to `127` are preserved; other values wrap by truncating to the
			## low 8 bits and reinterpreting them in two's complement.
			## ```roc
			## expect I128.to_i8_wrap(42) == 42
			##
			## expect I128.to_i8_wrap(200) == -56
			## ```
			to_i8_wrap : I128 -> I8

			## Convert an [I128] to an [I8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-128` to `127` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_i8_try(42) == Ok(42)
			##
			## expect I128.to_i8_try(200) == Err(OutOfRange)
			## ```
			to_i8_try : I128 -> Try(I8, [OutOfRange, ..])

			## Convert an [I128] to an [I16], wrapping on overflow. Values from
			## `-32768` to `32767` are preserved; other values wrap by truncating
			## to the low 16 bits and reinterpreting them in two's complement.
			## ```roc
			## expect I128.to_i16_wrap(42) == 42
			##
			## expect I128.to_i16_wrap(40000) == -25536
			## ```
			to_i16_wrap : I128 -> I16

			## Convert an [I128] to an [I16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-32768` to `32767` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_i16_try(42) == Ok(42)
			##
			## expect I128.to_i16_try(40000) == Err(OutOfRange)
			## ```
			to_i16_try : I128 -> Try(I16, [OutOfRange, ..])

			## Convert an [I128] to an [I32], wrapping on overflow. Values from
			## `-2147483648` to `2147483647` are preserved; other values wrap by
			## truncating to the low 32 bits and reinterpreting them in two's
			## complement.
			## ```roc
			## expect I128.to_i32_wrap(42) == 42
			##
			## expect I128.to_i32_wrap(3000000000) == -1294967296
			## ```
			to_i32_wrap : I128 -> I32

			## Convert an [I128] to an [I32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-2147483648` to `2147483647` succeed; other
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_i32_try(42) == Ok(42)
			##
			## expect I128.to_i32_try(3000000000) == Err(OutOfRange)
			## ```
			to_i32_try : I128 -> Try(I32, [OutOfRange, ..])

			## Convert an [I128] to an [I64], wrapping on overflow. Values from
			## `-9223372036854775808` to `9223372036854775807` are preserved; other
			## values wrap by truncating to the low 64 bits and reinterpreting them
			## in two's complement.
			## ```roc
			## expect I128.to_i64_wrap(42) == 42
			## ```
			to_i64_wrap : I128 -> I64

			## Convert an [I128] to an [I64], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `-9223372036854775808` to `9223372036854775807`
			## succeed; other values return `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_i64_try(42) == Ok(42)
			## ```
			to_i64_try : I128 -> Try(I64, [OutOfRange, ..])

			# Conversions to unsigned integers (all lossy for negative values)

			## Convert an [I128] to a [U8], wrapping on overflow. Values from `0` to
			## `255` are preserved; other values wrap by truncating to the low 8
			## bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I128.to_u8_wrap(42) == 42
			##
			## expect I128.to_u8_wrap(-1) == 255
			## ```
			to_u8_wrap : I128 -> U8

			## Convert an [I128] to a [U8], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `255` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_u8_try(42) == Ok(42)
			##
			## expect I128.to_u8_try(-1) == Err(OutOfRange)
			## ```
			to_u8_try : I128 -> Try(U8, [OutOfRange, ..])

			## Convert an [I128] to a [U16], wrapping on overflow. Values from `0`
			## to `65535` are preserved; other values wrap by truncating to the low
			## 16 bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I128.to_u16_wrap(42) == 42
			##
			## expect I128.to_u16_wrap(-1) == 65535
			## ```
			to_u16_wrap : I128 -> U16

			## Convert an [I128] to a [U16], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `65535` succeed; other values return
			## `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_u16_try(42) == Ok(42)
			##
			## expect I128.to_u16_try(-1) == Err(OutOfRange)
			## ```
			to_u16_try : I128 -> Try(U16, [OutOfRange, ..])

			## Convert an [I128] to a [U32], wrapping on overflow. Values from `0`
			## to `4294967295` are preserved; other values wrap by truncating to
			## the low 32 bits (two's complement reinterpretation of those bits).
			## ```roc
			## expect I128.to_u32_wrap(42) == 42
			##
			## expect I128.to_u32_wrap(-1) == 4294967295
			## ```
			to_u32_wrap : I128 -> U32

			## Convert an [I128] to a [U32], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `4294967295` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_u32_try(42) == Ok(42)
			##
			## expect I128.to_u32_try(-1) == Err(OutOfRange)
			## ```
			to_u32_try : I128 -> Try(U32, [OutOfRange, ..])

			## Convert an [I128] to a [U64], wrapping on overflow. Values from `0`
			## to `18446744073709551615` are preserved; other values wrap by
			## truncating to the low 64 bits (two's complement reinterpretation of
			## those bits).
			## ```roc
			## expect I128.to_u64_wrap(42) == 42
			##
			## expect I128.to_u64_wrap(-1) == 18446744073709551615
			## ```
			to_u64_wrap : I128 -> U64

			## Convert an [I128] to a [U64], returning `Err(OutOfRange)` if the value
			## does not fit. Values from `0` to `18446744073709551615` succeed; other
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_u64_try(42) == Ok(42)
			##
			## expect I128.to_u64_try(-1) == Err(OutOfRange)
			## ```
			to_u64_try : I128 -> Try(U64, [OutOfRange, ..])

			## Convert an [I128] to a [U128], wrapping on overflow. Non-negative
			## values are preserved; negative values wrap into the upper end of
			## the [U128] range (two's complement reinterpretation of the bits).
			## ```roc
			## expect I128.to_u128_wrap(42) == 42
			##
			## expect I128.to_u128_wrap(-1) == 340282366920938463463374607431768211455
			## ```
			to_u128_wrap : I128 -> U128

			## Convert an [I128] to a [U128], returning `Err(OutOfRange)` if the
			## value is negative. Non-negative values succeed; negative values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect I128.to_u128_try(42) == Ok(42)
			##
			## expect I128.to_u128_try(-1) == Err(OutOfRange)
			## ```
			to_u128_try : I128 -> Try(U128, [OutOfRange, ..])

			# Conversions to floating point (all safe)
			to_f32 : I128 -> F32
			to_f64 : I128 -> F64

			## Convert an [I128] to a [Dec], returning `Err(OutOfRange)` if the
			## value does not fit in a [Dec].
			to_dec_try : I128 -> Try(Dec, [OutOfRange, ..])

			## Encode an I128 using a format that provides encode_i128
			encode : I128, fmt -> Try(encoded, err)
				where [fmt.encode_i128 : fmt, I128 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_i128(self)
			}

			## Decode an I128 using a format that provides decode_i128
			decode : src, fmt -> (Try(I128, err), src)
				where [fmt.decode_i128 : fmt, src -> (Try(I128, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_i128(format, source)
			}
		}

		Dec :: [].{

			## Returns the default [Dec] value, which is `0.0`. Functions like
			## [List.sum] use this as the starting value when adding up a list of
			## numbers.
			## ```roc
			## expect Dec.default() == 0.0
			## ```
			default : () -> Dec
			default = || 0.0

			## The highest value representable by a [Dec], which is
			## `170141183460469231731.687303715884105727`.
			## ```roc
			## expect Dec.highest == 170141183460469231731.687303715884105727
			## ```
			highest : Dec
			highest = 170141183460469231731.687303715884105727

			## The lowest value representable by a [Dec], which is
			## `-170141183460469231731.687303715884105728`.
			## ```roc
			## expect Dec.lowest == -170141183460469231731.687303715884105728
			## ```
			lowest : Dec
			lowest = -170141183460469231731.687303715884105728

			## Convert a [Dec] to its decimal string representation.
			## ```roc
			## expect Dec.to_str(42.5) == "42.5"
			##
			## expect Dec.to_str(-42.5) == "-42.5"
			## ```
			to_str : Dec -> Str

			## Returns `Bool.True` if the value is `0.0`.
			## ```roc
			## expect Dec.is_zero(0.0)
			##
			## expect !Dec.is_zero(0.1)
			## ```
			is_zero : Dec -> Bool
			is_zero = |self| self == 0

			## Returns `Bool.True` if the value is less than `0.0`.
			## ```roc
			## expect Dec.is_negative(-0.1)
			##
			## expect !Dec.is_negative(0.0)
			## ```
			is_negative : Dec -> Bool
			is_negative = |self| self < 0

			## Returns `Bool.True` if the value is greater than `0.0`.
			## ```roc
			## expect Dec.is_positive(0.1)
			##
			## expect !Dec.is_positive(0.0)
			## ```
			is_positive : Dec -> Bool
			is_positive = |self| self > 0

			## Returns `Bool.True` if the two values are equal.
			## ```roc
			## expect Dec.is_eq(3.5, 3.5)
			##
			## expect !Dec.is_eq(3.5, 4.0)
			## ```
			is_eq : Dec, Dec -> Bool

			## Returns `Bool.True` if the first value is greater than the second.
			## ```roc
			## expect Dec.is_gt(5.0, 3.0)
			##
			## expect !Dec.is_gt(3.0, 3.0)
			## ```
			is_gt : Dec, Dec -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to the second.
			## ```roc
			## expect Dec.is_gte(3.0, 3.0)
			##
			## expect !Dec.is_gte(2.0, 3.0)
			## ```
			is_gte : Dec, Dec -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## ```roc
			## expect Dec.is_lt(3.0, 5.0)
			##
			## expect !Dec.is_lt(3.0, 3.0)
			## ```
			is_lt : Dec, Dec -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the second.
			## ```roc
			## expect Dec.is_lte(3.0, 3.0)
			##
			## expect !Dec.is_lte(5.0, 3.0)
			## ```
			is_lte : Dec, Dec -> Bool

			## Returns the greater of two [Dec] values.
			## ```roc
			## expect Dec.max(5, 3) == 5
			##
			## expect Dec.max(-3, -1) == -1
			## ```
			max : Dec, Dec -> Dec
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [Dec] values.
			## ```roc
			## expect Dec.min(5, 3) == 3
			##
			## expect Dec.min(-3, -1) == -3
			## ```
			min : Dec, Dec -> Dec
			min = |a, b|
				if a < b
					a
				else
					b

			## Negate a [Dec].
			## ```roc
			## expect Dec.negate(3.5) == -3.5
			##
			## expect Dec.negate(-3.5) == 3.5
			## ```
			negate : Dec -> Dec

			## Return the absolute value of a [Dec].
			## ```roc
			## expect Dec.abs(3.5) == 3.5
			##
			## expect Dec.abs(-3.5) == 3.5
			## ```
			abs : Dec -> Dec

			## Add two [Dec] values.
			## ```roc
			## expect Dec.plus(1.5, 2.5) == 4.0
			## ```
			plus : Dec, Dec -> Dec

			## Subtract the second [Dec] from the first.
			## ```roc
			## expect Dec.minus(5.0, 3.5) == 1.5
			## ```
			minus : Dec, Dec -> Dec

			## Multiply two [Dec] values.
			## ```roc
			## expect Dec.times(2.5, 4.0) == 10.0
			## ```
			times : Dec, Dec -> Dec

			## Divide the first [Dec] by the second. Crashes if the second [Dec]
			## is zero.
			## ```roc
			## expect Dec.div_by(10.0, 4.0) == 2.5
			## ```
			div_by : Dec, Dec -> Dec

			## Divide the first [Dec] by the second, truncating toward zero to
			## produce a whole-number [Dec] result.
			## ```roc
			## expect Dec.div_trunc_by(7.5, 2.0) == 3.0
			##
			## expect Dec.div_trunc_by(-7.5, 2.0) == -3.0
			## ```
			div_trunc_by : Dec, Dec -> Dec

			## Return the remainder of dividing the first [Dec] by the second.
			## The sign of the result matches the sign of the dividend.
			## ```roc
			## expect Dec.rem_by(7.5, 2.0) == 1.5
			##
			## expect Dec.rem_by(-7.5, 2.0) == -1.5
			## ```
			rem_by : Dec, Dec -> Dec

			## Return the absolute difference between two [Dec] values.
			## ```roc
			## expect Dec.abs_diff(2.5, 5.0) == 2.5
			##
			## expect Dec.abs_diff(-1.5, 5.0) == 6.5
			## ```
			abs_diff : Dec, Dec -> Dec

			## Build a [Dec] from a list of base-10 digits, most significant
			## first. Each element of the list must be a digit in the range `0`
			## to `9`. Returns `Err(OutOfRange)` if the resulting value does not
			## fit in a [Dec], or if any element is not a valid digit. The result is always
			## non-negative; to build a negative value, [negate] the result.
			## ```roc
			## expect Dec.from_int_digits([1, 2, 3]) == Ok(123.0)
			## ```
			from_int_digits : List(U8) -> Try(Dec, [OutOfRange, ..])

			## Build a [Dec] from a tuple of (integer digits, fractional digits),
			## each as a list of base-10 digits most significant first. Each
			## element of both lists must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in a
			## [Dec], or if any element is not a valid digit. The result is
			## always non-negative; to build a negative value, [negate] the
			## result.
			## ```roc
			## expect Dec.from_dec_digits(([1, 2], [5])) == Ok(12.5)
			## ```
			from_dec_digits : (List(U8), List(U8)) -> Try(Dec, [OutOfRange, ..])

			from_numeral : Numeral -> Try(Dec, [InvalidNumeral(Str), ..])

			## Parse a [Dec] from a [Str]. Returns `Err(BadNumStr)` if the
			## string is not a valid decimal number, or if the parsed value does
			## not fit in a [Dec].
			## ```roc
			## expect Dec.from_str("42.5") == Ok(42.5)
			##
			## expect Dec.from_str("-1.25") == Ok(-1.25)
			##
			## expect Dec.from_str("not a number") == Err(BadNumStr)
			## ```
			from_str : Str -> Try(Dec, [BadNumStr, ..])

			# Conversions to signed integers (all lossy - truncates fractional part)

			## Convert a [Dec] to an [I8]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `-128` to `127` are preserved; other values wrap by
			## truncating to the low 8 bits and reinterpreting them in two's
			## complement.
			## ```roc
			## expect Dec.to_i8_wrap(42.7) == 42
			##
			## expect Dec.to_i8_wrap(200.0) == -56
			## ```
			to_i8_wrap : Dec -> I8

			## Convert a [Dec] to an [I8], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero. Integer-part values from `-128` to `127` succeed; other
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect Dec.to_i8_try(42.7) == Ok(42)
			##
			## expect Dec.to_i8_try(200.0) == Err(OutOfRange)
			## ```
			to_i8_try : Dec -> Try(I8, [OutOfRange, ..])

			## Convert a [Dec] to an [I16]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `-32768` to `32767` are preserved; other values wrap
			## by truncating to the low 16 bits and reinterpreting them in two's
			## complement.
			## ```roc
			## expect Dec.to_i16_wrap(42.5) == 42
			##
			## expect Dec.to_i16_wrap(40000.0) == -25536
			## ```
			to_i16_wrap : Dec -> I16

			## Convert a [Dec] to an [I16], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero. Integer-part values from `-32768` to `32767` succeed; other
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect Dec.to_i16_try(42.5) == Ok(42)
			##
			## expect Dec.to_i16_try(40000.0) == Err(OutOfRange)
			## ```
			to_i16_try : Dec -> Try(I16, [OutOfRange, ..])

			## Convert a [Dec] to an [I32]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `-2147483648` to `2147483647` are preserved; other
			## values wrap by truncating to the low 32 bits and reinterpreting
			## them in two's complement.
			## ```roc
			## expect Dec.to_i32_wrap(42.5) == 42
			##
			## expect Dec.to_i32_wrap(3000000000.0) == -1294967296
			## ```
			to_i32_wrap : Dec -> I32

			## Convert a [Dec] to an [I32], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero. Integer-part values from `-2147483648` to `2147483647`
			## succeed; other values return `Err(OutOfRange)`.
			## ```roc
			## expect Dec.to_i32_try(42.5) == Ok(42)
			##
			## expect Dec.to_i32_try(3000000000.0) == Err(OutOfRange)
			## ```
			to_i32_try : Dec -> Try(I32, [OutOfRange, ..])

			## Convert a [Dec] to an [I64]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `-9223372036854775808` to `9223372036854775807` are
			## preserved; other values wrap by truncating to the low 64 bits and
			## reinterpreting them in two's complement.
			## ```roc
			## expect Dec.to_i64_wrap(42.5) == 42
			## ```
			to_i64_wrap : Dec -> I64

			## Convert a [Dec] to an [I64], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero.
			## ```roc
			## expect Dec.to_i64_try(42.5) == Ok(42)
			## ```
			to_i64_try : Dec -> Try(I64, [OutOfRange, ..])

			## Convert a [Dec] to an [I128]. The fractional part is truncated
			## toward zero. The entire integer part of any [Dec] fits in an
			## [I128], so no wrapping occurs in practice.
			## ```roc
			## expect Dec.to_i128_wrap(42.5) == 42
			## ```
			to_i128_wrap : Dec -> I128

			## Convert a [Dec] to an [I128], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero.
			## ```roc
			## expect Dec.to_i128_try(42.5) == Ok(42)
			## ```
			to_i128_try : Dec -> Try(I128, [OutOfRange, ..])

			# Conversions to unsigned integers (all lossy - truncates fractional part)

			## Convert a [Dec] to a [U8]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `0` to `255` are preserved; other values wrap by
			## truncating to the low 8 bits (two's complement reinterpretation
			## of those bits).
			## ```roc
			## expect Dec.to_u8_wrap(42.7) == 42
			##
			## expect Dec.to_u8_wrap(-1.0) == 255
			## ```
			to_u8_wrap : Dec -> U8

			## Convert a [Dec] to a [U8], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero. Integer-part values from `0` to `255` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect Dec.to_u8_try(42.7) == Ok(42)
			##
			## expect Dec.to_u8_try(-1.0) == Err(OutOfRange)
			## ```
			to_u8_try : Dec -> Try(U8, [OutOfRange, ..])

			## Convert a [Dec] to a [U16]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `0` to `65535` are preserved; other values wrap by
			## truncating to the low 16 bits (two's complement reinterpretation
			## of those bits).
			## ```roc
			## expect Dec.to_u16_wrap(42.5) == 42
			##
			## expect Dec.to_u16_wrap(-1.0) == 65535
			## ```
			to_u16_wrap : Dec -> U16

			## Convert a [Dec] to a [U16], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero. Integer-part values from `0` to `65535` succeed; other
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect Dec.to_u16_try(42.5) == Ok(42)
			##
			## expect Dec.to_u16_try(-1.0) == Err(OutOfRange)
			## ```
			to_u16_try : Dec -> Try(U16, [OutOfRange, ..])

			## Convert a [Dec] to a [U32]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `0` to `4294967295` are preserved; other values wrap
			## by truncating to the low 32 bits (two's complement
			## reinterpretation of those bits).
			## ```roc
			## expect Dec.to_u32_wrap(42.5) == 42
			##
			## expect Dec.to_u32_wrap(-1.0) == 4294967295
			## ```
			to_u32_wrap : Dec -> U32

			## Convert a [Dec] to a [U32], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero. Integer-part values from `0` to `4294967295` succeed; other
			## values return `Err(OutOfRange)`.
			## ```roc
			## expect Dec.to_u32_try(42.5) == Ok(42)
			##
			## expect Dec.to_u32_try(-1.0) == Err(OutOfRange)
			## ```
			to_u32_try : Dec -> Try(U32, [OutOfRange, ..])

			## Convert a [Dec] to a [U64]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `0` to `18446744073709551615` are preserved; other
			## values wrap by truncating to the low 64 bits (two's complement
			## reinterpretation of those bits).
			## ```roc
			## expect Dec.to_u64_wrap(42.5) == 42
			##
			## expect Dec.to_u64_wrap(-1.0) == 18446744073709551615
			## ```
			to_u64_wrap : Dec -> U64

			## Convert a [Dec] to a [U64], returning `Err(OutOfRange)` if the
			## integer part does not fit. The fractional part is truncated toward
			## zero.
			## ```roc
			## expect Dec.to_u64_try(42.5) == Ok(42)
			##
			## expect Dec.to_u64_try(-1.0) == Err(OutOfRange)
			## ```
			to_u64_try : Dec -> Try(U64, [OutOfRange, ..])

			## Convert a [Dec] to a [U128]. The fractional part is truncated
			## toward zero. Non-negative integer parts are preserved; negative
			## values wrap into the upper end of the [U128] range (two's
			## complement reinterpretation of the bits).
			## ```roc
			## expect Dec.to_u128_wrap(42.5) == 42
			## ```
			to_u128_wrap : Dec -> U128

			## Convert a [Dec] to a [U128], returning `Err(OutOfRange)` if the
			## value is negative. The fractional part is truncated toward zero.
			## ```roc
			## expect Dec.to_u128_try(42.5) == Ok(42)
			##
			## expect Dec.to_u128_try(-1.0) == Err(OutOfRange)
			## ```
			to_u128_try : Dec -> Try(U128, [OutOfRange, ..])

			# Conversions to floating point (lossy - Dec has more precision)

			## Convert a [Dec] to an [F32]. This conversion is lossy because
			## [Dec] has more precision than [F32] in its fractional range.
			## Values outside the finite [F32] range wrap to `Infinity` or
			## `-Infinity`.
			to_f32_wrap : Dec -> F32

			## Convert a [Dec] to an [F32], returning `Err(OutOfRange)` if the
			## value does not fit in the finite [F32] range. This conversion is
			## lossy because [Dec] has more precision than [F32] in its
			## fractional range.
			to_f32_try : Dec -> Try(F32, [OutOfRange, ..])

			## Convert a [Dec] to an [F64]. This conversion is lossy because
			## [Dec] has more precision than [F64] in its fractional range.
			to_f64 : Dec -> F64

			## List of decimals beginning with this `Dec` and ending with the
			## other `Dec`, stepping by `1.0`. (Use [until] instead to end with
			## the other `Dec` minus one.) Returns an empty list if this `Dec`
			## is greater than the other.
			## ```roc
			## expect Dec.to(1.0, 4.0) == [1.0, 2.0, 3.0, 4.0]
			##
			## expect Dec.to(-2.0, 1.0) == [-2.0, -1.0, 0.0, 1.0]
			##
			## expect Dec.to(5.0, 2.0) == []
			## ```
			to : Dec, Dec -> List(Dec)
			to = |start, end| range_to(start, end)

			## List of decimals beginning with this `Dec` and ending with the
			## other `Dec` minus one, stepping by `1.0`. (Use [to] instead to
			## end with the other `Dec` exactly, instead of minus one.) Returns
			## an empty list if this `Dec` is greater than or equal to the
			## other.
			## ```roc
			## expect Dec.until(1.0, 4.0) == [1.0, 2.0, 3.0]
			##
			## expect Dec.until(-2.0, 1.0) == [-2.0, -1.0, 0.0]
			##
			## expect Dec.until(5.0, 2.0) == []
			## ```
			until : Dec, Dec -> List(Dec)
			until = |start, end| range_until(start, end)

			## Encode a Dec using a format that provides encode_dec
			encode : Dec, fmt -> Try(encoded, err)
				where [fmt.encode_dec : fmt, Dec -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_dec(self)
			}

			## Decode a Dec using a format that provides decode_dec
			decode : src, fmt -> (Try(Dec, err), src)
				where [fmt.decode_dec : fmt, src -> (Try(Dec, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_dec(format, source)
			}
		}

		F32 :: [].{

			## Returns the default [F32] value, which is `0.0`. Functions like
			## [List.sum] use this as the starting value when adding up a list of
			## numbers.
			## ```roc
			## expect F32.is_zero(F32.default())
			## ```
			default : () -> F32
			default = || 0.0

			## The highest finite value representable by an [F32], which is
			## `3.40282347e38`.
			## ```roc
			## expect F32.highest == 3.40282347e38
			## ```
			highest : F32
			highest = 3.40282347e38

			## The lowest finite value representable by an [F32], which is
			## `-3.40282347e38`.
			## ```roc
			## expect F32.lowest == -3.40282347e38
			## ```
			lowest : F32
			lowest = -3.40282347e38

			## Convert an [F32] to its decimal string representation.
			## ```roc
			## expect F32.to_str(42.5) == "42.5"
			##
			## expect F32.to_str(-42.5) == "-42.5"
			## ```
			to_str : F32 -> Str

			## Returns `Bool.True` if the value is `0.0`. Both positive and
			## negative zero return `Bool.True`.
			## ```roc
			## expect F32.is_zero(0.0)
			##
			## expect !F32.is_zero(0.5)
			## ```
			is_zero : F32 -> Bool
			is_zero = |self| if self >= 0 {
				self <= 0
			} else {
				False
			}

			## Returns `Bool.True` if the value is less than `0.0`.
			## ```roc
			## expect F32.is_negative(-0.5)
			##
			## expect !F32.is_negative(0.0)
			## ```
			is_negative : F32 -> Bool
			is_negative = |self| self < 0

			## Returns `Bool.True` if the value is greater than `0.0`.
			## ```roc
			## expect F32.is_positive(0.5)
			##
			## expect !F32.is_positive(0.0)
			## ```
			is_positive : F32 -> Bool
			is_positive = |self| self > 0

			## Returns `Bool.True` if the first value is greater than the second.
			## Comparisons involving `NaN` always return `Bool.False`.
			## ```roc
			## expect F32.is_gt(5.0, 3.0)
			##
			## expect !F32.is_gt(3.0, 3.0)
			## ```
			is_gt : F32, F32 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to
			## the second. Comparisons involving `NaN` always return `Bool.False`.
			## ```roc
			## expect F32.is_gte(3.0, 3.0)
			##
			## expect !F32.is_gte(2.0, 3.0)
			## ```
			is_gte : F32, F32 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## Comparisons involving `NaN` always return `Bool.False`.
			## ```roc
			## expect F32.is_lt(3.0, 5.0)
			##
			## expect !F32.is_lt(3.0, 3.0)
			## ```
			is_lt : F32, F32 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the
			## second. Comparisons involving `NaN` always return `Bool.False`.
			## ```roc
			## expect F32.is_lte(3.0, 3.0)
			##
			## expect !F32.is_lte(5.0, 3.0)
			## ```
			is_lte : F32, F32 -> Bool

			## Returns the greater of two [F32] values.
			## ```roc
			## expect F32.max(5, 3) == 5
			##
			## expect F32.max(-3, -1) == -1
			## ```
			max : F32, F32 -> F32
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [F32] values.
			## ```roc
			## expect F32.min(5, 3) == 3
			##
			## expect F32.min(-3, -1) == -3
			## ```
			min : F32, F32 -> F32
			min = |a, b|
				if a < b
					a
				else
					b

			## Negate an [F32]. Flips the sign bit, so negating `0.0` produces
			## `-0.0` and negating `NaN` produces `NaN`.
			## ```roc
			## expect F32.negate(3.5).to_str() == "-3.5"
			##
			## expect F32.negate(-3.5).to_str() == "3.5"
			## ```
			negate : F32 -> F32

			## Return the absolute value of an [F32]. The result of `abs(NaN)` is
			## `NaN`.
			## ```roc
			## expect F32.abs(3.5).to_str() == "3.5"
			##
			## expect F32.abs(-3.5).to_str() == "3.5"
			## ```
			abs : F32 -> F32

			## Add two [F32] values. Addition is subject to IEEE 754 rounding; the
			## result may be `inf`, `-inf`, or `NaN`.
			## ```roc
			## expect F32.plus(1.5, 2.5).to_str() == "4"
			## ```
			plus : F32, F32 -> F32

			## Subtract the second [F32] from the first. Subtraction is subject to
			## IEEE 754 rounding; the result may be `inf`, `-inf`, or `NaN`.
			## ```roc
			## expect F32.minus(5.0, 3.5).to_str() == "1.5"
			## ```
			minus : F32, F32 -> F32

			## Multiply two [F32] values. Multiplication is subject to IEEE 754
			## rounding; the result may be `inf`, `-inf`, or `NaN`.
			## ```roc
			## expect F32.times(2.5, 4.0).to_str() == "10"
			## ```
			times : F32, F32 -> F32

			## Divide the first [F32] by the second. Unlike integer division,
			## division by zero does not crash: it produces `inf`, `-inf`, or
			## `NaN` as specified by IEEE 754.
			## ```roc
			## expect F32.div_by(10.0, 4.0).to_str() == "2.5"
			## ```
			div_by : F32, F32 -> F32

			## Divide the first [F32] by the second, truncating toward zero to
			## produce a whole-number [F32] result.
			## ```roc
			## expect F32.div_trunc_by(7.5, 2.0).to_str() == "3"
			##
			## expect F32.div_trunc_by(-7.5, 2.0).to_str() == "-3"
			## ```
			div_trunc_by : F32, F32 -> F32

			## Return the remainder of dividing the first [F32] by the second.
			## The sign of the result matches the sign of the dividend.
			## ```roc
			## expect F32.rem_by(7.5, 2.0).to_str() == "1.5"
			##
			## expect F32.rem_by(-7.5, 2.0).to_str() == "-1.5"
			## ```
			rem_by : F32, F32 -> F32

			## Return the absolute difference between two [F32] values.
			## ```roc
			## expect F32.abs_diff(2.5, 5.0).to_str() == "2.5"
			##
			## expect F32.abs_diff(-1.5, 5.0).to_str() == "6.5"
			## ```
			abs_diff : F32, F32 -> F32

			## Build an [F32] from a list of base-10 digits, most significant
			## first. Each element of the list must be a digit in the range `0`
			## to `9`. Returns `Err(OutOfRange)` if the resulting value does not
			## fit in an [F32], or if any element is not a valid digit. The
			## result is always non-negative; to build a negative value, [negate]
			## the result.
			## ```roc
			## expect match F32.from_int_digits([1, 2, 3]) {
			##     Ok(x) => F32.to_str(x) == "123"
			##     Err(_) => False
			## }
			## ```
			from_int_digits : List(U8) -> Try(F32, [OutOfRange, ..])

			## Build an [F32] from a tuple of (integer digits, fractional digits),
			## each as a list of base-10 digits most significant first. Each
			## element of both lists must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in an
			## [F32], or if any element is not a valid digit. The result is
			## always non-negative; to build a negative value, [negate] the
			## result.
			## ```roc
			## expect match F32.from_dec_digits(([1, 2], [5])) {
			##     Ok(x) => F32.to_str(x) == "12.5"
			##     Err(_) => False
			## }
			## ```
			from_dec_digits : (List(U8), List(U8)) -> Try(F32, [OutOfRange, ..])

			from_numeral : Numeral -> Try(F32, [InvalidNumeral(Str), ..])

			## Parse an [F32] from a [Str]. Returns `Err(BadNumStr)` if the
			## string is not a valid decimal number, or if the parsed value does
			## not fit in an [F32].
			## ```roc
			## expect match F32.from_str("42.5") {
			##     Ok(x) => F32.to_str(x) == "42.5"
			##     Err(_) => False
			## }
			##
			## expect match F32.from_str("-1.25") {
			##     Ok(x) => F32.to_str(x) == "-1.25"
			##     Err(_) => False
			## }
			##
			## expect Try.is_err(F32.from_str("not a number"))
			## ```
			from_str : Str -> Try(F32, [BadNumStr, ..])

			# Conversions to signed integers (all lossy - truncation + range check)

			## Convert an [F32] to an [I8]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `-128` to `127` are preserved; other values wrap by
			## truncating to the low 8 bits and reinterpreting them in two's
			## complement. The result for `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F32.to_i8_wrap(42.7) == 42
			## ```
			to_i8_wrap : F32 -> I8

			## Convert an [F32] to an [I8], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## Integer-part values from `-128` to `127` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect F32.to_i8_try(42.7) == Ok(42)
			##
			## expect F32.to_i8_try(200.0) == Err(OutOfRange)
			## ```
			to_i8_try : F32 -> Try(I8, [OutOfRange, ..])

			## Convert an [F32] to an [I16]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `-32768` to `32767` are preserved; other values wrap
			## by truncating to the low 16 bits and reinterpreting them in two's
			## complement. The result for `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F32.to_i16_wrap(42.5) == 42
			## ```
			to_i16_wrap : F32 -> I16

			## Convert an [F32] to an [I16], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## Integer-part values from `-32768` to `32767` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect F32.to_i16_try(42.5) == Ok(42)
			##
			## expect F32.to_i16_try(40000.0) == Err(OutOfRange)
			## ```
			to_i16_try : F32 -> Try(I16, [OutOfRange, ..])

			## Convert an [F32] to an [I32]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## `NaN`, `inf`, or `-inf` is implementation-defined.
			## ```roc
			## expect F32.to_i32_wrap(42.5) == 42
			## ```
			to_i32_wrap : F32 -> I32

			## Convert an [F32] to an [I32], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## ```roc
			## expect F32.to_i32_try(42.5) == Ok(42)
			## ```
			to_i32_try : F32 -> Try(I32, [OutOfRange, ..])

			## Convert an [F32] to an [I64]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## `NaN`, `inf`, or `-inf` is implementation-defined.
			## ```roc
			## expect F32.to_i64_wrap(42.5) == 42
			## ```
			to_i64_wrap : F32 -> I64

			## Convert an [F32] to an [I64], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## ```roc
			## expect F32.to_i64_try(42.5) == Ok(42)
			## ```
			to_i64_try : F32 -> Try(I64, [OutOfRange, ..])

			## Convert an [F32] to an [I128]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## `NaN`, `inf`, or `-inf` is implementation-defined.
			## ```roc
			## expect F32.to_i128_wrap(42.5) == 42
			## ```
			to_i128_wrap : F32 -> I128

			## Convert an [F32] to an [I128], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## ```roc
			## expect F32.to_i128_try(42.5) == Ok(42)
			## ```
			to_i128_try : F32 -> Try(I128, [OutOfRange, ..])

			# Conversions to unsigned integers (all lossy - truncation + range check)

			## Convert an [F32] to a [U8]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F32.to_u8_wrap(42.7) == 42
			## ```
			to_u8_wrap : F32 -> U8

			## Convert an [F32] to a [U8], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F32.to_u8_try(42.7) == Ok(42)
			##
			## expect F32.to_u8_try(-1.0) == Err(OutOfRange)
			## ```
			to_u8_try : F32 -> Try(U8, [OutOfRange, ..])

			## Convert an [F32] to a [U16]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F32.to_u16_wrap(42.5) == 42
			## ```
			to_u16_wrap : F32 -> U16

			## Convert an [F32] to a [U16], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F32.to_u16_try(42.5) == Ok(42)
			## ```
			to_u16_try : F32 -> Try(U16, [OutOfRange, ..])

			## Convert an [F32] to a [U32]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F32.to_u32_wrap(42.5) == 42
			## ```
			to_u32_wrap : F32 -> U32

			## Convert an [F32] to a [U32], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F32.to_u32_try(42.5) == Ok(42)
			## ```
			to_u32_try : F32 -> Try(U32, [OutOfRange, ..])

			## Convert an [F32] to a [U64]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F32.to_u64_wrap(42.5) == 42
			## ```
			to_u64_wrap : F32 -> U64

			## Convert an [F32] to a [U64], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F32.to_u64_try(42.5) == Ok(42)
			## ```
			to_u64_try : F32 -> Try(U64, [OutOfRange, ..])

			## Convert an [F32] to a [U128]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F32.to_u128_wrap(42.5) == 42
			## ```
			to_u128_wrap : F32 -> U128

			## Convert an [F32] to a [U128], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F32.to_u128_try(42.5) == Ok(42)
			## ```
			to_u128_try : F32 -> Try(U128, [OutOfRange, ..])

			## Convert an [F32] to an [F64]. This is a safe widening conversion:
			## every [F32] value is exactly representable as an [F64], including
			## `NaN`, `inf`, and `-inf`.
			## ```roc
			## expect F64.to_str(F32.to_f64(1.5)) == "1.5"
			## ```
			to_f64 : F32 -> F64

			## Encode an F32 using a format that provides encode_f32
			encode : F32, fmt -> Try(encoded, err)
				where [fmt.encode_f32 : fmt, F32 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_f32(self)
			}

			## Decode an F32 using a format that provides decode_f32
			decode : src, fmt -> (Try(F32, err), src)
				where [fmt.decode_f32 : fmt, src -> (Try(F32, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_f32(format, source)
			}
		}

		F64 :: [].{

			## Returns the default [F64] value, which is `0.0`. Functions like
			## [List.sum] use this as the starting value when adding up a list of
			## numbers.
			## ```roc
			## expect F64.is_zero(F64.default())
			## ```
			default : () -> F64
			default = || 0.0

			## The highest finite value representable by an [F64], which is
			## `1.7976931348623157e308`.
			## ```roc
			## expect F64.highest == 1.7976931348623157e308
			## ```
			highest : F64
			highest = 1.7976931348623157e308

			## The lowest finite value representable by an [F64], which is
			## `-1.7976931348623157e308`.
			## ```roc
			## expect F64.lowest == -1.7976931348623157e308
			## ```
			lowest : F64
			lowest = -1.7976931348623157e308

			## Convert an [F64] to its decimal string representation.
			## ```roc
			## expect F64.to_str(42.5) == "42.5"
			##
			## expect F64.to_str(-42.5) == "-42.5"
			## ```
			to_str : F64 -> Str

			## Returns `Bool.True` if the value is `0.0`. Both positive and
			## negative zero return `Bool.True`.
			## ```roc
			## expect F64.is_zero(0.0)
			##
			## expect !F64.is_zero(0.5)
			## ```
			is_zero : F64 -> Bool
			is_zero = |self| if self >= 0 {
				self <= 0
			} else {
				False
			}

			## Returns `Bool.True` if the value is less than `0.0`.
			## ```roc
			## expect F64.is_negative(-0.5)
			##
			## expect !F64.is_negative(0.0)
			## ```
			is_negative : F64 -> Bool
			is_negative = |self| self < 0

			## Returns `Bool.True` if the value is greater than `0.0`.
			## ```roc
			## expect F64.is_positive(0.5)
			##
			## expect !F64.is_positive(0.0)
			## ```
			is_positive : F64 -> Bool
			is_positive = |self| self > 0

			## Returns `Bool.True` if the first value is greater than the second.
			## Comparisons involving `NaN` always return `Bool.False`.
			## ```roc
			## expect F64.is_gt(5.0, 3.0)
			##
			## expect !F64.is_gt(3.0, 3.0)
			## ```
			is_gt : F64, F64 -> Bool

			## Returns `Bool.True` if the first value is greater than or equal to
			## the second. Comparisons involving `NaN` always return `Bool.False`.
			## ```roc
			## expect F64.is_gte(3.0, 3.0)
			##
			## expect !F64.is_gte(2.0, 3.0)
			## ```
			is_gte : F64, F64 -> Bool

			## Returns `Bool.True` if the first value is less than the second.
			## Comparisons involving `NaN` always return `Bool.False`.
			## ```roc
			## expect F64.is_lt(3.0, 5.0)
			##
			## expect !F64.is_lt(3.0, 3.0)
			## ```
			is_lt : F64, F64 -> Bool

			## Returns `Bool.True` if the first value is less than or equal to the
			## second. Comparisons involving `NaN` always return `Bool.False`.
			## ```roc
			## expect F64.is_lte(3.0, 3.0)
			##
			## expect !F64.is_lte(5.0, 3.0)
			## ```
			is_lte : F64, F64 -> Bool

			## Returns the greater of two [F64] values.
			## ```roc
			## expect F64.max(5, 3) == 5
			##
			## expect F64.max(-3, -1) == -1
			## ```
			max : F64, F64 -> F64
			max = |a, b|
				if a > b
					a
				else
					b

			## Returns the smaller of two [F64] values.
			## ```roc
			## expect F64.min(5, 3) == 3
			##
			## expect F64.min(-3, -1) == -3
			## ```
			min : F64, F64 -> F64
			min = |a, b|
				if a < b
					a
				else
					b

			## Negate an [F64]. Flips the sign bit, so negating `0.0` produces
			## `-0.0` and negating `NaN` produces `NaN`.
			## ```roc
			## expect F64.negate(3.5).to_str() == "-3.5"
			##
			## expect F64.negate(-3.5).to_str() == "3.5"
			## ```
			negate : F64 -> F64

			## Return the absolute value of an [F64]. The result of `abs(NaN)` is
			## `NaN`.
			## ```roc
			## expect F64.abs(3.5).to_str() == "3.5"
			##
			## expect F64.abs(-3.5).to_str() == "3.5"
			## ```
			abs : F64 -> F64

			## Add two [F64] values. Addition is subject to IEEE 754 rounding; the
			## result may be `inf`, `-inf`, or `NaN`.
			## ```roc
			## expect F64.plus(1.5, 2.5).to_str() == "4"
			## ```
			plus : F64, F64 -> F64

			## Subtract the second [F64] from the first. Subtraction is subject to
			## IEEE 754 rounding; the result may be `inf`, `-inf`, or `NaN`.
			## ```roc
			## expect F64.minus(5.0, 3.5).to_str() == "1.5"
			## ```
			minus : F64, F64 -> F64

			## Multiply two [F64] values. Multiplication is subject to IEEE 754
			## rounding; the result may be `inf`, `-inf`, or `NaN`.
			## ```roc
			## expect F64.times(2.5, 4.0).to_str() == "10"
			## ```
			times : F64, F64 -> F64

			## Divide the first [F64] by the second. Unlike integer division,
			## division by zero does not crash: it produces `inf`, `-inf`, or
			## `NaN` as specified by IEEE 754.
			## ```roc
			## expect F64.div_by(10.0, 4.0).to_str() == "2.5"
			## ```
			div_by : F64, F64 -> F64

			## Divide the first [F64] by the second, truncating toward zero to
			## produce a whole-number [F64] result.
			## ```roc
			## expect F64.div_trunc_by(7.5, 2.0).to_str() == "3"
			##
			## expect F64.div_trunc_by(-7.5, 2.0).to_str() == "-3"
			## ```
			div_trunc_by : F64, F64 -> F64

			## Return the remainder of dividing the first [F64] by the second.
			## The sign of the result matches the sign of the dividend.
			## ```roc
			## expect F64.rem_by(7.5, 2.0).to_str() == "1.5"
			##
			## expect F64.rem_by(-7.5, 2.0).to_str() == "-1.5"
			## ```
			rem_by : F64, F64 -> F64

			## Return the absolute difference between two [F64] values.
			## ```roc
			## expect F64.abs_diff(2.5, 5.0).to_str() == "2.5"
			##
			## expect F64.abs_diff(-1.5, 5.0).to_str() == "6.5"
			## ```
			abs_diff : F64, F64 -> F64

			## Build an [F64] from a list of base-10 digits, most significant
			## first. Each element of the list must be a digit in the range `0`
			## to `9`. Returns `Err(OutOfRange)` if the resulting value does not
			## fit in an [F64], or if any element is not a valid digit. The
			## result is always non-negative; to build a negative value, [negate]
			## the result.
			## ```roc
			## expect match F64.from_int_digits([1, 2, 3]) {
			##     Ok(x) => F64.to_str(x) == "123"
			##     Err(_) => False
			## }
			## ```
			from_int_digits : List(U8) -> Try(F64, [OutOfRange, ..])

			## Build an [F64] from a tuple of (integer digits, fractional digits),
			## each as a list of base-10 digits most significant first. Each
			## element of both lists must be a digit in the range `0` to `9`.
			## Returns `Err(OutOfRange)` if the resulting value does not fit in an
			## [F64], or if any element is not a valid digit. The result is
			## always non-negative; to build a negative value, [negate] the
			## result.
			## ```roc
			## expect match F64.from_dec_digits(([1, 2], [5])) {
			##     Ok(x) => F64.to_str(x) == "12.5"
			##     Err(_) => False
			## }
			## ```
			from_dec_digits : (List(U8), List(U8)) -> Try(F64, [OutOfRange, ..])

			from_numeral : Numeral -> Try(F64, [InvalidNumeral(Str), ..])

			## Parse an [F64] from a [Str]. Returns `Err(BadNumStr)` if the
			## string is not a valid decimal number, or if the parsed value does
			## not fit in an [F64].
			## ```roc
			## expect match F64.from_str("42.5") {
			##     Ok(x) => F64.to_str(x) == "42.5"
			##     Err(_) => False
			## }
			##
			## expect match F64.from_str("-1.25") {
			##     Ok(x) => F64.to_str(x) == "-1.25"
			##     Err(_) => False
			## }
			##
			## expect Try.is_err(F64.from_str("not a number"))
			## ```
			from_str : Str -> Try(F64, [BadNumStr, ..])

			# Conversions to signed integers (all lossy - truncation + range check)

			## Convert an [F64] to an [I8]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `-128` to `127` are preserved; other values wrap by
			## truncating to the low 8 bits and reinterpreting them in two's
			## complement. The result for `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F64.to_i8_wrap(42.7) == 42
			## ```
			to_i8_wrap : F64 -> I8

			## Convert an [F64] to an [I8], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## Integer-part values from `-128` to `127` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect F64.to_i8_try(42.7) == Ok(42)
			##
			## expect F64.to_i8_try(200.0) == Err(OutOfRange)
			## ```
			to_i8_try : F64 -> Try(I8, [OutOfRange, ..])

			## Convert an [F64] to an [I16]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. Integer-part
			## values from `-32768` to `32767` are preserved; other values wrap
			## by truncating to the low 16 bits and reinterpreting them in two's
			## complement. The result for `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F64.to_i16_wrap(42.5) == 42
			## ```
			to_i16_wrap : F64 -> I16

			## Convert an [F64] to an [I16], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## Integer-part values from `-32768` to `32767` succeed; other values
			## return `Err(OutOfRange)`.
			## ```roc
			## expect F64.to_i16_try(42.5) == Ok(42)
			##
			## expect F64.to_i16_try(40000.0) == Err(OutOfRange)
			## ```
			to_i16_try : F64 -> Try(I16, [OutOfRange, ..])

			## Convert an [F64] to an [I32]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## `NaN`, `inf`, or `-inf` is implementation-defined.
			## ```roc
			## expect F64.to_i32_wrap(42.5) == 42
			## ```
			to_i32_wrap : F64 -> I32

			## Convert an [F64] to an [I32], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## ```roc
			## expect F64.to_i32_try(42.5) == Ok(42)
			## ```
			to_i32_try : F64 -> Try(I32, [OutOfRange, ..])

			## Convert an [F64] to an [I64]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## `NaN`, `inf`, or `-inf` is implementation-defined.
			## ```roc
			## expect F64.to_i64_wrap(42.5) == 42
			## ```
			to_i64_wrap : F64 -> I64

			## Convert an [F64] to an [I64], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## ```roc
			## expect F64.to_i64_try(42.5) == Ok(42)
			## ```
			to_i64_try : F64 -> Try(I64, [OutOfRange, ..])

			## Convert an [F64] to an [I128]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## `NaN`, `inf`, or `-inf` is implementation-defined.
			## ```roc
			## expect F64.to_i128_wrap(42.5) == 42
			## ```
			to_i128_wrap : F64 -> I128

			## Convert an [F64] to an [I128], returning `Err(OutOfRange)` if the
			## integer part does not fit, or if the value is `NaN`, `inf`, or
			## `-inf`. The fractional part is truncated toward zero.
			## ```roc
			## expect F64.to_i128_try(42.5) == Ok(42)
			## ```
			to_i128_try : F64 -> Try(I128, [OutOfRange, ..])

			# Conversions to unsigned integers (all lossy - truncation + range check)

			## Convert an [F64] to a [U8]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F64.to_u8_wrap(42.7) == 42
			## ```
			to_u8_wrap : F64 -> U8

			## Convert an [F64] to a [U8], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F64.to_u8_try(42.7) == Ok(42)
			##
			## expect F64.to_u8_try(-1.0) == Err(OutOfRange)
			## ```
			to_u8_try : F64 -> Try(U8, [OutOfRange, ..])

			## Convert an [F64] to a [U16]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F64.to_u16_wrap(42.5) == 42
			## ```
			to_u16_wrap : F64 -> U16

			## Convert an [F64] to a [U16], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F64.to_u16_try(42.5) == Ok(42)
			## ```
			to_u16_try : F64 -> Try(U16, [OutOfRange, ..])

			## Convert an [F64] to a [U32]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F64.to_u32_wrap(42.5) == 42
			## ```
			to_u32_wrap : F64 -> U32

			## Convert an [F64] to a [U32], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F64.to_u32_try(42.5) == Ok(42)
			## ```
			to_u32_try : F64 -> Try(U32, [OutOfRange, ..])

			## Convert an [F64] to a [U64]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F64.to_u64_wrap(42.5) == 42
			## ```
			to_u64_wrap : F64 -> U64

			## Convert an [F64] to a [U64], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F64.to_u64_try(42.5) == Ok(42)
			## ```
			to_u64_try : F64 -> Try(U64, [OutOfRange, ..])

			## Convert an [F64] to a [U128]. The fractional part is truncated
			## toward zero; the integer part wraps on overflow. The result for
			## negative values, `NaN`, `inf`, or `-inf` is
			## implementation-defined.
			## ```roc
			## expect F64.to_u128_wrap(42.5) == 42
			## ```
			to_u128_wrap : F64 -> U128

			## Convert an [F64] to a [U128], returning `Err(OutOfRange)` if the
			## integer part does not fit, if the value is negative, or if the
			## value is `NaN`, `inf`, or `-inf`. The fractional part is
			## truncated toward zero.
			## ```roc
			## expect F64.to_u128_try(42.5) == Ok(42)
			## ```
			to_u128_try : F64 -> Try(U128, [OutOfRange, ..])

			## Convert an [F64] to an [F32], narrowing the value. [F64] has more
			## precision and a wider range than [F32], so this conversion may
			## lose precision, and values that exceed the [F32] range overflow
			## to `inf` or `-inf`. `NaN`, `inf`, and `-inf` are preserved.
			## ```roc
			## expect F32.to_str(F64.to_f32_wrap(1.5)) == "1.5"
			## ```
			to_f32_wrap : F64 -> F32

			## Convert an [F64] to an [F32], returning `Err(OutOfRange)` if the
			## value's magnitude exceeds the [F32] range (which would otherwise
			## overflow to `inf` or `-inf`), or if the value is `NaN`. Values
			## that fit in an [F32] succeed, though precision may still be lost
			## due to [F32]'s smaller mantissa.
			## ```roc
			## expect match F64.to_f32_try(1.5) {
			##     Ok(x) => F32.to_str(x) == "1.5"
			##     Err(_) => False
			## }
			## ```
			to_f32_try : F64 -> Try(F32, [OutOfRange, ..])
			to_f32_try = |num| {
				answer = f64_to_f32_try_unsafe(num)
				if answer.success != 0 {
					Ok(answer.val_or_memory_garbage)
				} else {
					Err(OutOfRange)
				}
			}

			## Encode an F64 using a format that provides encode_f64
			encode : F64, fmt -> Try(encoded, err)
				where [fmt.encode_f64 : fmt, F64 -> Try(encoded, err)]
			encode = |self, format| {
				format.encode_f64(self)
			}

			## Decode an F64 using a format that provides decode_f64
			decode : src, fmt -> (Try(F64, err), src)
				where [fmt.decode_f64 : fmt, src -> (Try(F64, err), src)]
			decode = |source, format| {
				Fmt : fmt
				Fmt.decode_f64(format, source)
			}
		}
	}

}

range_to = |var $current, end| {
	var $answer = [] # Not bothering with List.with_capacity because this will become an iterator once those exist.

	while $current <= end {
		$answer = $answer.append($current)
		$current = $current + 1
	}

	$answer
}

range_until = |var $current, end| {
	var $answer = [] # Not bothering with List.with_capacity because this will become an iterator once those exist.

	while $current < end {
		$answer = $answer.append($current)
		$current = $current + 1
	}

	$answer
}

# Implemented by the compiler, does not perform bounds checks
list_get_unsafe : List(item), U64 -> item

# Implemented by the compiler, does not perform bounds checks
list_append_unsafe : List(item), item -> List(item)

# Unsafe conversion functions - these return simple records instead of Try types
# They are low-level operations that get replaced by the compiler
# Note: success is U8 (0 = false, 1 = true) since Bool is not available at top level
f64_to_f32_try_unsafe : F64 -> { success : U8, val_or_memory_garbage : F32 }
