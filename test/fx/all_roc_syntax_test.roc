app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stdout as StdoutAlias
# TODO implement: import "../../README.md" as readme : Str

# Note 1: I tried to demonstrate all Roc syntax (possible in a single app file),
# but I probably forgot some things. Let @Anton know on zulip if you spot something missing, or make a github issue.

# # Double hashtag for doc comment
number_operators : I64, I64 -> _
number_operators = |a, b| {
	a_f64 = I64.to_f64(a)
	b_f64 = I64.to_f64(b)

	{
		# binary operators
		sum: a + b,
		diff: a - b,
		prod: a * b,
		div: a_f64 / b_f64,
		div_trunc: a // b,
		rem: a % b,
		eq: a == b,
		neq: a != b,
		lt: a < b,
		lteq: a <= b,
		gt: a > b,
		gteq: a >= b,

		# Not implemented yet:
		# default: None ?? 0,

		# unary operators
		neg: -a,
		# the last item can have a comma too
	}
}

boolean_operators : Bool, Bool -> _
boolean_operators = |a, b| {
	bool_and_keyword: a and b,
	bool_or_keyword: a or b,
	not_a: !a,
}

simple_match : [Red, Green, Blue] -> Str
simple_match = |color| {
	match color {
		Red => "The color is red."
		Green => "The color is green."
		Blue => "The color is blue."
	}
}

match_list_patterns : List(U64) -> U64
match_list_patterns = |lst| {
	match lst {
		[] => 0
		[x] => x
		[1, 2, 3] => 6
		[1, 2, ..] => 66
		[2, .., 1] => 88
		[1, .. as tail] => 77 + tail.len()
		[_head, 5] => 55

		# Not implemented yet:
		# [ 99, x ] if x < 4 => 99 + x

		# Note: avoid overusing `_` in a match branch, in general you should
		# try to match all cases explicitly.
		_ => 100
	}
}

match_tag_union_advanced : Try({}, [StdoutErr(Str), Other]) -> Str
match_tag_union_advanced = |try|
# `Try(a, b)` is the tag union `[Ok(a), Err(b)]` under the hood.
	match try {
		Ok(_) =>
			"Success"

		Err(StdoutErr(err)) =>
			"StdoutErr: ${Str.inspect(err)}"

		Err(_) =>
			"Unknown error"
		}

multiline_str : U64 -> Str
multiline_str = |number|
	\\Line 1
	\\Line 2
	\\Line ${number.to_str()}

# end name with `!` for effectful functions
# `=>` shows effectfulness in the type signature
effect_demo! : Str => {}
effect_demo! = |msg|
	Stdout.line!(msg)

# thread 229407 panic
# question_postfix : List(Str) -> Try(I64, _)
# question_postfix = |strings| {
#     # `?` to immediately return the error if there is one
#     first_str = strings.first()?
#     first_num = I64.from_str(first_str)?

#     Ok(first_num + 1)
# }

# three dots for things you want to fill in later, will crash if implement_me_later(arg) is called
implement_me_later = |_str| ...

# for loops can be easier to think about than List.fold (previously `List.walk`)
for_loop = |num_list| {
	var $sum = 0

	for num in num_list {
		$sum = $sum + num
	}

	$sum
}

print! = |something| {
	Stdout.line!(Str.inspect(something))
}

dbg_keyword = || {
	foo = 42

	dbg foo

	# This variation does not work yet:
	# bar = dbg 43

	foo
}

if_demo : U64 -> Str
if_demo = |num| {
	# every if must have an else branch!
	one_line_if = if num == 1 "One" else "NotOne"

	two_line_if = 
		if num == 2
			"Two"
				else
					"NotTwo"

	# thread 336429 panic
	# with_curlies = 
	#     if num == 5 {
	#         "Five"
	#     } else {
	#         "NotFive"
	#     }

	# else if
	if num == 3
		"Three"
			else if num == 4
				"Four"
					else
						one_line_if.concat(two_line_if)
}

tuple_demo = 
# tuples can contain multiple types
	("Roc", 1)

# Here we use a type variable `a` to indicate this function works for a list of any type.
type_var : List(a) -> List(a)
type_var = |lst| lst

# TODO Roc crashed: Error evaluating: TypeMismatch
# destructuring = || {
#     tup = ("Roc", 1)
#     (str, num) = tup

#     rec = { x: 1, y: str } # TODO implement tuple access with `.index` ?
#     { x, y } = rec

#     (str, num, x, y)
# }

# TODO not sure if still planned for implementation
# record_update = {
#     rec = { x: 1, y: 2 }
#     rec2 = { rec & y: 3 }
#     rec2
# }

record_update_2 : { name : Str, age : I64 } -> { name : Str, age : I64 }
record_update_2 = |person| {
	{ ..person, age: 31 }
}

number_literals = {
	usage_based: 5, # Dec by default
	explicit_u8: 5u8,
	explicit_i8: 5i8,
	explicit_u16: 5u16,
	explicit_i16: 5i16,
	explicit_u32: 5u32,
	explicit_i32: 5i32,
	explicit_u64: 5u64,
	explicit_i64: 5i64,
	explicit_u128: 5u128,
	explicit_i128: 5i128,
	explicit_f32: 5.0f32,
	explicit_f64: 5.0f64,
	explicit_dec: 5.0dec,
	hex: 0x5,
	octal: 0o5,
	binary: 0b0101,
}

# Opaque type
Username :: Str

# Define a nominal type with a custom is_eq method
Animal := [Dog(Str), Cat(Str)].{
	is_eq = |a, b| match (a, b) {
		(Dog(name1), Dog(name2)) => name1 == name2
		(Cat(name1), Cat(name2)) => name1 == name2
		_ => Bool.False
	}
}

early_return = |arg| {
	first = 
		if !arg {
			return 99
		} else {
			"continue"
		}

	# Do some other stuff
	Str.count_utf8_bytes(first)
}

main! = || {
	Stdout.line!("Hello, world!")
	StdoutAlias.line!("Hello, world! (using alias)")

	Stdout.line!(Str.inspect(number_operators(10, 5)))
	print!(boolean_operators(Bool.True, Bool.False))

	# pizza operator (|>) is gone, we now have static dispatch instead:
	print!("One".concat(" Two"))

	Stdout.line!(simple_match(Red))
	print!(match_list_patterns([1, 10]))
	Stdout.line!(match_tag_union_advanced(Ok({})))

	Stdout.line!(multiline_str(3))
	Stdout.line!("Unicode escape sequence: \u(00A0)")

	effect_demo!("This is an effectful function!")

	# Stdout.line!(Str.inspect(question_postfix(["1", "not a number", "100"])))

	# panic: index out of bounds
	# print!(for_loop([1,2,3,4,5]))

	sum = for_loop([1, 2, 3, 4, 5])
	print!(sum)

	expect sum == 15

	print!(dbg_keyword())

	Stdout.line!(if_demo(2))

	print!(tuple_demo)

	print!(type_var(["a", "b"]))

	# print!(destructuring())

	# print!(record_update)

	# TODO Roc crashed: Error evaluating: TypeMismatch
	# print!({ x: 10, y: 20 }.x)

	print!(record_update_2({ name: "Alice", age: 30 }))

	print!(number_literals)

	# TODO: not implemented yet.
	# bob = @Username("Bob")
	# Stdout.line!("Username: ${bob}")

	dog : Animal
	dog = Dog("Fido")
	cat : Animal
	cat = Cat("Whiskers")
	print!(dog == cat)

	print!(early_return(Bool.False))

	# TODO Stdout.line!(readme);

	# Commented out so CI tests can pass
	# crash "Avoid using crash in production software!"
}

# Top level expects only run when using `roc test file.roc`
expect Bool.True != Bool.False
