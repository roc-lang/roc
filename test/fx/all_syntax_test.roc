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

# TODO issue #8646
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

while_loop = |limit| {
	var $count = 0
	var $sum = 0

	while $count < limit {
		$sum = $sum + $count
		$count = $count + 1
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

	with_curlies = 
	    if num == 5 {
	        "Five"
	    } else {
	        "NotFive"
	    }

	# else if
	if num == 3
		"Three"
	else if num == 4
		"Four"
	else
		one_line_if.concat(two_line_if).concat(with_curlies)
}

tuple_demo = 
# tuples can contain multiple types
	("Roc", 1)

# Here we use a type variable `a` to indicate this function works for a list of any type.
type_var : List(a) -> List(a)
type_var = |lst| lst

destructuring = || {
    tup = ("Roc", 1)
    (str, num) = tup

    rec = { x: 1, y: str } # TODO implement tuple access with `.index` ?
    { x, y } = rec

    (str, num, x, y)
}

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

number_literals : {
	usage_based: I64,
	explicit_u8: U8,
	explicit_i8: I8,
	explicit_u16: U16,
	explicit_i16: I16,
	explicit_u32: U32,
	explicit_i32: I32,
	explicit_u64: U64,
	explicit_i64: I64,
	explicit_u128: U128,
	explicit_i128: I128,
	# Note: F32, F64, and Dec literals use type inference which doesn't work with Str.inspect
	# So we use simple Dec literals here
	explicit_dec: Dec,
	hex: I64,
	octal: I64,
	binary: I64,
}
number_literals = {
	usage_based: 5,
	explicit_u8: 5,
	explicit_i8: 5,
	explicit_u16: 5,
	explicit_i16: 5,
	explicit_u32: 5,
	explicit_i32: 5,
	explicit_u64: 5,
	explicit_i64: 5,
	explicit_u128: 5,
	explicit_i128: 5,
	explicit_dec: 5.0,
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

my_concat = Str.concat

# Tags can have multiple payloads
multi_payload_tag : [Foo(I64, Str), Bar] -> Str
multi_payload_tag = |tag| match tag {
	Foo(num, name) => "Foo with ${num.to_str()} and ${name}"
	Bar => "Just Bar"
}

# Mark a tag union as open using `..`.
# This function accepts any tag union containing at least Red and Green.
color_to_str : [Red, Green, ..] -> Str
color_to_str = |color| match color {
	Red => "red"
	Green => "green"
	_ => "other color"
}

# TODO: Closed tag unions with `..[]]` - syntax not implemented yet
# str_to_color : Str -> [Red, Green, Blue, Other, ..[]]

# Type alias for an extensible tag union. You can use a type var (`others`) like so:
Letters(others) : [A, B, ..others]

# Use the type alias in a function signature. Pass `[C]` as `others`.
letter_to_str : Letters([C]) -> Str
letter_to_str = |letter| match letter {
	A => "A"
	B => "B"
	_ => "other letter"
}

# If you want to define a function that works for any type that has a specific method, you can use `where`:
stringify : a -> Str where [a.to_str : a -> Str]
stringify = |value| value.to_str()

main! = || {
	Stdout.line!("Hello, world!")
	StdoutAlias.line!("Hello, world! (using alias)")

	Stdout.line!(Str.inspect(number_operators(10, 5)))
	print!(boolean_operators(Bool.True, Bool.False))

	# pizza operator (|>) is gone, we now have static dispatch instead.
	# It allows you to call methods that are defined on the type (like `Animal.is_eq` above).
	print!("One".concat(" Two"))

	# If you want a very similar style for a function that is not defined on the type but is in scope, you can use `->`:
	print!("Three"->my_concat(" Four"))

	Stdout.line!(simple_match(Red))
	print!(match_list_patterns([1, 10]))
	Stdout.line!(match_tag_union_advanced(Ok({})))

	Stdout.line!(multiline_str(3))
	Stdout.line!("Unicode escape sequence: \u(00A0)")

	effect_demo!("This is an effectful function!")

	#Stdout.line!(Str.inspect(question_postfix(["1", "not a number", "100"])))

	sum = for_loop([1, 2, 3, 4, 5])
	print!(sum)

	expect sum == 15

	while_sum = while_loop(5)
	print!(while_sum)

	print!(dbg_keyword())

	Stdout.line!(if_demo(2))

	print!(tuple_demo)

	print!(type_var(["a", "b"]))

	print!(destructuring())

	# print!(record_update)

	print!({ x: 10, y: 20 }.x)

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

	print!(stringify(12345))

	# Tags with multiple payloads
	print!(multi_payload_tag(Foo(42, "hello")))

	# Open tag unions with `..`
	# This function accepts [Red, Green, ..] so we can pass Blue too
	print!(color_to_str(Blue))

	# Type alias for extensible tag union
	print!(letter_to_str(A))
	print!(letter_to_str(C)) # C is not in [A, B] but we passed it in the signature of letter_to_str

	# TODO Stdout.line!(readme);

	# Commented out so CI tests can pass
	# crash "Avoid using crash in production software!"
}

# Top level expects only run when using `roc test file.roc`
expect Bool.True != Bool.False
