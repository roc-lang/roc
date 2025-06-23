# META
~~~ini
description=A grab bag of all v0.1 syntax, heavily commented to show multiline formatting
type=file
~~~
# SOURCE
~~~roc
# This is a module comment!
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout exposing [line!, write!]

import # Comment after import keyword
	pf # Comment after qualifier
		.StdoutMultiline # Comment after ident
		exposing [ # Comment after exposing open
			line!, # Comment after exposed item
			write!, # Another after exposed item
		] # Comment after exposing close

import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]

import BadName as GoodName
import
	BadNameMultiline
		as
		GoodNameMultiline

Map(a, b) : List(a), (a -> b) -> List(b)
MapML( # Comment here
	a, # And here
	b,
) # And after the last arg
	: # And after the colon
		List( # Inside Tag args
			a, # After tag arg
		),
		(a -> b) -> # After arrow
			List( # Inside tag args
				b,
			) # And after the type decl

Foo : (Bar, Baz)

FooMultiline : ( # Comment after pattern tuple open
	Bar, # Comment after pattern tuple item
	Baz, # Another after pattern tuple item
) # Comment after pattern tuple close

Some(a) : { foo : Ok(a), bar : Something }
SomeMl(a) : { # After record open
	foo : Ok(a), # After field
	bar : Something, # After last field
}

SomeMultiline(a) : { # Comment after pattern record open
	foo # After field name
		: # Before field anno
			Ok(a), # Comment after pattern record field
	bar : Something, # Another after pattern record field
} # Comment after pattern record close

Maybe(a) : [Some(a), None]

MaybeMultiline(a) : [ # Comment after tag union open
	Some(a), # Comment after tag union member
	None, # Another after tag union member
] # Comment after tag union close

SomeFunc(a) : Maybe(a), a -> Maybe(a)

add_one_oneline = |num| if num 2 else 5

add_one : U64 -> U64
add_one = |num| {
	other = 1
	if num {
		dbg # After debug
			some_func() # After debug expr
		0
	} else {
		dbg 123
		other
	}
}

match_time = |
	a, # After arg
	b,
| # After args
	match a {
		Blue | Green | Red => {
			x = 12
			x
		}
		Blue # After pattern in alt
		| # Before pattern in alt
			Green
		| Red # After alt pattern
			=> {
				x = 12
				x
			}
		lower # After pattern comment
			=> 1
		"foo" => # After arrow comment
			100
		"foo" | "bar" => 200
		[1, 2, 3, .. as rest] # After pattern comment
			=> # After arrow comment
				123 # After branch comment

		# Just a random comment

		[1, 2 | 5, 3, .. as rest] => 123
		[
			1,
			2 | 5,
			3,
			.. # After DoubleDot
				as # Before alias
					rest, # After last pattern in list
		] => 123
		3.14 => 314
		3.14 | 6.28 => 314
		(1, 2, 3) => 123
		(1, 2 | 5, 3) => 123
		{ foo: 1, bar: 2, ..rest } => 12->add(34)
		{ # After pattern record open
			foo # After pattern record field name
				: # Before pattern record field value
					1, # After pattern record field
			bar: 2,
			.. # After spread operator
				rest, # After last field
		} => 12
		{ foo: 1, bar: 2 | 7 } => 12
		{
			foo: 1,
			bar: 2 | 7, # After last record field
		} => 12
		Ok(123) => 123
		Ok(Some(dude)) => dude
		TwoArgs("hello", Some("world")) => 1000
	}

expect # Comment after expect keyword
	blah == 1 # Comment after expect statement

main! : List(String) -> Result({}, _)
main! = |_| { # Yeah I can leave a comment here
	world = "World"
	var number = 123
	expect blah == 1
	tag = Blue
	return # Comment after return keyword
		tag # Comment after return statement

	# Just a random comment!

	...
	match_time(
		..., # Single args with comment
	)
	some_func(
		dbg # After debug
			42, # After debug expr
	)
	crash # Comment after crash keyword
		"Unreachable!" # Comment after crash statement
	tag_with_payload = Ok(number)
	interpolated = "Hello, ${world}"
	list = [
		add_one(
			dbg # After dbg in list
				number, # after dbg expr as arg
		), # Comment one
		456, # Comment two
		789, # Comment three
	]
	for n in list {
		Stdout.line!("Adding ${n} to ${number}")
		number = number + n
	}
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
	multiline_tuple = (
		123,
		"World",
		tag1,
		Ok(world), # This one has a comment
		(nested, tuple),
		[1, 2, 3],
	)
	bin_op_result = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
	Stdout.line!(interpolated)?
	Stdout.line!(
		"How about ${ # Comment after string interpolation open
			Num.toStr(number) # Comment after string interpolation expr
		} as a string?",
	)
} # Comment after top-level decl

empty : {}
empty = {}

tuple : Value((a, b, c))

expect {
	foo = 1 # This should work too
	blah = 1
	blah == foo
}
~~~
# PROBLEMS
**UNDECLARED TYPE**
The type ``Bar`` is not declared in this scope.

This type is referenced here:
**syntax_grab_bag.md:36:8:36:11:**
```roc
Foo : (Bar, Baz)
```


**UNDECLARED TYPE**
The type ``Baz`` is not declared in this scope.

This type is referenced here:
**syntax_grab_bag.md:36:13:36:16:**
```roc
Foo : (Bar, Baz)
```


**UNDECLARED TYPE**
The type ``Bar`` is not declared in this scope.

This type is referenced here:
**syntax_grab_bag.md:39:2:39:5:**
```roc
	Bar, # Comment after pattern tuple item
```


**UNDECLARED TYPE**
The type ``Baz`` is not declared in this scope.

This type is referenced here:
**syntax_grab_bag.md:40:2:40:5:**
```roc
	Baz, # Another after pattern tuple item
```


**UNDECLARED TYPE**
The type ``Something`` is not declared in this scope.

This type is referenced here:
**syntax_grab_bag.md:43:32:43:41:**
```roc
Some(a) : { foo : Ok(a), bar : Something }
```


**UNDECLARED TYPE**
The type ``Something`` is not declared in this scope.

This type is referenced here:
**syntax_grab_bag.md:46:8:46:17:**
```roc
	bar : Something, # After last field
```


**UNDECLARED TYPE**
The type ``Something`` is not declared in this scope.

This type is referenced here:
**syntax_grab_bag.md:53:8:53:17:**
```roc
	bar : Something, # Another after pattern record field
```


**UNDECLARED TYPE**
The type ``None`` is not declared in this scope.

This type is referenced here:
**syntax_grab_bag.md:56:22:56:26:**
```roc
Maybe(a) : [Some(a), None]
```


**UNDECLARED TYPE**
The type ``None`` is not declared in this scope.

This type is referenced here:
**syntax_grab_bag.md:60:2:60:6:**
```roc
	None, # Another after tag union member
```


**NOT IMPLEMENTED**
This feature is not yet implemented: top-level import

**NOT IMPLEMENTED**
This feature is not yet implemented: top-level import

**NOT IMPLEMENTED**
This feature is not yet implemented: top-level import

**NOT IMPLEMENTED**
This feature is not yet implemented: top-level import

**NOT IMPLEMENTED**
This feature is not yet implemented: top-level import

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize if_then_else expression

**UNUSED VARIABLE**
Variable ``num`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_num` to suppress this warning.
The unused variable is declared here:
**syntax_grab_bag.md:65:20:65:23:**
```roc
add_one_oneline = |num| if num 2 else 5
```


**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize if_then_else expression

**UNUSED VARIABLE**
Variable ``other`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_other` to suppress this warning.
The unused variable is declared here:
**syntax_grab_bag.md:69:2:69:7:**
```roc
	other = 1
```


**UNUSED VARIABLE**
Variable ``num`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_num` to suppress this warning.
The unused variable is declared here:
**syntax_grab_bag.md:68:12:68:15:**
```roc
add_one = |num| {
```


**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize match expression

**UNUSED VARIABLE**
Variable ``b`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**syntax_grab_bag.md:82:2:82:3:**
```roc
	b,
```


**UNUSED VARIABLE**
Variable ``a`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_a` to suppress this warning.
The unused variable is declared here:
**syntax_grab_bag.md:81:2:81:3:**
```roc
	a, # After arg
```


**NOT IMPLEMENTED**
This feature is not yet implemented: top-level expect

**UNDECLARED TYPE**
The type ``String`` is not declared in this scope.

This type is referenced here:
**syntax_grab_bag.md:143:14:143:20:**
```roc
main! : List(String) -> Result({}, _)
```


**NOT IMPLEMENTED**
This feature is not yet implemented: statement type in block

**NOT IMPLEMENTED**
This feature is not yet implemented: statement type in block

**NOT IMPLEMENTED**
This feature is not yet implemented: ...

**NOT IMPLEMENTED**
This feature is not yet implemented: ...

**UNDEFINED VARIABLE**
Nothing is named `some_func` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize dbg expression

**NOT IMPLEMENTED**
This feature is not yet implemented: crash statement

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize dbg expression

**NOT IMPLEMENTED**
This feature is not yet implemented: statement type in block

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

**UNDEFINED VARIABLE**
Nothing is named `nested` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `tag1` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `nested` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented: binop

**NOT IMPLEMENTED**
This feature is not yet implemented: binop

**NOT IMPLEMENTED**
This feature is not yet implemented: binop

**NOT IMPLEMENTED**
This feature is not yet implemented: binop

**NOT IMPLEMENTED**
This feature is not yet implemented: binop

**NOT IMPLEMENTED**
This feature is not yet implemented: binop

**NOT IMPLEMENTED**
This feature is not yet implemented: binop

**NOT IMPLEMENTED**
This feature is not yet implemented: binop

**NOT IMPLEMENTED**
This feature is not yet implemented: binop

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize suffix_single_question expression

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize suffix_single_question expression

**UNDEFINED VARIABLE**
Nothing is named `line!` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `toStr` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNUSED VARIABLE**
Variable ``multiline_tuple`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_multiline_tuple` to suppress this warning.
The unused variable is declared here:
**syntax_grab_bag.md:180:2:180:17:**
```roc
	multiline_tuple = (
```


**UNUSED VARIABLE**
Variable ``record`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_record` to suppress this warning.
The unused variable is declared here:
**syntax_grab_bag.md:178:2:178:8:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```


**UNUSED VARIABLE**
Variable ``tag_with_payload`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_tag_with_payload` to suppress this warning.
The unused variable is declared here:
**syntax_grab_bag.md:164:2:164:18:**
```roc
	tag_with_payload = Ok(number)
```


**UNUSED VARIABLE**
Variable ``list`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_list` to suppress this warning.
The unused variable is declared here:
**syntax_grab_bag.md:166:2:166:6:**
```roc
	list = [
```


**UNUSED VARIABLE**
Variable ``bin_op_result`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_bin_op_result` to suppress this warning.
The unused variable is declared here:
**syntax_grab_bag.md:188:2:188:15:**
```roc
	bin_op_result = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
```


**UNUSED VARIABLE**
Variable ``static_dispatch_style`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_static_dispatch_style` to suppress this warning.
The unused variable is declared here:
**syntax_grab_bag.md:189:2:189:23:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```


**UNUSED VARIABLE**
Variable ``interpolated`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_interpolated` to suppress this warning.
The unused variable is declared here:
**syntax_grab_bag.md:165:2:165:14:**
```roc
	interpolated = "Hello, ${world}"
```


**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

**NOT IMPLEMENTED**
This feature is not yet implemented: top-level expect

# TOKENS
~~~zig
Newline(1:2-1:28),
KwApp(2:1-2:4),OpenSquare(2:5-2:6),LowerIdent(2:6-2:11),CloseSquare(2:11-2:12),OpenCurly(2:13-2:14),LowerIdent(2:15-2:17),OpColon(2:17-2:18),KwPlatform(2:19-2:27),StringStart(2:28-2:29),StringPart(2:29-2:54),StringEnd(2:54-2:55),CloseCurly(2:56-2:57),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(4:1-4:7),LowerIdent(4:8-4:10),NoSpaceDotUpperIdent(4:10-4:17),KwExposing(4:18-4:26),OpenSquare(4:27-4:28),LowerIdent(4:28-4:33),Comma(4:33-4:34),LowerIdent(4:35-4:41),CloseSquare(4:41-4:42),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(6:1-6:7),Newline(6:9-6:38),
LowerIdent(7:2-7:4),Newline(7:6-7:30),
DotUpperIdent(8:3-8:19),Newline(8:21-8:41),
KwExposing(9:3-9:11),OpenSquare(9:12-9:13),Newline(9:15-9:43),
LowerIdent(10:4-10:9),Comma(10:9-10:10),Newline(10:12-10:39),
LowerIdent(11:4-11:10),Comma(11:10-11:11),Newline(11:13-11:40),
CloseSquare(12:3-12:4),Newline(12:6-12:35),
Newline(1:1-1:1),
KwImport(14:1-14:7),LowerIdent(14:8-14:11),NoSpaceDotUpperIdent(14:11-14:21),KwExposing(14:22-14:30),OpenSquare(14:31-14:32),LowerIdent(14:32-14:36),KwAs(14:37-14:39),LowerIdent(14:40-14:48),Comma(14:48-14:49),UpperIdent(14:50-14:54),KwAs(14:55-14:57),UpperIdent(14:58-14:71),Comma(14:71-14:72),UpperIdent(14:73-14:79),DotStar(14:79-14:81),CloseSquare(14:81-14:82),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(16:1-16:7),UpperIdent(16:8-16:15),KwAs(16:16-16:18),UpperIdent(16:19-16:27),Newline(1:1-1:1),
KwImport(17:1-17:7),Newline(1:1-1:1),
UpperIdent(18:2-18:18),Newline(1:1-1:1),
KwAs(19:3-19:5),Newline(1:1-1:1),
UpperIdent(20:3-20:20),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(22:1-22:4),NoSpaceOpenRound(22:4-22:5),LowerIdent(22:5-22:6),Comma(22:6-22:7),LowerIdent(22:8-22:9),CloseRound(22:9-22:10),OpColon(22:11-22:12),UpperIdent(22:13-22:17),NoSpaceOpenRound(22:17-22:18),LowerIdent(22:18-22:19),CloseRound(22:19-22:20),Comma(22:20-22:21),OpenRound(22:22-22:23),LowerIdent(22:23-22:24),OpArrow(22:25-22:27),LowerIdent(22:28-22:29),CloseRound(22:29-22:30),OpArrow(22:31-22:33),UpperIdent(22:34-22:38),NoSpaceOpenRound(22:38-22:39),LowerIdent(22:39-22:40),CloseRound(22:40-22:41),Newline(1:1-1:1),
UpperIdent(23:1-23:6),NoSpaceOpenRound(23:6-23:7),Newline(23:9-23:22),
LowerIdent(24:2-24:3),Comma(24:3-24:4),Newline(24:6-24:15),
LowerIdent(25:2-25:3),Comma(25:3-25:4),Newline(1:1-1:1),
CloseRound(26:1-26:2),Newline(26:4-26:27),
OpColon(27:2-27:3),Newline(27:5-27:25),
UpperIdent(28:3-28:7),NoSpaceOpenRound(28:7-28:8),Newline(28:10-28:26),
LowerIdent(29:4-29:5),Comma(29:5-29:6),Newline(29:8-29:22),
CloseRound(30:3-30:4),Comma(30:4-30:5),Newline(1:1-1:1),
OpenRound(31:3-31:4),LowerIdent(31:4-31:5),OpArrow(31:6-31:8),LowerIdent(31:9-31:10),CloseRound(31:10-31:11),OpArrow(31:12-31:14),Newline(31:16-31:28),
UpperIdent(32:4-32:8),NoSpaceOpenRound(32:8-32:9),Newline(32:11-32:27),
LowerIdent(33:5-33:6),Comma(33:6-33:7),Newline(1:1-1:1),
CloseRound(34:4-34:5),Newline(34:7-34:31),
Newline(1:1-1:1),
UpperIdent(36:1-36:4),OpColon(36:5-36:6),OpenRound(36:7-36:8),UpperIdent(36:8-36:11),Comma(36:11-36:12),UpperIdent(36:13-36:16),CloseRound(36:16-36:17),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(38:1-38:13),OpColon(38:14-38:15),OpenRound(38:16-38:17),Newline(38:19-38:52),
UpperIdent(39:2-39:5),Comma(39:5-39:6),Newline(39:8-39:41),
UpperIdent(40:2-40:5),Comma(40:5-40:6),Newline(40:8-40:41),
CloseRound(41:1-41:2),Newline(41:4-41:38),
Newline(1:1-1:1),
UpperIdent(43:1-43:5),NoSpaceOpenRound(43:5-43:6),LowerIdent(43:6-43:7),CloseRound(43:7-43:8),OpColon(43:9-43:10),OpenCurly(43:11-43:12),LowerIdent(43:13-43:16),OpColon(43:17-43:18),UpperIdent(43:19-43:21),NoSpaceOpenRound(43:21-43:22),LowerIdent(43:22-43:23),CloseRound(43:23-43:24),Comma(43:24-43:25),LowerIdent(43:26-43:29),OpColon(43:30-43:31),UpperIdent(43:32-43:41),CloseCurly(43:42-43:43),Newline(1:1-1:1),
UpperIdent(44:1-44:7),NoSpaceOpenRound(44:7-44:8),LowerIdent(44:8-44:9),CloseRound(44:9-44:10),OpColon(44:11-44:12),OpenCurly(44:13-44:14),Newline(44:16-44:34),
LowerIdent(45:2-45:5),OpColon(45:6-45:7),UpperIdent(45:8-45:10),NoSpaceOpenRound(45:10-45:11),LowerIdent(45:11-45:12),CloseRound(45:12-45:13),Comma(45:13-45:14),Newline(45:16-45:28),
LowerIdent(46:2-46:5),OpColon(46:6-46:7),UpperIdent(46:8-46:17),Comma(46:17-46:18),Newline(46:20-46:37),
CloseCurly(47:1-47:2),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(49:1-49:14),NoSpaceOpenRound(49:14-49:15),LowerIdent(49:15-49:16),CloseRound(49:16-49:17),OpColon(49:18-49:19),OpenCurly(49:20-49:21),Newline(49:23-49:57),
LowerIdent(50:2-50:5),Newline(50:7-50:24),
OpColon(51:3-51:4),Newline(51:6-51:24),
UpperIdent(52:4-52:6),NoSpaceOpenRound(52:6-52:7),LowerIdent(52:7-52:8),CloseRound(52:8-52:9),Comma(52:9-52:10),Newline(52:12-52:47),
LowerIdent(53:2-53:5),OpColon(53:6-53:7),UpperIdent(53:8-53:17),Comma(53:17-53:18),Newline(53:20-53:55),
CloseCurly(54:1-54:2),Newline(54:4-54:39),
Newline(1:1-1:1),
UpperIdent(56:1-56:6),NoSpaceOpenRound(56:6-56:7),LowerIdent(56:7-56:8),CloseRound(56:8-56:9),OpColon(56:10-56:11),OpenSquare(56:12-56:13),UpperIdent(56:13-56:17),NoSpaceOpenRound(56:17-56:18),LowerIdent(56:18-56:19),CloseRound(56:19-56:20),Comma(56:20-56:21),UpperIdent(56:22-56:26),CloseSquare(56:26-56:27),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(58:1-58:15),NoSpaceOpenRound(58:15-58:16),LowerIdent(58:16-58:17),CloseRound(58:17-58:18),OpColon(58:19-58:20),OpenSquare(58:21-58:22),Newline(58:24-58:53),
UpperIdent(59:2-59:6),NoSpaceOpenRound(59:6-59:7),LowerIdent(59:7-59:8),CloseRound(59:8-59:9),Comma(59:9-59:10),Newline(59:12-59:43),
UpperIdent(60:2-60:6),Comma(60:6-60:7),Newline(60:9-60:40),
CloseSquare(61:1-61:2),Newline(61:4-61:34),
Newline(1:1-1:1),
UpperIdent(63:1-63:9),NoSpaceOpenRound(63:9-63:10),LowerIdent(63:10-63:11),CloseRound(63:11-63:12),OpColon(63:13-63:14),UpperIdent(63:15-63:20),NoSpaceOpenRound(63:20-63:21),LowerIdent(63:21-63:22),CloseRound(63:22-63:23),Comma(63:23-63:24),LowerIdent(63:25-63:26),OpArrow(63:27-63:29),UpperIdent(63:30-63:35),NoSpaceOpenRound(63:35-63:36),LowerIdent(63:36-63:37),CloseRound(63:37-63:38),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(65:1-65:16),OpAssign(65:17-65:18),OpBar(65:19-65:20),LowerIdent(65:20-65:23),OpBar(65:23-65:24),KwIf(65:25-65:27),LowerIdent(65:28-65:31),Int(65:32-65:33),KwElse(65:34-65:38),Int(65:39-65:40),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(67:1-67:8),OpColon(67:9-67:10),UpperIdent(67:11-67:14),OpArrow(67:15-67:17),UpperIdent(67:18-67:21),Newline(1:1-1:1),
LowerIdent(68:1-68:8),OpAssign(68:9-68:10),OpBar(68:11-68:12),LowerIdent(68:12-68:15),OpBar(68:15-68:16),OpenCurly(68:17-68:18),Newline(1:1-1:1),
LowerIdent(69:2-69:7),OpAssign(69:8-69:9),Int(69:10-69:11),Newline(1:1-1:1),
KwIf(70:2-70:4),LowerIdent(70:5-70:8),OpenCurly(70:9-70:10),Newline(1:1-1:1),
KwDbg(71:3-71:6),Newline(71:8-71:20),
LowerIdent(72:4-72:13),NoSpaceOpenRound(72:13-72:14),CloseRound(72:14-72:15),Newline(72:17-72:34),
Int(73:3-73:4),Newline(1:1-1:1),
CloseCurly(74:2-74:3),KwElse(74:4-74:8),OpenCurly(74:9-74:10),Newline(1:1-1:1),
KwDbg(75:3-75:6),Int(75:7-75:10),Newline(1:1-1:1),
LowerIdent(76:3-76:8),Newline(1:1-1:1),
CloseCurly(77:2-77:3),Newline(1:1-1:1),
CloseCurly(78:1-78:2),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(80:1-80:11),OpAssign(80:12-80:13),OpBar(80:14-80:15),Newline(1:1-1:1),
LowerIdent(81:2-81:3),Comma(81:3-81:4),Newline(81:6-81:16),
LowerIdent(82:2-82:3),Comma(82:3-82:4),Newline(1:1-1:1),
OpBar(83:1-83:2),Newline(83:4-83:15),
KwMatch(84:2-84:7),LowerIdent(84:8-84:9),OpenCurly(84:10-84:11),Newline(1:1-1:1),
UpperIdent(85:3-85:7),OpBar(85:8-85:9),UpperIdent(85:10-85:15),OpBar(85:16-85:17),UpperIdent(85:18-85:21),OpFatArrow(85:22-85:24),OpenCurly(85:25-85:26),Newline(1:1-1:1),
LowerIdent(86:4-86:5),OpAssign(86:6-86:7),Int(86:8-86:10),Newline(1:1-1:1),
LowerIdent(87:4-87:5),Newline(1:1-1:1),
CloseCurly(88:3-88:4),Newline(1:1-1:1),
UpperIdent(89:3-89:7),Newline(89:9-89:30),
OpBar(90:3-90:4),Newline(90:6-90:28),
UpperIdent(91:4-91:9),Newline(1:1-1:1),
OpBar(92:3-92:4),UpperIdent(92:5-92:8),Newline(92:10-92:28),
OpFatArrow(93:4-93:6),OpenCurly(93:7-93:8),Newline(1:1-1:1),
LowerIdent(94:5-94:6),OpAssign(94:7-94:8),Int(94:9-94:11),Newline(1:1-1:1),
LowerIdent(95:5-95:6),Newline(1:1-1:1),
CloseCurly(96:4-96:5),Newline(1:1-1:1),
LowerIdent(97:3-97:8),Newline(97:10-97:32),
OpFatArrow(98:4-98:6),Int(98:7-98:8),Newline(1:1-1:1),
StringStart(99:3-99:4),StringPart(99:4-99:7),StringEnd(99:7-99:8),OpFatArrow(99:9-99:11),Newline(99:13-99:33),
Int(100:4-100:7),Newline(1:1-1:1),
StringStart(101:3-101:4),StringPart(101:4-101:7),StringEnd(101:7-101:8),OpBar(101:9-101:10),StringStart(101:11-101:12),StringPart(101:12-101:15),StringEnd(101:15-101:16),OpFatArrow(101:17-101:19),Int(101:20-101:23),Newline(1:1-1:1),
OpenSquare(102:3-102:4),Int(102:4-102:5),Comma(102:5-102:6),Int(102:7-102:8),Comma(102:8-102:9),Int(102:10-102:11),Comma(102:11-102:12),DoubleDot(102:13-102:15),KwAs(102:16-102:18),LowerIdent(102:19-102:23),CloseSquare(102:23-102:24),Newline(102:26-102:48),
OpFatArrow(103:4-103:6),Newline(103:8-103:28),
Int(104:5-104:8),Newline(104:10-104:31),
Newline(1:1-1:1),
Newline(106:4-106:26),
Newline(1:1-1:1),
OpenSquare(108:3-108:4),Int(108:4-108:5),Comma(108:5-108:6),Int(108:7-108:8),OpBar(108:9-108:10),Int(108:11-108:12),Comma(108:12-108:13),Int(108:14-108:15),Comma(108:15-108:16),DoubleDot(108:17-108:19),KwAs(108:20-108:22),LowerIdent(108:23-108:27),CloseSquare(108:27-108:28),OpFatArrow(108:29-108:31),Int(108:32-108:35),Newline(1:1-1:1),
OpenSquare(109:3-109:4),Newline(1:1-1:1),
Int(110:4-110:5),Comma(110:5-110:6),Newline(1:1-1:1),
Int(111:4-111:5),OpBar(111:6-111:7),Int(111:8-111:9),Comma(111:9-111:10),Newline(1:1-1:1),
Int(112:4-112:5),Comma(112:5-112:6),Newline(1:1-1:1),
DoubleDot(113:4-113:6),Newline(113:8-113:24),
KwAs(114:5-114:7),Newline(114:9-114:22),
LowerIdent(115:6-115:10),Comma(115:10-115:11),Newline(115:13-115:40),
CloseSquare(116:3-116:4),OpFatArrow(116:5-116:7),Int(116:8-116:11),Newline(1:1-1:1),
Float(117:3-117:7),OpFatArrow(117:8-117:10),Int(117:11-117:14),Newline(1:1-1:1),
Float(118:3-118:7),OpBar(118:8-118:9),Float(118:10-118:14),OpFatArrow(118:15-118:17),Int(118:18-118:21),Newline(1:1-1:1),
OpenRound(119:3-119:4),Int(119:4-119:5),Comma(119:5-119:6),Int(119:7-119:8),Comma(119:8-119:9),Int(119:10-119:11),CloseRound(119:11-119:12),OpFatArrow(119:13-119:15),Int(119:16-119:19),Newline(1:1-1:1),
OpenRound(120:3-120:4),Int(120:4-120:5),Comma(120:5-120:6),Int(120:7-120:8),OpBar(120:9-120:10),Int(120:11-120:12),Comma(120:12-120:13),Int(120:14-120:15),CloseRound(120:15-120:16),OpFatArrow(120:17-120:19),Int(120:20-120:23),Newline(1:1-1:1),
OpenCurly(121:3-121:4),LowerIdent(121:5-121:8),OpColon(121:8-121:9),Int(121:10-121:11),Comma(121:11-121:12),LowerIdent(121:13-121:16),OpColon(121:16-121:17),Int(121:18-121:19),Comma(121:19-121:20),DoubleDot(121:21-121:23),LowerIdent(121:23-121:27),CloseCurly(121:28-121:29),OpFatArrow(121:30-121:32),Int(121:33-121:35),OpArrow(121:35-121:37),LowerIdent(121:37-121:40),NoSpaceOpenRound(121:40-121:41),Int(121:41-121:43),CloseRound(121:43-121:44),Newline(1:1-1:1),
OpenCurly(122:3-122:4),Newline(122:6-122:32),
LowerIdent(123:4-123:7),Newline(123:9-123:41),
OpColon(124:5-124:6),Newline(124:8-124:42),
Int(125:6-125:7),Comma(125:7-125:8),Newline(125:10-125:37),
LowerIdent(126:4-126:7),OpColon(126:7-126:8),Int(126:9-126:10),Comma(126:10-126:11),Newline(1:1-1:1),
DoubleDot(127:4-127:6),Newline(127:8-127:30),
LowerIdent(128:5-128:9),Comma(128:9-128:10),Newline(128:12-128:29),
CloseCurly(129:3-129:4),OpFatArrow(129:5-129:7),Int(129:8-129:10),Newline(1:1-1:1),
OpenCurly(130:3-130:4),LowerIdent(130:5-130:8),OpColon(130:8-130:9),Int(130:10-130:11),Comma(130:11-130:12),LowerIdent(130:13-130:16),OpColon(130:16-130:17),Int(130:18-130:19),OpBar(130:20-130:21),Int(130:22-130:23),CloseCurly(130:24-130:25),OpFatArrow(130:26-130:28),Int(130:29-130:31),Newline(1:1-1:1),
OpenCurly(131:3-131:4),Newline(1:1-1:1),
LowerIdent(132:4-132:7),OpColon(132:7-132:8),Int(132:9-132:10),Comma(132:10-132:11),Newline(1:1-1:1),
LowerIdent(133:4-133:7),OpColon(133:7-133:8),Int(133:9-133:10),OpBar(133:11-133:12),Int(133:13-133:14),Comma(133:14-133:15),Newline(133:17-133:41),
CloseCurly(134:3-134:4),OpFatArrow(134:5-134:7),Int(134:8-134:10),Newline(1:1-1:1),
UpperIdent(135:3-135:5),NoSpaceOpenRound(135:5-135:6),Int(135:6-135:9),CloseRound(135:9-135:10),OpFatArrow(135:11-135:13),Int(135:14-135:17),Newline(1:1-1:1),
UpperIdent(136:3-136:5),NoSpaceOpenRound(136:5-136:6),UpperIdent(136:6-136:10),NoSpaceOpenRound(136:10-136:11),LowerIdent(136:11-136:15),CloseRound(136:15-136:16),CloseRound(136:16-136:17),OpFatArrow(136:18-136:20),LowerIdent(136:21-136:25),Newline(1:1-1:1),
UpperIdent(137:3-137:10),NoSpaceOpenRound(137:10-137:11),StringStart(137:11-137:12),StringPart(137:12-137:17),StringEnd(137:17-137:18),Comma(137:18-137:19),UpperIdent(137:20-137:24),NoSpaceOpenRound(137:24-137:25),StringStart(137:25-137:26),StringPart(137:26-137:31),StringEnd(137:31-137:32),CloseRound(137:32-137:33),CloseRound(137:33-137:34),OpFatArrow(137:35-137:37),Int(137:38-137:42),Newline(1:1-1:1),
CloseCurly(138:2-138:3),Newline(1:1-1:1),
Newline(1:1-1:1),
KwExpect(140:1-140:7),Newline(140:9-140:38),
LowerIdent(141:2-141:6),OpEquals(141:7-141:9),Int(141:10-141:11),Newline(141:13-141:44),
Newline(1:1-1:1),
LowerIdent(143:1-143:6),OpColon(143:7-143:8),UpperIdent(143:9-143:13),NoSpaceOpenRound(143:13-143:14),UpperIdent(143:14-143:20),CloseRound(143:20-143:21),OpArrow(143:22-143:24),UpperIdent(143:25-143:31),NoSpaceOpenRound(143:31-143:32),OpenCurly(143:32-143:33),CloseCurly(143:33-143:34),Comma(143:34-143:35),Underscore(143:36-143:37),CloseRound(143:37-143:38),Newline(1:1-1:1),
LowerIdent(144:1-144:6),OpAssign(144:7-144:8),OpBar(144:9-144:10),Underscore(144:10-144:11),OpBar(144:11-144:12),OpenCurly(144:13-144:14),Newline(144:16-144:48),
LowerIdent(145:2-145:7),OpAssign(145:8-145:9),StringStart(145:10-145:11),StringPart(145:11-145:16),StringEnd(145:16-145:17),Newline(1:1-1:1),
KwVar(146:2-146:5),LowerIdent(146:6-146:12),OpAssign(146:13-146:14),Int(146:15-146:18),Newline(1:1-1:1),
KwExpect(147:2-147:8),LowerIdent(147:9-147:13),OpEquals(147:14-147:16),Int(147:17-147:18),Newline(1:1-1:1),
LowerIdent(148:2-148:5),OpAssign(148:6-148:7),UpperIdent(148:8-148:12),Newline(1:1-1:1),
KwReturn(149:2-149:8),Newline(149:10-149:39),
LowerIdent(150:3-150:6),Newline(150:8-150:39),
Newline(1:1-1:1),
Newline(152:3-152:26),
Newline(1:1-1:1),
TripleDot(154:2-154:5),Newline(1:1-1:1),
LowerIdent(155:2-155:12),NoSpaceOpenRound(155:12-155:13),Newline(1:1-1:1),
TripleDot(156:3-156:6),Comma(156:6-156:7),Newline(156:9-156:34),
CloseRound(157:2-157:3),Newline(1:1-1:1),
LowerIdent(158:2-158:11),NoSpaceOpenRound(158:11-158:12),Newline(1:1-1:1),
KwDbg(159:3-159:6),Newline(159:8-159:20),
Int(160:4-160:6),Comma(160:6-160:7),Newline(160:9-160:26),
CloseRound(161:2-161:3),Newline(1:1-1:1),
KwCrash(162:2-162:7),Newline(162:9-162:37),
StringStart(163:3-163:4),StringPart(163:4-163:16),StringEnd(163:16-163:17),Newline(163:19-163:49),
LowerIdent(164:2-164:18),OpAssign(164:19-164:20),UpperIdent(164:21-164:23),NoSpaceOpenRound(164:23-164:24),LowerIdent(164:24-164:30),CloseRound(164:30-164:31),Newline(1:1-1:1),
LowerIdent(165:2-165:14),OpAssign(165:15-165:16),StringStart(165:17-165:18),StringPart(165:18-165:25),OpenStringInterpolation(165:25-165:27),LowerIdent(165:27-165:32),CloseStringInterpolation(165:32-165:33),StringPart(165:33-165:33),StringEnd(165:33-165:34),Newline(1:1-1:1),
LowerIdent(166:2-166:6),OpAssign(166:7-166:8),OpenSquare(166:9-166:10),Newline(1:1-1:1),
LowerIdent(167:3-167:10),NoSpaceOpenRound(167:10-167:11),Newline(1:1-1:1),
KwDbg(168:4-168:7),Newline(168:9-168:27),
LowerIdent(169:5-169:11),Comma(169:11-169:12),Newline(169:14-169:36),
CloseRound(170:3-170:4),Comma(170:4-170:5),Newline(170:7-170:19),
Int(171:3-171:6),Comma(171:6-171:7),Newline(171:9-171:21),
Int(172:3-172:6),Comma(172:6-172:7),Newline(172:9-172:23),
CloseSquare(173:2-173:3),Newline(1:1-1:1),
KwFor(174:2-174:5),LowerIdent(174:6-174:7),KwIn(174:8-174:10),LowerIdent(174:11-174:15),OpenCurly(174:16-174:17),Newline(1:1-1:1),
UpperIdent(175:3-175:9),NoSpaceDotLowerIdent(175:9-175:15),NoSpaceOpenRound(175:15-175:16),StringStart(175:16-175:17),StringPart(175:17-175:24),OpenStringInterpolation(175:24-175:26),LowerIdent(175:26-175:27),CloseStringInterpolation(175:27-175:28),StringPart(175:28-175:32),OpenStringInterpolation(175:32-175:34),LowerIdent(175:34-175:40),CloseStringInterpolation(175:40-175:41),StringPart(175:41-175:41),StringEnd(175:41-175:42),CloseRound(175:42-175:43),Newline(1:1-1:1),
LowerIdent(176:3-176:9),OpAssign(176:10-176:11),LowerIdent(176:12-176:18),OpPlus(176:19-176:20),LowerIdent(176:21-176:22),Newline(1:1-1:1),
CloseCurly(177:2-177:3),Newline(1:1-1:1),
LowerIdent(178:2-178:8),OpAssign(178:9-178:10),OpenCurly(178:11-178:12),LowerIdent(178:13-178:16),OpColon(178:16-178:17),Int(178:18-178:21),Comma(178:21-178:22),LowerIdent(178:23-178:26),OpColon(178:26-178:27),StringStart(178:28-178:29),StringPart(178:29-178:34),StringEnd(178:34-178:35),Comma(178:35-178:36),LowerIdent(178:37-178:40),OpColon(178:40-178:41),LowerIdent(178:42-178:45),Comma(178:45-178:46),LowerIdent(178:47-178:50),OpColon(178:50-178:51),UpperIdent(178:52-178:54),NoSpaceOpenRound(178:54-178:55),LowerIdent(178:55-178:60),CloseRound(178:60-178:61),Comma(178:61-178:62),LowerIdent(178:63-178:69),CloseCurly(178:70-178:71),Newline(1:1-1:1),
LowerIdent(179:2-179:7),OpAssign(179:8-179:9),OpenRound(179:10-179:11),Int(179:11-179:14),Comma(179:14-179:15),StringStart(179:16-179:17),StringPart(179:17-179:22),StringEnd(179:22-179:23),Comma(179:23-179:24),LowerIdent(179:25-179:28),Comma(179:28-179:29),UpperIdent(179:30-179:32),NoSpaceOpenRound(179:32-179:33),LowerIdent(179:33-179:38),CloseRound(179:38-179:39),Comma(179:39-179:40),OpenRound(179:41-179:42),LowerIdent(179:42-179:48),Comma(179:48-179:49),LowerIdent(179:50-179:55),CloseRound(179:55-179:56),Comma(179:56-179:57),OpenSquare(179:58-179:59),Int(179:59-179:60),Comma(179:60-179:61),Int(179:62-179:63),Comma(179:63-179:64),Int(179:65-179:66),CloseSquare(179:66-179:67),CloseRound(179:67-179:68),Newline(1:1-1:1),
LowerIdent(180:2-180:17),OpAssign(180:18-180:19),OpenRound(180:20-180:21),Newline(1:1-1:1),
Int(181:3-181:6),Comma(181:6-181:7),Newline(1:1-1:1),
StringStart(182:3-182:4),StringPart(182:4-182:9),StringEnd(182:9-182:10),Comma(182:10-182:11),Newline(1:1-1:1),
LowerIdent(183:3-183:7),Comma(183:7-183:8),Newline(1:1-1:1),
UpperIdent(184:3-184:5),NoSpaceOpenRound(184:5-184:6),LowerIdent(184:6-184:11),CloseRound(184:11-184:12),Comma(184:12-184:13),Newline(184:15-184:38),
OpenRound(185:3-185:4),LowerIdent(185:4-185:10),Comma(185:10-185:11),LowerIdent(185:12-185:17),CloseRound(185:17-185:18),Comma(185:18-185:19),Newline(1:1-1:1),
OpenSquare(186:3-186:4),Int(186:4-186:5),Comma(186:5-186:6),Int(186:7-186:8),Comma(186:8-186:9),Int(186:10-186:11),CloseSquare(186:11-186:12),Comma(186:12-186:13),Newline(1:1-1:1),
CloseRound(187:2-187:3),Newline(1:1-1:1),
LowerIdent(188:2-188:15),OpAssign(188:16-188:17),UpperIdent(188:18-188:21),NoSpaceOpenRound(188:21-188:22),LowerIdent(188:22-188:25),CloseRound(188:25-188:26),OpDoubleQuestion(188:27-188:29),Int(188:30-188:32),OpGreaterThan(188:33-188:34),Int(188:35-188:36),OpStar(188:37-188:38),Int(188:39-188:40),OpOr(188:41-188:43),Int(188:44-188:46),OpPlus(188:47-188:48),Int(188:49-188:50),OpLessThan(188:51-188:52),Int(188:53-188:54),OpAnd(188:55-188:58),Int(188:59-188:61),OpBinaryMinus(188:62-188:63),Int(188:64-188:65),OpGreaterThanOrEq(188:66-188:68),Int(188:69-188:71),OpOr(188:72-188:74),Int(188:75-188:77),OpLessThanOrEq(188:78-188:80),Int(188:81-188:82),OpSlash(188:83-188:84),Int(188:85-188:86),Newline(1:1-1:1),
LowerIdent(189:2-189:23),OpAssign(189:24-189:25),LowerIdent(189:26-189:33),NoSpaceOpenRound(189:33-189:34),LowerIdent(189:34-189:38),CloseRound(189:38-189:39),NoSpaceOpQuestion(189:39-189:40),NoSpaceDotLowerIdent(189:40-189:63),NoSpaceOpenRound(189:63-189:64),CloseRound(189:64-189:65),NoSpaceOpQuestion(189:65-189:66),NoSpaceDotLowerIdent(189:66-189:94),NoSpaceOpenRound(189:94-189:95),CloseRound(189:95-189:96),NoSpaceOpQuestion(189:96-189:97),NoSpaceDotLowerIdent(189:97-189:110),NoSpaceOpQuestion(189:110-189:111),Newline(1:1-1:1),
UpperIdent(190:2-190:8),NoSpaceDotLowerIdent(190:8-190:14),NoSpaceOpenRound(190:14-190:15),LowerIdent(190:15-190:27),CloseRound(190:27-190:28),NoSpaceOpQuestion(190:28-190:29),Newline(1:1-1:1),
UpperIdent(191:2-191:8),NoSpaceDotLowerIdent(191:8-191:14),NoSpaceOpenRound(191:14-191:15),Newline(1:1-1:1),
StringStart(192:3-192:4),StringPart(192:4-192:14),OpenStringInterpolation(192:14-192:16),Newline(192:18-192:58),
UpperIdent(193:4-193:7),NoSpaceDotLowerIdent(193:7-193:13),NoSpaceOpenRound(193:13-193:14),LowerIdent(193:14-193:20),CloseRound(193:20-193:21),Newline(193:23-193:63),
CloseStringInterpolation(194:3-194:4),StringPart(194:4-194:17),StringEnd(194:17-194:18),Comma(194:18-194:19),Newline(1:1-1:1),
CloseRound(195:2-195:3),Newline(1:1-1:1),
CloseCurly(196:1-196:2),Newline(196:4-196:33),
Newline(1:1-1:1),
LowerIdent(198:1-198:6),OpColon(198:7-198:8),OpenCurly(198:9-198:10),CloseCurly(198:10-198:11),Newline(1:1-1:1),
LowerIdent(199:1-199:6),OpAssign(199:7-199:8),OpenCurly(199:9-199:10),CloseCurly(199:10-199:11),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(201:1-201:6),OpColon(201:7-201:8),UpperIdent(201:9-201:14),NoSpaceOpenRound(201:14-201:15),NoSpaceOpenRound(201:15-201:16),LowerIdent(201:16-201:17),Comma(201:17-201:18),LowerIdent(201:19-201:20),Comma(201:20-201:21),LowerIdent(201:22-201:23),CloseRound(201:23-201:24),CloseRound(201:24-201:25),Newline(1:1-1:1),
Newline(1:1-1:1),
KwExpect(203:1-203:7),OpenCurly(203:8-203:9),Newline(1:1-1:1),
LowerIdent(204:2-204:5),OpAssign(204:6-204:7),Int(204:8-204:9),Newline(204:11-204:32),
LowerIdent(205:2-205:6),OpAssign(205:7-205:8),Int(205:9-205:10),Newline(1:1-1:1),
LowerIdent(206:2-206:6),OpEquals(206:7-206:9),LowerIdent(206:10-206:13),Newline(1:1-1:1),
CloseCurly(207:1-207:2),EndOfFile(207:2-207:2),
~~~
# PARSE
~~~clojure
(file (1:2-207:2)
	(app (2:1-2:57)
		(provides (2:6-2:12) (exposed_item (lower_ident "main!")))
		(record_field (2:15-2:57)
			"pf"
			(string (2:28-2:55) (string_part (2:29-2:54) "../basic-cli/platform.roc")))
		(packages (2:13-2:57)
			(record_field (2:15-2:57)
				"pf"
				(string (2:28-2:55) (string_part (2:29-2:54) "../basic-cli/platform.roc")))))
	(statements
		(import (4:1-4:42)
			".Stdout"
			(qualifier "pf")
			(exposing
				(exposed_item (lower_ident "line!"))
				(exposed_item (lower_ident "write!"))))
		(import (6:1-12:4)
			".StdoutMultiline"
			(qualifier "pf")
			(exposing
				(exposed_item (lower_ident "line!"))
				(exposed_item (lower_ident "write!"))))
		(import (14:1-14:82)
			".Something"
			(qualifier "pkg")
			(exposing
				(exposed_item (lower_ident "func" "function"))
				(exposed_item (upper_ident "Type" "ValueCategory"))
				(exposed_item (upper_ident_star "Custom"))))
		(import (16:1-16:27) "BadName" (alias "GoodName"))
		(import (17:1-20:20) "BadNameMultiline" (alias "GoodNameMultiline"))
		(type_decl (22:1-23:6)
			(header (22:1-22:10)
				"Map"
				(args
					(ty_var (22:5-22:6) "a")
					(ty_var (22:8-22:9) "b")))
			(fn (22:13-22:41)
				(apply (22:13-22:20)
					(ty "List")
					(ty_var (22:18-22:19) "a"))
				(fn (22:23-22:29)
					(ty_var (22:23-22:24) "a")
					(ty_var (22:28-22:29) "b"))
				(apply (22:34-22:41)
					(ty "List")
					(ty_var (22:39-22:40) "b"))))
		(type_decl (23:1-36:4)
			(header (23:1-26:2)
				"MapML"
				(args
					(ty_var (24:2-24:3) "a")
					(ty_var (25:2-25:3) "b")))
			(fn (28:3-34:5)
				(apply (28:3-30:4)
					(ty "List")
					(ty_var (29:4-29:5) "a"))
				(fn (31:4-31:10)
					(ty_var (31:4-31:5) "a")
					(ty_var (31:9-31:10) "b"))
				(apply (32:4-34:5)
					(ty "List")
					(ty_var (33:5-33:6) "b"))))
		(type_decl (36:1-38:13)
			(header (36:1-36:4) "Foo" (args))
			(tuple (36:7-36:17)
				(ty "Bar")
				(ty "Baz")))
		(type_decl (38:1-43:5)
			(header (38:1-38:13) "FooMultiline" (args))
			(tuple (38:16-41:2)
				(ty "Bar")
				(ty "Baz")))
		(type_decl (43:1-44:7)
			(header (43:1-43:8)
				"Some"
				(args (ty_var (43:6-43:7) "a")))
			(record (43:11-43:43)
				(anno_record_field (43:13-43:25)
					"foo"
					(apply (43:19-43:24)
						(ty "Ok")
						(ty_var (43:22-43:23) "a")))
				(anno_record_field (43:26-43:43) "bar" (ty "Something"))))
		(type_decl (44:1-49:14)
			(header (44:1-44:10)
				"SomeMl"
				(args (ty_var (44:8-44:9) "a")))
			(record (44:13-47:2)
				(anno_record_field (45:2-45:14)
					"foo"
					(apply (45:8-45:13)
						(ty "Ok")
						(ty_var (45:11-45:12) "a")))
				(anno_record_field (46:2-46:18) "bar" (ty "Something"))))
		(type_decl (49:1-56:6)
			(header (49:1-49:17)
				"SomeMultiline"
				(args (ty_var (49:15-49:16) "a")))
			(record (49:20-54:2)
				(anno_record_field (50:2-52:10)
					"foo"
					(apply (52:4-52:9)
						(ty "Ok")
						(ty_var (52:7-52:8) "a")))
				(anno_record_field (53:2-53:18) "bar" (ty "Something"))))
		(type_decl (56:1-58:15)
			(header (56:1-56:9)
				"Maybe"
				(args (ty_var (56:7-56:8) "a")))
			(tag_union (56:12-56:27)
				(tags
					(apply (56:13-56:20)
						(ty "Some")
						(ty_var (56:18-56:19) "a"))
					(ty "None"))))
		(type_decl (58:1-63:9)
			(header (58:1-58:18)
				"MaybeMultiline"
				(args (ty_var (58:16-58:17) "a")))
			(tag_union (58:21-61:2)
				(tags
					(apply (59:2-59:9)
						(ty "Some")
						(ty_var (59:7-59:8) "a"))
					(ty "None"))))
		(type_decl (63:1-65:16)
			(header (63:1-63:12)
				"SomeFunc"
				(args (ty_var (63:10-63:11) "a")))
			(fn (63:15-63:38)
				(apply (63:15-63:23)
					(ty "Maybe")
					(ty_var (63:21-63:22) "a"))
				(ty_var (63:25-63:26) "a")
				(apply (63:30-63:38)
					(ty "Maybe")
					(ty_var (63:36-63:37) "a"))))
		(decl (65:1-67:8)
			(ident (65:1-65:16) "add_one_oneline")
			(lambda (65:19-67:8)
				(args (ident (65:20-65:23) "num"))
				(if_then_else (65:25-67:8)
					(ident (65:28-65:31) "" "num")
					(int (65:32-65:33) "2")
					(int (65:39-65:40) "5"))))
		(type_anno (67:1-68:8)
			"add_one"
			(fn (67:11-67:21)
				(ty "U64")
				(ty "U64")))
		(decl (68:1-78:2)
			(ident (68:1-68:8) "add_one")
			(lambda (68:11-78:2)
				(args (ident (68:12-68:15) "num"))
				(block (68:17-78:2)
					(statements
						(decl (69:2-69:11)
							(ident (69:2-69:7) "other")
							(int (69:10-69:11) "1"))
						(if_then_else (70:2-78:2)
							(ident (70:5-70:8) "" "num")
							(block (70:9-74:3)
								(statements
									(dbg
										(apply (72:4-72:15)
											(ident (72:4-72:13) "" "some_func")))
									(int (73:3-73:4) "0")))
							(block (74:9-77:3)
								(statements
									(dbg (int (75:7-75:10) "123"))
									(ident (76:3-76:8) "" "other"))))))))
		(decl (80:1-140:7)
			(ident (80:1-80:11) "match_time")
			(lambda (80:14-140:7)
				(args
					(ident (81:2-81:3) "a")
					(ident (82:2-82:3) "b"))
				(match
					(ident (84:8-84:9) "" "a")
					(branches
						(branch (85:3-89:7)
							(alternatives
								(tag (85:3-85:7) "Blue")
								(tag (85:10-85:15) "Green")
								(tag (85:18-85:21) "Red"))
							(block (85:25-88:4)
								(statements
									(decl (86:4-86:10)
										(ident (86:4-86:5) "x")
										(int (86:8-86:10) "12"))
									(ident (87:4-87:5) "" "x"))))
						(branch (89:3-97:8)
							(alternatives
								(tag (89:3-89:7) "Blue")
								(tag (91:4-91:9) "Green")
								(tag (92:5-92:8) "Red"))
							(block (93:7-96:5)
								(statements
									(decl (94:5-94:11)
										(ident (94:5-94:6) "x")
										(int (94:9-94:11) "12"))
									(ident (95:5-95:6) "" "x"))))
						(branch (97:3-99:4)
							(ident (97:3-97:8) "lower")
							(int (98:7-98:8) "1"))
						(branch (99:3-101:4)
							(string (99:3-99:4) """)
							(int (100:4-100:7) "100"))
						(branch (101:3-102:4)
							(alternatives
								(string (101:3-101:4) """)
								(string (101:11-101:12) """))
							(int (101:20-101:23) "200"))
						(branch (102:3-108:4)
							(list (102:3-102:24)
								(number (102:4-102:5) "1")
								(number (102:7-102:8) "2")
								(number (102:10-102:11) "3")
								(list_rest (102:13-102:23) "rest"))
							(int (104:5-104:8) "123"))
						(branch (108:3-109:4)
							(list (108:3-108:28)
								(number (108:4-108:5) "1")
								(alternatives
									(number (108:7-108:8) "2")
									(number (108:11-108:12) "5"))
								(number (108:14-108:15) "3")
								(list_rest (108:17-108:27) "rest"))
							(int (108:32-108:35) "123"))
						(branch (109:3-117:7)
							(list (109:3-116:4)
								(number (110:4-110:5) "1")
								(alternatives
									(number (111:4-111:5) "2")
									(number (111:8-111:9) "5"))
								(number (112:4-112:5) "3")
								(list_rest (113:4-115:10) "rest"))
							(int (116:8-116:11) "123"))
						(branch (117:3-118:7)
							(number (117:3-117:7) "3.14")
							(int (117:11-117:14) "314"))
						(branch (118:3-119:4)
							(alternatives
								(number (118:3-118:7) "3.14")
								(number (118:10-118:14) "6.28"))
							(int (118:18-118:21) "314"))
						(branch (119:3-120:4)
							(tuple (119:3-119:12)
								(number (119:4-119:5) "1")
								(number (119:7-119:8) "2")
								(number (119:10-119:11) "3"))
							(int (119:16-119:19) "123"))
						(branch (120:3-121:4)
							(tuple (120:3-120:16)
								(number (120:4-120:5) "1")
								(alternatives
									(number (120:7-120:8) "2")
									(number (120:11-120:12) "5"))
								(number (120:14-120:15) "3"))
							(int (120:20-120:23) "123"))
						(branch (121:3-122:4)
							(record (121:3-121:29)
								(field (121:5-121:12)
									"foo"
									(number (121:10-121:11) "1"))
								(field (121:13-121:20)
									"bar"
									(number (121:18-121:19) "2"))
								(field (121:21-121:29) "rest" "rest"))
							(local_dispatch (121:33-122:4)
								(int (121:33-121:35) "12")
								(apply (121:35-121:44)
									(ident (121:37-121:40) "" "add")
									(int (121:41-121:43) "34"))))
						(branch (122:3-130:4)
							(record (122:3-129:4)
								(field (123:4-125:8)
									"foo"
									(number (125:6-125:7) "1"))
								(field (126:4-126:11)
									"bar"
									(number (126:9-126:10) "2"))
								(field (127:4-128:10) "rest" "rest"))
							(int (129:8-129:10) "12"))
						(branch (130:3-131:4)
							(record (130:3-130:25)
								(field (130:5-130:12)
									"foo"
									(number (130:10-130:11) "1"))
								(field (130:13-130:25)
									"bar"
									(alternatives
										(number (130:18-130:19) "2")
										(number (130:22-130:23) "7"))))
							(int (130:29-130:31) "12"))
						(branch (131:3-135:5)
							(record (131:3-134:4)
								(field (132:4-132:11)
									"foo"
									(number (132:9-132:10) "1"))
								(field (133:4-133:15)
									"bar"
									(alternatives
										(number (133:9-133:10) "2")
										(number (133:13-133:14) "7"))))
							(int (134:8-134:10) "12"))
						(branch (135:3-136:5)
							(tag (135:3-135:10)
								"Ok"
								(number (135:6-135:9) "123"))
							(int (135:14-135:17) "123"))
						(branch (136:3-137:10)
							(tag (136:3-136:17)
								"Ok"
								(tag (136:6-136:16)
									"Some"
									(ident (136:11-136:15) "dude")))
							(ident (136:21-136:25) "" "dude"))
						(branch (137:3-138:3)
							(tag (137:3-137:34)
								"TwoArgs"
								(string (137:11-137:12) """)
								(tag (137:20-137:33)
									"Some"
									(string (137:25-137:26) """)))
							(int (137:38-137:42) "1000"))))))
		(expect (140:1-143:6)
			(binop (141:2-143:6)
				"=="
				(ident (141:2-141:6) "" "blah")
				(int (141:10-141:11) "1")))
		(type_anno (143:1-144:6)
			"main!"
			(fn (143:9-143:38)
				(apply (143:9-143:21)
					(ty "List")
					(ty "String"))
				(apply (143:25-143:38)
					(ty "Result")
					(record (143:32-143:34))
					(_))))
		(decl (144:1-196:2)
			(ident (144:1-144:6) "main!")
			(lambda (144:9-196:2)
				(args (underscore))
				(block (144:13-196:2)
					(statements
						(decl (145:2-145:17)
							(ident (145:2-145:7) "world")
							(string (145:10-145:17) (string_part (145:11-145:16) "World")))
						(var (146:2-147:8)
							(name "number")
							(int (146:15-146:18) "123"))
						(expect (147:2-148:5)
							(binop (147:9-148:5)
								"=="
								(ident (147:9-147:13) "" "blah")
								(int (147:17-147:18) "1")))
						(decl (148:2-148:12)
							(ident (148:2-148:5) "tag")
							(tag (148:8-148:12) "Blue"))
						(return (149:2-154:5)
							(ident (150:3-150:6) "" "tag"))
						(ellipsis)
						(apply (155:2-157:3)
							(ident (155:2-155:12) "" "match_time")
							(ellipsis))
						(apply (158:2-161:3)
							(ident (158:2-158:11) "" "some_func")
							(dbg (int (160:4-160:6) "42")))
						(crash (162:2-163:49)
							(string (163:3-163:17) (string_part (163:4-163:16) "Unreachable!")))
						(decl (164:2-164:31)
							(ident (164:2-164:18) "tag_with_payload")
							(apply (164:21-164:31)
								(tag (164:21-164:23) "Ok")
								(ident (164:24-164:30) "" "number")))
						(decl (165:2-165:34)
							(ident (165:2-165:14) "interpolated")
							(string (165:17-165:34)
								(string_part (165:18-165:25) "Hello, ")
								(ident (165:27-165:32) "" "world")
								(string_part (165:33-165:33) "")))
						(decl (166:2-173:3)
							(ident (166:2-166:6) "list")
							(list (166:9-173:3)
								(apply (167:3-170:4)
									(ident (167:3-167:10) "" "add_one")
									(dbg (ident (169:5-169:11) "" "number")))
								(int (171:3-171:6) "456")
								(int (172:3-172:6) "789")))
						(for
							(ident (174:6-174:7) "n")
							(ident (174:11-174:15) "" "list")
							(block (174:16-177:3)
								(statements
									(apply (175:3-175:43)
										(ident (175:3-175:15) "Stdout" ".line!")
										(string (175:16-175:42)
											(string_part (175:17-175:24) "Adding ")
											(ident (175:26-175:27) "" "n")
											(string_part (175:28-175:32) " to ")
											(ident (175:34-175:40) "" "number")
											(string_part (175:41-175:41) "")))
									(decl (176:3-177:3)
										(ident (176:3-176:9) "number")
										(binop (176:12-177:3)
											"+"
											(ident (176:12-176:18) "" "number")
											(ident (176:21-176:22) "" "n"))))))
						(decl (178:2-178:71)
							(ident (178:2-178:8) "record")
							(record (178:11-178:71)
								(field "foo" (int (178:18-178:21) "123"))
								(field
									"bar"
									(string (178:28-178:35) (string_part (178:29-178:34) "Hello")))
								(field
									"baz"
									(ident (178:42-178:45) "" "tag"))
								(field
									"qux"
									(apply (178:52-178:61)
										(tag (178:52-178:54) "Ok")
										(ident (178:55-178:60) "" "world")))
								(field "punned")))
						(decl (179:2-179:68)
							(ident (179:2-179:7) "tuple")
							(tuple (179:10-179:68)
								(int (179:11-179:14) "123")
								(string (179:16-179:23) (string_part (179:17-179:22) "World"))
								(ident (179:25-179:28) "" "tag")
								(apply (179:30-179:39)
									(tag (179:30-179:32) "Ok")
									(ident (179:33-179:38) "" "world"))
								(tuple (179:41-179:56)
									(ident (179:42-179:48) "" "nested")
									(ident (179:50-179:55) "" "tuple"))
								(list (179:58-179:67)
									(int (179:59-179:60) "1")
									(int (179:62-179:63) "2")
									(int (179:65-179:66) "3"))))
						(decl (180:2-187:3)
							(ident (180:2-180:17) "multiline_tuple")
							(tuple (180:20-187:3)
								(int (181:3-181:6) "123")
								(string (182:3-182:10) (string_part (182:4-182:9) "World"))
								(ident (183:3-183:7) "" "tag1")
								(apply (184:3-184:12)
									(tag (184:3-184:5) "Ok")
									(ident (184:6-184:11) "" "world"))
								(tuple (185:3-185:18)
									(ident (185:4-185:10) "" "nested")
									(ident (185:12-185:17) "" "tuple"))
								(list (186:3-186:12)
									(int (186:4-186:5) "1")
									(int (186:7-186:8) "2")
									(int (186:10-186:11) "3"))))
						(decl (188:2-189:23)
							(ident (188:2-188:15) "bin_op_result")
							(binop (188:18-189:23)
								"or"
								(binop (188:18-188:74)
									"or"
									(binop (188:18-188:43)
										">"
										(binop (188:18-188:34)
											"??"
											(apply (188:18-188:26)
												(tag (188:18-188:21) "Err")
												(ident (188:22-188:25) "" "foo"))
											(int (188:30-188:32) "12"))
										(binop (188:35-188:43)
											"*"
											(int (188:35-188:36) "5")
											(int (188:39-188:40) "5")))
									(binop (188:44-188:74)
										"and"
										(binop (188:44-188:58)
											"<"
											(binop (188:44-188:52)
												"+"
												(int (188:44-188:46) "13")
												(int (188:49-188:50) "2"))
											(int (188:53-188:54) "5"))
										(binop (188:59-188:74)
											">="
											(binop (188:59-188:68)
												"-"
												(int (188:59-188:61) "10")
												(int (188:64-188:65) "1"))
											(int (188:69-188:71) "16"))))
								(binop (188:75-189:23)
									"<="
									(int (188:75-188:77) "12")
									(binop (188:81-189:23)
										"/"
										(int (188:81-188:82) "3")
										(int (188:85-188:86) "5")))))
						(decl (189:2-190:8)
							(ident (189:2-189:23) "static_dispatch_style")
							(field_access (189:26-190:8)
								(binop (189:26-190:8)
									" This is a module comment!"
									(field_access (189:26-189:110)
										(binop (189:26-189:110)
											" This is a module comment!"
											(field_access (189:26-189:94)
												(binop (189:26-189:94)
													" This is a module comment!"
													(suffix_single_question (189:26-189:40)
														(apply (189:26-189:39)
															(ident (189:26-189:33) "" "some_fn")
															(ident (189:34-189:38) "" "arg1")))
													(suffix_single_question (189:40-189:66)
														(apply (189:40-189:65)
															(ident (189:40-189:63) "" ".static_dispatch_method")))))
											(suffix_single_question (189:66-189:97)
												(apply (189:66-189:96)
													(ident (189:66-189:94) "" ".next_static_dispatch_method")))))
									(suffix_single_question (189:97-189:111)
										(ident (189:97-189:110) "" ".record_field")))))
						(suffix_single_question (190:2-190:29)
							(apply (190:2-190:28)
								(ident (190:2-190:14) "Stdout" ".line!")
								(ident (190:15-190:27) "" "interpolated")))
						(apply (191:2-195:3)
							(ident (191:2-191:14) "Stdout" ".line!")
							(string (192:3-194:18)
								(string_part (192:4-192:14) "How about ")
								(apply (193:4-193:21)
									(ident (193:4-193:13) "Num" ".toStr")
									(ident (193:14-193:20) "" "number"))
								(string_part (194:4-194:17) " as a string?")))))))
		(type_anno (198:1-199:6) "empty" (record (198:9-198:11)))
		(decl (199:1-199:11)
			(ident (199:1-199:6) "empty")
			(record (199:9-199:11)))
		(type_anno (201:1-203:7)
			"tuple"
			(apply (201:9-201:25)
				(ty "Value")
				(tuple (201:15-201:24)
					(ty_var (201:16-201:17) "a")
					(ty_var (201:19-201:20) "b")
					(ty_var (201:22-201:23) "c"))))
		(expect (203:1-207:2)
			(block (203:8-207:2)
				(statements
					(decl (204:2-204:9)
						(ident (204:2-204:5) "foo")
						(int (204:8-204:9) "1"))
					(decl (205:2-205:10)
						(ident (205:2-205:6) "blah")
						(int (205:9-205:10) "1"))
					(binop (206:2-207:2)
						"=="
						(ident (206:2-206:6) "" "blah")
						(ident (206:10-206:13) "" "foo")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (65:1-65:16)
				(pid 172)
				(ident "add_one_oneline")))
		(def_expr
			(e_lambda (65:19-67:8)
				(args
					(p_assign (65:20-65:23)
						(pid 173)
						(ident "num")))
				(e_runtime_error (1:1-1:1) "not_implemented"))))
	(d_let
		(def_pattern
			(p_assign (68:1-68:8)
				(pid 182)
				(ident "add_one")))
		(def_expr
			(e_lambda (68:11-78:2)
				(args
					(p_assign (68:12-68:15)
						(pid 183)
						(ident "num")))
				(e_block (68:17-78:2)
					(s_let (69:2-69:11)
						(p_assign (69:2-69:7)
							(pid 184)
							(ident "other"))
						(e_int (69:10-69:11)
							(int_var 186)
							(precision_var 185)
							(literal "1")
							(value "TODO")
							(bound "u8")))
					(e_runtime_error (1:1-1:1) "not_implemented"))))
		(annotation (68:1-68:8)
			(signature 198)
			(declared_type
				(fn (67:11-67:21)
					(ty (67:11-67:14) "U64")
					(ty (67:18-67:21) "U64")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (80:1-80:11)
				(pid 201)
				(ident "match_time")))
		(def_expr
			(e_lambda (80:14-140:7)
				(args
					(p_assign (81:2-81:3)
						(pid 202)
						(ident "a"))
					(p_assign (82:2-82:3)
						(pid 203)
						(ident "b")))
				(e_runtime_error (1:1-1:1) "not_implemented"))))
	(d_let
		(def_pattern
			(p_assign (144:1-144:6)
				(pid 218)
				(ident "main!")))
		(def_expr
			(e_lambda (144:9-196:2)
				(args (p_underscore (144:10-144:11) (pid 219)))
				(e_block (144:13-196:2)
					(s_let (145:2-145:17)
						(p_assign (145:2-145:7)
							(pid 220)
							(ident "world"))
						(e_string (145:10-145:17) (e_literal (145:11-145:16) "World")))
					(s_var (146:2-147:8)
						(pid 227)
						(p_assign (146:2-147:8)
							(pid 227)
							(ident "number"))
						(e_int (146:15-146:18)
							(int_var 225)
							(precision_var 224)
							(literal "123")
							(value "TODO")
							(bound "u8")))
					(s_let (148:2-148:12)
						(p_assign (148:2-148:5)
							(pid 231)
							(ident "tag"))
						(e_tag (148:8-148:12)
							(ext_var 0)
							(name "Blue")
							(args "TODO")))
					(s_expr (154:2-155:12) (e_runtime_error (154:2-154:5) "not_implemented"))
					(s_expr (155:2-158:11)
						(e_call (155:2-157:3)
							(e_lookup (155:2-155:12) (pid 201))
							(e_runtime_error (156:3-156:6) "not_implemented")))
					(s_expr (158:2-162:7)
						(e_call (158:2-161:3)
							(e_runtime_error (158:2-158:11) "ident_not_in_scope")
							(e_runtime_error (1:1-1:1) "not_implemented")))
					(s_let (164:2-164:31)
						(p_assign (164:2-164:18)
							(pid 253)
							(ident "tag_with_payload"))
						(e_call (164:21-164:31)
							(e_tag (164:21-164:23)
								(ext_var 0)
								(name "Ok")
								(args "TODO"))
							(e_lookup (164:24-164:30) (pid 227))))
					(s_let (165:2-165:34)
						(p_assign (165:2-165:14)
							(pid 259)
							(ident "interpolated"))
						(e_string (165:17-165:34)
							(e_literal (165:18-165:25) "Hello, ")
							(e_lookup (165:27-165:32) (pid 220))
							(e_literal (165:33-165:33) "")))
					(s_let (166:2-173:3)
						(p_assign (166:2-166:6)
							(pid 265)
							(ident "list"))
						(e_list (166:9-173:3)
							(elem_var 276)
							(elems
								(e_call (167:3-170:4)
									(e_lookup (167:3-167:10) (pid 182))
									(e_runtime_error (1:1-1:1) "not_implemented"))
								(e_int (171:3-171:6)
									(int_var 271)
									(precision_var 270)
									(literal "456")
									(value "TODO")
									(bound "u8"))
								(e_int (172:3-172:6)
									(int_var 274)
									(precision_var 273)
									(literal "789")
									(value "TODO")
									(bound "u8")))))
					(s_let (178:2-178:71)
						(p_assign (178:2-178:8)
							(pid 281)
							(ident "record"))
						(e_runtime_error (1:1-1:1) "not_implemented"))
					(s_let (179:2-179:68)
						(p_assign (179:2-179:7)
							(pid 285)
							(ident "tuple"))
						(e_tuple (179:10-179:68)
							(tuple_var "#312")
							(elems
								(e_int (179:11-179:14)
									(int_var 287)
									(precision_var 286)
									(literal "123")
									(value "TODO")
									(bound "u8"))
								(e_string (179:16-179:23) (e_literal (179:17-179:22) "World"))
								(e_lookup (179:25-179:28) (pid 231))
								(e_call (179:30-179:39)
									(e_tag (179:30-179:32)
										(ext_var 0)
										(name "Ok")
										(args "TODO"))
									(e_lookup (179:33-179:38) (pid 220)))
								(e_tuple (179:41-179:56)
									(tuple_var "#299")
									(elems
										(e_runtime_error (179:42-179:48) "ident_not_in_scope")
										(e_lookup (179:50-179:55) (pid 285))))
								(e_list (179:58-179:67)
									(elem_var 310)
									(elems
										(e_int (179:59-179:60)
											(int_var 302)
											(precision_var 301)
											(literal "1")
											(value "TODO")
											(bound "u8"))
										(e_int (179:62-179:63)
											(int_var 305)
											(precision_var 304)
											(literal "2")
											(value "TODO")
											(bound "u8"))
										(e_int (179:65-179:66)
											(int_var 308)
											(precision_var 307)
											(literal "3")
											(value "TODO")
											(bound "u8")))))))
					(s_let (180:2-187:3)
						(p_assign (180:2-180:17)
							(pid 315)
							(ident "multiline_tuple"))
						(e_tuple (180:20-187:3)
							(tuple_var "#343")
							(elems
								(e_int (181:3-181:6)
									(int_var 317)
									(precision_var 316)
									(literal "123")
									(value "TODO")
									(bound "u8"))
								(e_string (182:3-182:10) (e_literal (182:4-182:9) "World"))
								(e_runtime_error (183:3-183:7) "ident_not_in_scope")
								(e_call (184:3-184:12)
									(e_tag (184:3-184:5)
										(ext_var 0)
										(name "Ok")
										(args "TODO"))
									(e_lookup (184:6-184:11) (pid 220)))
								(e_tuple (185:3-185:18)
									(tuple_var "#330")
									(elems
										(e_runtime_error (185:4-185:10) "ident_not_in_scope")
										(e_lookup (185:12-185:17) (pid 285))))
								(e_list (186:3-186:12)
									(elem_var 341)
									(elems
										(e_int (186:4-186:5)
											(int_var 333)
											(precision_var 332)
											(literal "1")
											(value "TODO")
											(bound "u8"))
										(e_int (186:7-186:8)
											(int_var 336)
											(precision_var 335)
											(literal "2")
											(value "TODO")
											(bound "u8"))
										(e_int (186:10-186:11)
											(int_var 339)
											(precision_var 338)
											(literal "3")
											(value "TODO")
											(bound "u8")))))))
					(s_let (188:2-189:23)
						(p_assign (188:2-188:15)
							(pid 346)
							(ident "bin_op_result"))
						(e_runtime_error (188:18-189:23) "not_implemented"))
					(s_let (189:2-190:8)
						(p_assign (189:2-189:23)
							(pid 410)
							(ident "static_dispatch_style"))
						(e_dot_access (189:26-190:8)
							(e_dot_access (189:26-189:110)
								(e_dot_access (189:26-189:94)
									(e_runtime_error (1:1-1:1) "not_implemented")
									"unknown")
								"unknown")
							"unknown"))
					(s_expr (190:2-191:8) (e_runtime_error (1:1-1:1) "not_implemented"))
					(e_call (191:2-195:3)
						(e_runtime_error (191:2-191:14) "ident_not_in_scope")
						(e_string (192:3-194:18)
							(e_literal (192:4-192:14) "How about ")
							(e_call (193:4-193:21)
								(e_runtime_error (193:4-193:13) "ident_not_in_scope")
								(e_lookup (193:14-193:20) (pid 227)))
							(e_literal (194:4-194:17) " as a string?"))))))
		(annotation (144:1-144:6)
			(signature 442)
			(declared_type
				(fn (143:9-143:38)
					(apply (143:9-143:21)
						"List"
						(ty (143:14-143:20) "String"))
					(apply (143:25-143:38)
						"Result"
						(record (143:32-143:34))
						(underscore (143:36-143:37)))
					"false"))))
	(d_let
		(def_pattern
			(p_assign (199:1-199:6)
				(pid 446)
				(ident "empty")))
		(def_expr (e_runtime_error (1:1-1:1) "not_implemented"))
		(annotation (199:1-199:6)
			(signature 450)
			(declared_type (record (198:9-198:11)))))
	(s_type_decl (22:1-23:6)
		(type_header (22:1-22:10)
			"Map"
			(args
				(ty_var (22:5-22:6) "a")
				(ty_var (22:8-22:9) "b")))
		(fn (22:13-22:41)
			(apply (22:13-22:20)
				"List"
				(ty_var (22:18-22:19) "a"))
			(parens (22:22-22:30)
				(fn (22:23-22:29)
					(ty_var (22:23-22:24) "a")
					(ty_var (22:28-22:29) "b")
					"false"))
			(apply (22:34-22:41)
				"List"
				(ty_var (22:39-22:40) "b"))
			"false"))
	(s_type_decl (23:1-36:4)
		(type_header (23:1-26:2)
			"MapML"
			(args
				(ty_var (24:2-24:3) "a")
				(ty_var (25:2-25:3) "b")))
		(fn (28:3-34:5)
			(apply (28:3-30:4)
				"List"
				(ty_var (29:4-29:5) "a"))
			(parens (31:3-31:11)
				(fn (31:4-31:10)
					(ty_var (31:4-31:5) "a")
					(ty_var (31:9-31:10) "b")
					"false"))
			(apply (32:4-34:5)
				"List"
				(ty_var (33:5-33:6) "b"))
			"false"))
	(s_type_decl (36:1-38:13)
		(type_header (36:1-36:4) "Foo")
		(tuple (36:7-36:17)
			(ty (36:8-36:11) "Bar")
			(ty (36:13-36:16) "Baz")))
	(s_type_decl (38:1-43:5)
		(type_header (38:1-38:13) "FooMultiline")
		(tuple (38:16-41:2)
			(ty (39:2-39:5) "Bar")
			(ty (40:2-40:5) "Baz")))
	(s_type_decl (43:1-44:7)
		(type_header (43:1-43:8)
			"Some"
			(args (ty_var (43:6-43:7) "a")))
		(record (43:11-43:43)
			(record_field
				"foo"
				(apply (43:19-43:24)
					"Ok"
					(ty_var (43:22-43:23) "a")))
			(record_field "bar" (ty (43:32-43:41) "Something"))))
	(s_type_decl (44:1-49:14)
		(type_header (44:1-44:10)
			"SomeMl"
			(args (ty_var (44:8-44:9) "a")))
		(record (44:13-47:2)
			(record_field
				"foo"
				(apply (45:8-45:13)
					"Ok"
					(ty_var (45:11-45:12) "a")))
			(record_field "bar" (ty (46:8-46:17) "Something"))))
	(s_type_decl (49:1-56:6)
		(type_header (49:1-49:17)
			"SomeMultiline"
			(args (ty_var (49:15-49:16) "a")))
		(record (49:20-54:2)
			(record_field
				"foo"
				(apply (52:4-52:9)
					"Ok"
					(ty_var (52:7-52:8) "a")))
			(record_field "bar" (ty (53:8-53:17) "Something"))))
	(s_type_decl (56:1-58:15)
		(type_header (56:1-56:9)
			"Maybe"
			(args (ty_var (56:7-56:8) "a")))
		(tag_union (56:12-56:27)
			(apply (56:13-56:20)
				"Some"
				(ty_var (56:18-56:19) "a"))
			(ty (56:22-56:26) "None")))
	(s_type_decl (58:1-63:9)
		(type_header (58:1-58:18)
			"MaybeMultiline"
			(args (ty_var (58:16-58:17) "a")))
		(tag_union (58:21-61:2)
			(apply (59:2-59:9)
				"Some"
				(ty_var (59:7-59:8) "a"))
			(ty (60:2-60:6) "None")))
	(s_type_decl (63:1-65:16)
		(type_header (63:1-63:12)
			"SomeFunc"
			(args (ty_var (63:10-63:11) "a")))
		(fn (63:15-63:38)
			(apply (63:15-63:23)
				"Maybe"
				(ty_var (63:21-63:22) "a"))
			(ty_var (63:25-63:26) "a")
			(apply (63:30-63:38)
				"Maybe"
				(ty_var (63:36-63:37) "a"))
			"false")))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "add_one_oneline" 178 (type "*"))
		(def "add_one" 200 (type "*"))
		(def "match_time" 209 (type "*"))
		(def "main!" 444 (type "*"))
		(def "empty" 452 (type "Error")))
	(expressions
		(expr (65:19-67:8) 176 (type "*"))
		(expr (68:11-78:2) 193 (type "*"))
		(expr (80:14-140:7) 206 (type "*"))
		(expr (144:9-196:2) 438 (type "*"))
		(expr (199:9-199:11) 448 (type "Error"))))
~~~