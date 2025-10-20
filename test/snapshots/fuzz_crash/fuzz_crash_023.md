# META
~~~ini
description=fuzz crash
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
	record = { foo: 123, bar: "Hello", ;az: tag, qux: Ok(world), punned }
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
# EXPECTED
PARSE ERROR - fuzz_crash_023.md:178:37:178:38
PARSE ERROR - fuzz_crash_023.md:178:38:178:40
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_023.md:178:40:178:41
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_023.md:178:45:178:46
PARSE ERROR - fuzz_crash_023.md:178:52:178:54
UNDECLARED TYPE - fuzz_crash_023.md:36:8:36:11
UNDECLARED TYPE - fuzz_crash_023.md:36:13:36:16
UNDECLARED TYPE - fuzz_crash_023.md:39:2:39:5
UNDECLARED TYPE - fuzz_crash_023.md:40:2:40:5
UNDECLARED TYPE - fuzz_crash_023.md:43:19:43:21
UNDECLARED TYPE - fuzz_crash_023.md:43:32:43:41
UNDECLARED TYPE - fuzz_crash_023.md:45:8:45:10
UNDECLARED TYPE - fuzz_crash_023.md:46:8:46:17
UNDECLARED TYPE - fuzz_crash_023.md:52:4:52:6
UNDECLARED TYPE - fuzz_crash_023.md:53:8:53:17
MODULE NOT FOUND - fuzz_crash_023.md:4:1:4:42
NOT IMPLEMENTED - :0:0:0:0
MODULE NOT FOUND - fuzz_crash_023.md:6:1:12:4
MODULE NOT FOUND - fuzz_crash_023.md:14:1:14:82
MODULE NOT FOUND - fuzz_crash_023.md:16:1:16:27
MODULE NOT FOUND - fuzz_crash_023.md:17:1:20:20
UNDEFINED VARIABLE - fuzz_crash_023.md:72:4:72:13
UNUSED VARIABLE - fuzz_crash_023.md:97:3:97:8
UNUSED VARIABLE - fuzz_crash_023.md:1:1:1:1
NOT IMPLEMENTED - :0:0:0:0
UNUSED VARIABLE - fuzz_crash_023.md:1:1:1:1
NOT IMPLEMENTED - :0:0:0:0
UNUSED VARIABLE - fuzz_crash_023.md:1:1:1:1
NOT IMPLEMENTED - :0:0:0:0
NOT IMPLEMENTED - :0:0:0:0
UNUSED VARIABLE - fuzz_crash_023.md:121:21:121:27
UNUSED VARIABLE - fuzz_crash_023.md:127:4:128:9
NOT IMPLEMENTED - :0:0:0:0
NOT IMPLEMENTED - :0:0:0:0
UNUSED VARIABLE - fuzz_crash_023.md:82:2:82:3
UNDEFINED VARIABLE - fuzz_crash_023.md:141:2:141:6
UNDECLARED TYPE - fuzz_crash_023.md:143:14:143:20
UNDEFINED VARIABLE - fuzz_crash_023.md:147:9:147:13
UNDEFINED VARIABLE - fuzz_crash_023.md:158:2:158:11
NOT IMPLEMENTED - :0:0:0:0
UNRECOGNIZED SYNTAX - fuzz_crash_023.md:178:38:178:40
UNRECOGNIZED SYNTAX - fuzz_crash_023.md:178:40:178:41
UNRECOGNIZED SYNTAX - fuzz_crash_023.md:178:45:178:46
MALFORMED TYPE - fuzz_crash_023.md:178:52:178:71
UNDEFINED VARIABLE - fuzz_crash_023.md:179:42:179:48
UNDEFINED VARIABLE - fuzz_crash_023.md:183:3:183:7
UNDEFINED VARIABLE - fuzz_crash_023.md:185:4:185:10
UNDEFINED VARIABLE - fuzz_crash_023.md:188:22:188:25
NOT IMPLEMENTED - :0:0:0:0
NOT IMPLEMENTED - :0:0:0:0
UNDEFINED VARIABLE - fuzz_crash_023.md:191:2:191:14
UNDEFINED VARIABLE - fuzz_crash_023.md:193:4:193:13
UNUSED VARIABLE - fuzz_crash_023.md:164:2:164:18
UNUSED VARIABLE - fuzz_crash_023.md:165:2:165:14
UNUSED VARIABLE - fuzz_crash_023.md:166:2:166:6
UNUSED VARIABLE - fuzz_crash_023.md:178:2:178:8
UNUSED VARIABLE - fuzz_crash_023.md:180:2:180:17
UNUSED VARIABLE - fuzz_crash_023.md:188:2:188:15
UNUSED VARIABLE - fuzz_crash_023.md:189:2:189:23
UNDECLARED TYPE - fuzz_crash_023.md:201:9:201:14
INVALID IF CONDITION - fuzz_crash_023.md:70:5:70:5
INCOMPATIBLE MATCH PATTERNS - fuzz_crash_023.md:84:2:84:2
TYPE MISMATCH - fuzz_crash_023.md:155:2:157:3
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_record_field_name`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_023.md:178:37:178:38:**
```roc
	record = { foo: 123, bar: "Hello", ;az: tag, qux: Ok(world), punned }
```
	                                   ^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_023.md:178:38:178:40:**
```roc
	record = { foo: 123, bar: "Hello", ;az: tag, qux: Ok(world), punned }
```
	                                    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_023.md:178:40:178:41:**
```roc
	record = { foo: 123, bar: "Hello", ;az: tag, qux: Ok(world), punned }
```
	                                      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_023.md:178:45:178:46:**
```roc
	record = { foo: 123, bar: "Hello", ;az: tag, qux: Ok(world), punned }
```
	                                           ^


**PARSE ERROR**
A parsing error occurred: `expected_arrow`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_023.md:178:52:178:54:**
```roc
	record = { foo: 123, bar: "Hello", ;az: tag, qux: Ok(world), punned }
```
	                                                  ^^


**UNDECLARED TYPE**
The type _Bar_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_023.md:36:8:36:11:**
```roc
Foo : (Bar, Baz)
```
       ^^^


**UNDECLARED TYPE**
The type _Baz_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_023.md:36:13:36:16:**
```roc
Foo : (Bar, Baz)
```
            ^^^


**UNDECLARED TYPE**
The type _Bar_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_023.md:39:2:39:5:**
```roc
	Bar, # Comment after pattern tuple item
```
	^^^


**UNDECLARED TYPE**
The type _Baz_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_023.md:40:2:40:5:**
```roc
	Baz, # Another after pattern tuple item
```
	^^^


**UNDECLARED TYPE**
The type _Ok_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_023.md:43:19:43:21:**
```roc
Some(a) : { foo : Ok(a), bar : Something }
```
                  ^^


**UNDECLARED TYPE**
The type _Something_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_023.md:43:32:43:41:**
```roc
Some(a) : { foo : Ok(a), bar : Something }
```
                               ^^^^^^^^^


**UNDECLARED TYPE**
The type _Ok_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_023.md:45:8:45:10:**
```roc
	foo : Ok(a), # After field
```
	      ^^


**UNDECLARED TYPE**
The type _Something_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_023.md:46:8:46:17:**
```roc
	bar : Something, # After last field
```
	      ^^^^^^^^^


**UNDECLARED TYPE**
The type _Ok_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_023.md:52:4:52:6:**
```roc
			Ok(a), # Comment after pattern record field
```
			^^


**UNDECLARED TYPE**
The type _Something_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_023.md:53:8:53:17:**
```roc
	bar : Something, # Another after pattern record field
```
	      ^^^^^^^^^


**MODULE NOT FOUND**
The module `pf.Stdout` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_023.md:4:1:4:42:**
```roc
import pf.Stdout exposing [line!, write!]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**NOT IMPLEMENTED**
This feature is not yet implemented: malformed import module name contains invalid control characters

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**MODULE NOT FOUND**
The module `MALFORMED_IMPORT` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_023.md:6:1:12:4:**
```roc
import # Comment after import keyword
	pf # Comment after qualifier
		.StdoutMultiline # Comment after ident
		exposing [ # Comment after exposing open
			line!, # Comment after exposed item
			write!, # Another after exposed item
		] # Comment after exposing close
```


**MODULE NOT FOUND**
The module `pkg.Something` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_023.md:14:1:14:82:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `BadName` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_023.md:16:1:16:27:**
```roc
import BadName as GoodName
```
^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `BadNameMultiline` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_023.md:17:1:20:20:**
```roc
import
	BadNameMultiline
		as
		GoodNameMultiline
```


**UNDEFINED VARIABLE**
Nothing is named `some_func` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_023.md:72:4:72:13:**
```roc
			some_func() # After debug expr
```
			^^^^^^^^^


**UNUSED VARIABLE**
Variable `lower` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_lower` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_023.md:97:3:97:8:**
```roc
		lower # After pattern comment
```
		^^^^^


**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_023.md:1:1:1:1:**
```roc
# This is a module comment!
```
^


**NOT IMPLEMENTED**
This feature is not yet implemented: alternatives pattern outside match expression

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_023.md:1:1:1:1:**
```roc
# This is a module comment!
```
^


**NOT IMPLEMENTED**
This feature is not yet implemented: alternatives pattern outside match expression

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_023.md:1:1:1:1:**
```roc
# This is a module comment!
```
^


**NOT IMPLEMENTED**
This feature is not yet implemented: alternatives pattern outside match expression

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize local_dispatch expression

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_023.md:121:21:121:27:**
```roc
		{ foo: 1, bar: 2, ..rest } => 12->add(34)
```
		                  ^^^^^^


**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_023.md:127:4:128:9:**
```roc
			.. # After spread operator
				rest, # After last field
```


**NOT IMPLEMENTED**
This feature is not yet implemented: alternatives pattern outside match expression

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**NOT IMPLEMENTED**
This feature is not yet implemented: alternatives pattern outside match expression

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_023.md:82:2:82:3:**
```roc
	b,
```
	^


**UNDEFINED VARIABLE**
Nothing is named `blah` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_023.md:141:2:141:6:**
```roc
	blah == 1 # Comment after expect statement
```
	^^^^


**UNDECLARED TYPE**
The type _String_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_023.md:143:14:143:20:**
```roc
main! : List(String) -> Result({}, _)
```
             ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `blah` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_023.md:147:9:147:13:**
```roc
	expect blah == 1
```
	       ^^^^


**UNDEFINED VARIABLE**
Nothing is named `some_func` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_023.md:158:2:158:11:**
```roc
	some_func(
```
	^^^^^^^^^


**NOT IMPLEMENTED**
This feature is not yet implemented: statement type in block

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**fuzz_crash_023.md:178:38:178:40:**
```roc
	record = { foo: 123, bar: "Hello", ;az: tag, qux: Ok(world), punned }
```
	                                    ^^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**fuzz_crash_023.md:178:40:178:41:**
```roc
	record = { foo: 123, bar: "Hello", ;az: tag, qux: Ok(world), punned }
```
	                                      ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**fuzz_crash_023.md:178:45:178:46:**
```roc
	record = { foo: 123, bar: "Hello", ;az: tag, qux: Ok(world), punned }
```
	                                           ^

This might be a syntax error, an unsupported language feature, or a typo.

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**fuzz_crash_023.md:178:52:178:71:**
```roc
	record = { foo: 123, bar: "Hello", ;az: tag, qux: Ok(world), punned }
```
	                                                  ^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `nested` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_023.md:179:42:179:48:**
```roc
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
```
	                                        ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `tag1` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_023.md:183:3:183:7:**
```roc
		tag1,
```
		^^^^


**UNDEFINED VARIABLE**
Nothing is named `nested` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_023.md:185:4:185:10:**
```roc
		(nested, tuple),
```
		 ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_023.md:188:22:188:25:**
```roc
	bin_op_result = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
```
	                    ^^^


**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize suffix_single_question expression

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize suffix_single_question expression

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**UNDEFINED VARIABLE**
Nothing is named `line!` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_023.md:191:2:191:14:**
```roc
	Stdout.line!(
```
	^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `toStr` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_023.md:193:4:193:13:**
```roc
			Num.toStr(number) # Comment after string interpolation expr
```
			^^^^^^^^^


**UNUSED VARIABLE**
Variable `tag_with_payload` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_tag_with_payload` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_023.md:164:2:164:18:**
```roc
	tag_with_payload = Ok(number)
```
	^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `interpolated` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_interpolated` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_023.md:165:2:165:14:**
```roc
	interpolated = "Hello, ${world}"
```
	^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `list` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_list` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_023.md:166:2:166:6:**
```roc
	list = [
```
	^^^^


**UNUSED VARIABLE**
Variable `record` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_record` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_023.md:178:2:178:8:**
```roc
	record = { foo: 123, bar: "Hello", ;az: tag, qux: Ok(world), punned }
```
	^^^^^^


**UNUSED VARIABLE**
Variable `multiline_tuple` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_multiline_tuple` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_023.md:180:2:180:17:**
```roc
	multiline_tuple = (
```
	^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `bin_op_result` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_bin_op_result` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_023.md:188:2:188:15:**
```roc
	bin_op_result = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
```
	^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `static_dispatch_style` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_static_dispatch_style` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_023.md:189:2:189:23:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
	^^^^^^^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _Value_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_023.md:201:9:201:14:**
```roc
tuple : Value((a, b, c))
```
        ^^^^^


**INVALID IF CONDITION**
This `if` condition needs to be a _Bool_:
**fuzz_crash_023.md:70:5:**
```roc
	if num {
```
    ^^^

Right now, it has the type:
    _Num(Int(Unsigned64))_

Every `if` condition must evaluate to a _Bool_–either `True` or `False`.

**INCOMPATIBLE MATCH PATTERNS**
The pattern in the fourth branch of this `match` differs from previous ones:
**fuzz_crash_023.md:84:2:**
```roc
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
```
  ^^^^^

The fourth pattern has this type:
    _Str_

But all the previous patterns have this type: 
    _[Red][Blue, Green]_others_

All patterns in an `match` must have compatible types.



**TYPE MISMATCH**
This expression is used in an unexpected way:
**fuzz_crash_023.md:155:2:157:3:**
```roc
	match_time(
		..., # Single args with comment
	)
```

It has the type:
    __arg -> _ret_

But I expected it to be:
    _[Red][Blue, Green]_others, _arg -> Error_

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,
KwImport,
LowerIdent,
DotUpperIdent,
KwExposing,OpenSquare,
LowerIdent,Comma,
LowerIdent,Comma,
CloseSquare,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,LowerIdent,KwAs,LowerIdent,Comma,UpperIdent,KwAs,UpperIdent,Comma,UpperIdent,DotStar,CloseSquare,
KwImport,UpperIdent,KwAs,UpperIdent,
KwImport,
UpperIdent,
KwAs,
UpperIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,
OpColon,
UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
CloseRound,Comma,
OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,
UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
CloseRound,
UpperIdent,OpColon,OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
UpperIdent,OpColon,OpenRound,
UpperIdent,Comma,
UpperIdent,Comma,
CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,
LowerIdent,OpColon,UpperIdent,Comma,
CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,
LowerIdent,
OpColon,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,
LowerIdent,OpColon,UpperIdent,Comma,
CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,
UpperIdent,Comma,
CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,Int,KwElse,Int,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,Int,
KwIf,LowerIdent,OpenCurly,
KwDbg,
LowerIdent,NoSpaceOpenRound,CloseRound,
Int,
CloseCurly,KwElse,OpenCurly,
KwDbg,Int,
LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpBar,
LowerIdent,Comma,
LowerIdent,Comma,
OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpBar,UpperIdent,OpBar,UpperIdent,OpFatArrow,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,
CloseCurly,
UpperIdent,
OpBar,
UpperIdent,
OpBar,UpperIdent,
OpFatArrow,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,
CloseCurly,
LowerIdent,
OpFatArrow,Int,
StringStart,StringPart,StringEnd,OpFatArrow,
Int,
StringStart,StringPart,StringEnd,OpBar,StringStart,StringPart,StringEnd,OpFatArrow,Int,
OpenSquare,Int,Comma,Int,Comma,Int,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,
OpFatArrow,
Int,
OpenSquare,Int,Comma,Int,OpBar,Int,Comma,Int,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,Int,
OpenSquare,
Int,Comma,
Int,OpBar,Int,Comma,
Int,Comma,
DoubleDot,
KwAs,
LowerIdent,Comma,
CloseSquare,OpFatArrow,Int,
Float,OpFatArrow,Int,
Float,OpBar,Float,OpFatArrow,Int,
OpenRound,Int,Comma,Int,Comma,Int,CloseRound,OpFatArrow,Int,
OpenRound,Int,Comma,Int,OpBar,Int,Comma,Int,CloseRound,OpFatArrow,Int,
OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,Comma,DoubleDot,LowerIdent,CloseCurly,OpFatArrow,Int,OpArrow,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
OpenCurly,
LowerIdent,
OpColon,
Int,Comma,
LowerIdent,OpColon,Int,Comma,
DoubleDot,
LowerIdent,Comma,
CloseCurly,OpFatArrow,Int,
OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,OpBar,Int,CloseCurly,OpFatArrow,Int,
OpenCurly,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,OpBar,Int,Comma,
CloseCurly,OpFatArrow,Int,
UpperIdent,NoSpaceOpenRound,Int,CloseRound,OpFatArrow,Int,
UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,Comma,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,OpFatArrow,Int,
CloseCurly,
KwExpect,
LowerIdent,OpEquals,Int,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,Underscore,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
KwVar,LowerIdent,OpAssign,Int,
KwExpect,LowerIdent,OpEquals,Int,
LowerIdent,OpAssign,UpperIdent,
KwReturn,
LowerIdent,
TripleDot,
LowerIdent,NoSpaceOpenRound,
TripleDot,Comma,
CloseRound,
LowerIdent,NoSpaceOpenRound,
KwDbg,
Int,Comma,
CloseRound,
KwCrash,
StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,OpAssign,OpenSquare,
LowerIdent,NoSpaceOpenRound,
KwDbg,
LowerIdent,Comma,
CloseRound,Comma,
Int,Comma,
Int,Comma,
CloseSquare,
KwFor,LowerIdent,KwIn,LowerIdent,OpenCurly,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,MalformedUnknownToken,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,CloseCurly,
LowerIdent,OpAssign,OpenRound,Int,Comma,StringStart,StringPart,StringEnd,Comma,LowerIdent,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpenRound,
Int,Comma,
StringStart,StringPart,StringEnd,Comma,
LowerIdent,Comma,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,
OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,Comma,
CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpDoubleQuestion,Int,OpGreaterThan,Int,OpStar,Int,OpOr,Int,OpPlus,Int,OpLessThan,Int,OpAnd,Int,OpBinaryMinus,Int,OpGreaterThanOrEq,Int,OpOr,Int,OpLessThanOrEq,Int,OpSlash,Int,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpQuestion,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,
StringStart,StringPart,OpenStringInterpolation,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseStringInterpolation,StringPart,StringEnd,Comma,
CloseRound,
CloseCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,
LowerIdent,OpAssign,OpenCurly,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,CloseRound,
KwExpect,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpEquals,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-import (raw "pf.Stdout")
			(exposing
				(exposed-lower-ident
					(text "line!"))
				(exposed-lower-ident
					(text "write!"))))
		(s-import (raw "pf.StdoutMultiline")
			(exposing
				(exposed-lower-ident
					(text "line!"))
				(exposed-lower-ident
					(text "write!"))))
		(s-import (raw "pkg.Something")
			(exposing
				(exposed-lower-ident
					(text "func")
					(as "function"))
				(exposed-upper-ident (text "Type") (as "ValueCategory"))
				(exposed-upper-ident-star (text "Custom"))))
		(s-import (raw "BadName") (alias "GoodName"))
		(s-import (raw "BadNameMultiline") (alias "GoodNameMultiline"))
		(s-type-decl
			(header (name "Map")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "a")))
				(ty-fn
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "b")))))
		(s-type-decl
			(header (name "MapML")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "a")))
				(ty-fn
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "b")))))
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tuple
				(ty (name "Bar"))
				(ty (name "Baz"))))
		(s-type-decl
			(header (name "FooMultiline")
				(args))
			(ty-tuple
				(ty (name "Bar"))
				(ty (name "Baz"))))
		(s-type-decl
			(header (name "Some")
				(args
					(ty-var (raw "a"))))
			(ty-record
				(anno-record-field (name "foo")
					(ty-apply
						(ty (name "Ok"))
						(ty-var (raw "a"))))
				(anno-record-field (name "bar")
					(ty (name "Something")))))
		(s-type-decl
			(header (name "SomeMl")
				(args
					(ty-var (raw "a"))))
			(ty-record
				(anno-record-field (name "foo")
					(ty-apply
						(ty (name "Ok"))
						(ty-var (raw "a"))))
				(anno-record-field (name "bar")
					(ty (name "Something")))))
		(s-type-decl
			(header (name "SomeMultiline")
				(args
					(ty-var (raw "a"))))
			(ty-record
				(anno-record-field (name "foo")
					(ty-apply
						(ty (name "Ok"))
						(ty-var (raw "a"))))
				(anno-record-field (name "bar")
					(ty (name "Something")))))
		(s-type-decl
			(header (name "Maybe")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Some"))
						(ty-var (raw "a")))
					(ty (name "None")))))
		(s-type-decl
			(header (name "MaybeMultiline")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Some"))
						(ty-var (raw "a")))
					(ty (name "None")))))
		(s-type-decl
			(header (name "SomeFunc")
				(args
					(ty-var (raw "a"))))
			(ty-fn
				(ty-apply
					(ty (name "Maybe"))
					(ty-var (raw "a")))
				(ty-var (raw "a"))
				(ty-apply
					(ty (name "Maybe"))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "add_one_oneline"))
			(e-lambda
				(args
					(p-ident (raw "num")))
				(e-if-then-else
					(e-ident (raw "num"))
					(e-int (raw "2"))
					(e-int (raw "5")))))
		(s-type-anno (name "add_one")
			(ty-fn
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "add_one"))
			(e-lambda
				(args
					(p-ident (raw "num")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "other"))
							(e-int (raw "1")))
						(e-if-then-else
							(e-ident (raw "num"))
							(e-block
								(statements
									(s-dbg
										(e-apply
											(e-ident (raw "some_func"))))
									(e-int (raw "0"))))
							(e-block
								(statements
									(s-dbg
										(e-int (raw "123")))
									(e-ident (raw "other")))))))))
		(s-decl
			(p-ident (raw "match_time"))
			(e-lambda
				(args
					(p-ident (raw "a"))
					(p-ident (raw "b")))
				(e-match
					(e-ident (raw "a"))
					(branches
						(branch
							(p-alternatives
								(p-tag (raw "Blue"))
								(p-tag (raw "Green"))
								(p-tag (raw "Red")))
							(e-block
								(statements
									(s-decl
										(p-ident (raw "x"))
										(e-int (raw "12")))
									(e-ident (raw "x")))))
						(branch
							(p-alternatives
								(p-tag (raw "Blue"))
								(p-tag (raw "Green"))
								(p-tag (raw "Red")))
							(e-block
								(statements
									(s-decl
										(p-ident (raw "x"))
										(e-int (raw "12")))
									(e-ident (raw "x")))))
						(branch
							(p-ident (raw "lower"))
							(e-int (raw "1")))
						(branch
							(p-string (raw """))
							(e-int (raw "100")))
						(branch
							(p-alternatives
								(p-string (raw """))
								(p-string (raw """)))
							(e-int (raw "200")))
						(branch
							(p-list
								(p-int (raw "1"))
								(p-int (raw "2"))
								(p-int (raw "3"))
								(p-list-rest (name "rest")))
							(e-int (raw "123")))
						(branch
							(p-list
								(p-int (raw "1"))
								(p-alternatives
									(p-int (raw "2"))
									(p-int (raw "5")))
								(p-int (raw "3"))
								(p-list-rest (name "rest")))
							(e-int (raw "123")))
						(branch
							(p-list
								(p-int (raw "1"))
								(p-alternatives
									(p-int (raw "2"))
									(p-int (raw "5")))
								(p-int (raw "3"))
								(p-list-rest (name "rest")))
							(e-int (raw "123")))
						(branch
							(p-frac (raw "3.14"))
							(e-int (raw "314")))
						(branch
							(p-alternatives
								(p-frac (raw "3.14"))
								(p-frac (raw "6.28")))
							(e-int (raw "314")))
						(branch
							(p-tuple
								(p-int (raw "1"))
								(p-int (raw "2"))
								(p-int (raw "3")))
							(e-int (raw "123")))
						(branch
							(p-tuple
								(p-int (raw "1"))
								(p-alternatives
									(p-int (raw "2"))
									(p-int (raw "5")))
								(p-int (raw "3")))
							(e-int (raw "123")))
						(branch
							(p-record
								(field (name "foo") (rest false)
									(p-int (raw "1")))
								(field (name "bar") (rest false)
									(p-int (raw "2")))
								(field (name "rest") (rest true)))
							(e-local-dispatch
								(e-int (raw "12"))
								(e-apply
									(e-ident (raw "add"))
									(e-int (raw "34")))))
						(branch
							(p-record
								(field (name "foo") (rest false)
									(p-int (raw "1")))
								(field (name "bar") (rest false)
									(p-int (raw "2")))
								(field (name "rest") (rest true)))
							(e-int (raw "12")))
						(branch
							(p-record
								(field (name "foo") (rest false)
									(p-int (raw "1")))
								(field (name "bar") (rest false)
									(p-alternatives
										(p-int (raw "2"))
										(p-int (raw "7")))))
							(e-int (raw "12")))
						(branch
							(p-record
								(field (name "foo") (rest false)
									(p-int (raw "1")))
								(field (name "bar") (rest false)
									(p-alternatives
										(p-int (raw "2"))
										(p-int (raw "7")))))
							(e-int (raw "12")))
						(branch
							(p-tag (raw "Ok")
								(p-int (raw "123")))
							(e-int (raw "123")))
						(branch
							(p-tag (raw "Ok")
								(p-tag (raw "Some")
									(p-ident (raw "dude"))))
							(e-ident (raw "dude")))
						(branch
							(p-tag (raw "TwoArgs")
								(p-string (raw """))
								(p-tag (raw "Some")
									(p-string (raw """))))
							(e-int (raw "1000")))))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "blah"))
				(e-int (raw "1"))))
		(s-type-anno (name "main!")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty (name "String")))
				(ty-apply
					(ty (name "Result"))
					(ty-record)
					(_))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "world"))
							(e-string
								(e-string-part (raw "World"))))
						(s-var (name "number")
							(e-int (raw "123")))
						(s-expect
							(e-binop (op "==")
								(e-ident (raw "blah"))
								(e-int (raw "1"))))
						(s-decl
							(p-ident (raw "tag"))
							(e-tag (raw "Blue")))
						(s-return
							(e-ident (raw "tag")))
						(e-ellipsis)
						(e-apply
							(e-ident (raw "match_time"))
							(e-ellipsis))
						(e-apply
							(e-ident (raw "some_func"))
							(e-dbg
								(e-int (raw "42"))))
						(s-crash
							(e-string
								(e-string-part (raw "Unreachable!"))))
						(s-decl
							(p-ident (raw "tag_with_payload"))
							(e-apply
								(e-tag (raw "Ok"))
								(e-ident (raw "number"))))
						(s-decl
							(p-ident (raw "interpolated"))
							(e-string
								(e-string-part (raw "Hello, "))
								(e-ident (raw "world"))
								(e-string-part (raw ""))))
						(s-decl
							(p-ident (raw "list"))
							(e-list
								(e-apply
									(e-ident (raw "add_one"))
									(e-dbg
										(e-ident (raw "number"))))
								(e-int (raw "456"))
								(e-int (raw "789"))))
						(s-for
							(p-ident (raw "n"))
							(e-ident (raw "list"))
							(e-block
								(statements
									(e-apply
										(e-ident (raw "Stdout.line!"))
										(e-string
											(e-string-part (raw "Adding "))
											(e-ident (raw "n"))
											(e-string-part (raw " to "))
											(e-ident (raw "number"))
											(e-string-part (raw ""))))
									(s-decl
										(p-ident (raw "number"))
										(e-binop (op "+")
											(e-ident (raw "number"))
											(e-ident (raw "n")))))))
						(s-decl
							(p-ident (raw "record"))
							(e-malformed (reason "expected_expr_close_curly_or_comma")))
						(e-malformed (reason "expr_unexpected_token"))
						(e-ident (raw "tag"))
						(e-malformed (reason "expr_unexpected_token"))
						(s-type-anno (name "qux")
							(ty-malformed (tag "expected_arrow")))
						(s-decl
							(p-ident (raw "tuple"))
							(e-tuple
								(e-int (raw "123"))
								(e-string
									(e-string-part (raw "World")))
								(e-ident (raw "tag"))
								(e-apply
									(e-tag (raw "Ok"))
									(e-ident (raw "world")))
								(e-tuple
									(e-ident (raw "nested"))
									(e-ident (raw "tuple")))
								(e-list
									(e-int (raw "1"))
									(e-int (raw "2"))
									(e-int (raw "3")))))
						(s-decl
							(p-ident (raw "multiline_tuple"))
							(e-tuple
								(e-int (raw "123"))
								(e-string
									(e-string-part (raw "World")))
								(e-ident (raw "tag1"))
								(e-apply
									(e-tag (raw "Ok"))
									(e-ident (raw "world")))
								(e-tuple
									(e-ident (raw "nested"))
									(e-ident (raw "tuple")))
								(e-list
									(e-int (raw "1"))
									(e-int (raw "2"))
									(e-int (raw "3")))))
						(s-decl
							(p-ident (raw "bin_op_result"))
							(e-binop (op "or")
								(e-binop (op ">")
									(e-binop (op "??")
										(e-apply
											(e-tag (raw "Err"))
											(e-ident (raw "foo")))
										(e-int (raw "12")))
									(e-binop (op "*")
										(e-int (raw "5"))
										(e-int (raw "5"))))
								(e-binop (op "or")
									(e-binop (op "and")
										(e-binop (op "<")
											(e-binop (op "+")
												(e-int (raw "13"))
												(e-int (raw "2")))
											(e-int (raw "5")))
										(e-binop (op ">=")
											(e-binop (op "-")
												(e-int (raw "10"))
												(e-int (raw "1")))
											(e-int (raw "16"))))
									(e-binop (op "<=")
										(e-int (raw "12"))
										(e-binop (op "/")
											(e-int (raw "3"))
											(e-int (raw "5")))))))
						(s-decl
							(p-ident (raw "static_dispatch_style"))
							(e-field-access
								(e-field-access
									(e-field-access
										(e-question-suffix
											(e-apply
												(e-ident (raw "some_fn"))
												(e-ident (raw "arg1"))))
										(e-question-suffix
											(e-apply
												(e-ident (raw "static_dispatch_method")))))
									(e-question-suffix
										(e-apply
											(e-ident (raw "next_static_dispatch_method")))))
								(e-question-suffix
									(e-ident (raw "record_field")))))
						(e-question-suffix
							(e-apply
								(e-ident (raw "Stdout.line!"))
								(e-ident (raw "interpolated"))))
						(e-apply
							(e-ident (raw "Stdout.line!"))
							(e-string
								(e-string-part (raw "How about "))
								(e-apply
									(e-ident (raw "Num.toStr"))
									(e-ident (raw "number")))
								(e-string-part (raw " as a string?"))))))))
		(s-type-anno (name "empty")
			(ty-record))
		(s-decl
			(p-ident (raw "empty"))
			(e-record))
		(s-type-anno (name "tuple")
			(ty-apply
				(ty (name "Value"))
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "b"))
					(ty-var (raw "c")))))
		(s-expect
			(e-block
				(statements
					(s-decl
						(p-ident (raw "foo"))
						(e-int (raw "1")))
					(s-decl
						(p-ident (raw "blah"))
						(e-int (raw "1")))
					(e-binop (op "==")
						(e-ident (raw "blah"))
						(e-ident (raw "foo"))))))))
~~~
# FORMATTED
~~~roc
# This is a module comment!
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout exposing [line!, write!]

import # Comment after import keyword
	pf.StdoutMultiline # Comment after ident
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
	record = 
		tag
		qux : 
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
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "add_one_oneline"))
		(e-lambda
			(args
				(p-assign (ident "num")))
			(e-if
				(if-branches
					(if-branch
						(e-lookup-local
							(p-assign (ident "num")))
						(e-num (value "2"))))
				(if-else
					(e-num (value "5"))))))
	(d-let
		(p-assign (ident "add_one"))
		(e-lambda
			(args
				(p-assign (ident "num")))
			(e-block
				(s-let
					(p-assign (ident "other"))
					(e-num (value "1")))
				(e-if
					(if-branches
						(if-branch
							(e-lookup-local
								(p-assign (ident "num")))
							(e-block
								(s-dbg
									(e-call
										(e-runtime-error (tag "ident_not_in_scope"))))
								(e-num (value "0")))))
					(if-else
						(e-block
							(s-dbg
								(e-num (value "123")))
							(e-lookup-local
								(p-assign (ident "other"))))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "U64") (builtin))
					(ty-lookup (name "U64") (builtin))))))
	(d-let
		(p-assign (ident "match_time"))
		(e-closure
			(captures
				(capture (ident "x"))
				(capture (ident "dude"))
				(capture (ident "x")))
			(e-lambda
				(args
					(p-assign (ident "a"))
					(p-assign (ident "b")))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "a"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag))
									(pattern (degenerate false)
										(p-applied-tag))
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-block
										(s-let
											(p-assign (ident "x"))
											(e-num (value "12")))
										(e-lookup-local
											(p-assign (ident "x"))))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag))
									(pattern (degenerate false)
										(p-applied-tag))
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-block
										(s-let
											(p-assign (ident "x"))
											(e-num (value "12")))
										(e-lookup-local
											(p-assign (ident "x"))))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-assign (ident "lower"))))
								(value
									(e-num (value "1"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-str (text """))))
								(value
									(e-num (value "100"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-str (text """)))
									(pattern (degenerate false)
										(p-str (text """))))
								(value
									(e-num (value "200"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-list
											(patterns
												(p-num (value "1"))
												(p-num (value "2"))
												(p-num (value "3")))
											(rest-at (index 3)
												(p-assign (ident "rest"))))))
								(value
									(e-num (value "123"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-list
											(patterns
												(p-num (value "1"))
												(p-runtime-error (tag "not_implemented"))
												(p-num (value "3")))
											(rest-at (index 3)
												(p-assign (ident "rest"))))))
								(value
									(e-num (value "123"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-list
											(patterns
												(p-num (value "1"))
												(p-runtime-error (tag "not_implemented"))
												(p-num (value "3")))
											(rest-at (index 3)
												(p-assign (ident "rest"))))))
								(value
									(e-num (value "123"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-small-dec)))
								(value
									(e-num (value "314"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-small-dec))
									(pattern (degenerate false)
										(p-small-dec)))
								(value
									(e-num (value "314"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-tuple
											(patterns
												(p-num (value "1"))
												(p-num (value "2"))
												(p-num (value "3"))))))
								(value
									(e-num (value "123"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-tuple
											(patterns
												(p-num (value "1"))
												(p-runtime-error (tag "not_implemented"))
												(p-num (value "3"))))))
								(value
									(e-num (value "123"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-record-destructure
											(destructs
												(record-destruct (label "foo") (ident "foo")
													(sub-pattern
														(p-num (value "1"))))
												(record-destruct (label "bar") (ident "bar")
													(sub-pattern
														(p-num (value "2"))))
												(record-destruct (label "rest") (ident "rest")
													(required
														(p-assign (ident "rest"))))))))
								(value
									(e-runtime-error (tag "not_implemented"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-record-destructure
											(destructs
												(record-destruct (label "foo") (ident "foo")
													(sub-pattern
														(p-num (value "1"))))
												(record-destruct (label "bar") (ident "bar")
													(sub-pattern
														(p-num (value "2"))))
												(record-destruct (label "rest") (ident "rest")
													(required
														(p-assign (ident "rest"))))))))
								(value
									(e-num (value "12"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-record-destructure
											(destructs
												(record-destruct (label "foo") (ident "foo")
													(sub-pattern
														(p-num (value "1"))))
												(record-destruct (label "bar") (ident "bar")
													(sub-pattern
														(p-runtime-error (tag "not_implemented"))))))))
								(value
									(e-num (value "12"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-record-destructure
											(destructs
												(record-destruct (label "foo") (ident "foo")
													(sub-pattern
														(p-num (value "1"))))
												(record-destruct (label "bar") (ident "bar")
													(sub-pattern
														(p-runtime-error (tag "not_implemented"))))))))
								(value
									(e-num (value "12"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-num (value "123"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-lookup-local
										(p-assign (ident "dude")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-num (value "1000"))))))))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "match_time"))
				(capture (ident "add_one")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-let
						(p-assign (ident "world"))
						(e-string
							(e-literal (string "World"))))
					(s-var
						(p-assign (ident "number"))
						(e-num (value "123")))
					(s-expect
						(e-binop (op "eq")
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-num (value "1"))))
					(s-let
						(p-assign (ident "tag"))
						(e-tag (name "Blue")))
					(s-return
						(e-lookup-local
							(p-assign (ident "tag"))))
					(s-expr
						(e-not-implemented))
					(s-expr
						(e-call
							(e-lookup-local
								(p-assign (ident "match_time")))
							(e-not-implemented)))
					(s-expr
						(e-call
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-dbg
								(e-num (value "42")))))
					(s-crash (msg "Unreachable!"))
					(s-let
						(p-assign (ident "tag_with_payload"))
						(e-tag (name "Ok")
							(args
								(e-lookup-local
									(p-assign (ident "number"))))))
					(s-let
						(p-assign (ident "interpolated"))
						(e-string
							(e-literal (string "Hello, "))
							(e-lookup-local
								(p-assign (ident "world")))
							(e-literal (string ""))))
					(s-let
						(p-assign (ident "list"))
						(e-list
							(elems
								(e-call
									(e-lookup-local
										(p-assign (ident "add_one")))
									(e-dbg
										(e-lookup-local
											(p-assign (ident "number")))))
								(e-num (value "456"))
								(e-num (value "789")))))
					(s-runtime-error (tag "not_implemented"))
					(s-let
						(p-assign (ident "record"))
						(e-runtime-error (tag "expr_not_canonicalized")))
					(s-expr
						(e-runtime-error (tag "expr_not_canonicalized")))
					(s-expr
						(e-lookup-local
							(p-assign (ident "tag"))))
					(s-expr
						(e-runtime-error (tag "expr_not_canonicalized")))
					(s-let
						(p-assign (ident "tuple"))
						(e-tuple
							(elems
								(e-num (value "123"))
								(e-string
									(e-literal (string "World")))
								(e-lookup-local
									(p-assign (ident "tag")))
								(e-tag (name "Ok")
									(args
										(e-lookup-local
											(p-assign (ident "world")))))
								(e-tuple
									(elems
										(e-runtime-error (tag "ident_not_in_scope"))
										(e-lookup-local
											(p-assign (ident "tuple")))))
								(e-list
									(elems
										(e-num (value "1"))
										(e-num (value "2"))
										(e-num (value "3")))))))
					(s-let
						(p-assign (ident "multiline_tuple"))
						(e-tuple
							(elems
								(e-num (value "123"))
								(e-string
									(e-literal (string "World")))
								(e-runtime-error (tag "ident_not_in_scope"))
								(e-tag (name "Ok")
									(args
										(e-lookup-local
											(p-assign (ident "world")))))
								(e-tuple
									(elems
										(e-runtime-error (tag "ident_not_in_scope"))
										(e-lookup-local
											(p-assign (ident "tuple")))))
								(e-list
									(elems
										(e-num (value "1"))
										(e-num (value "2"))
										(e-num (value "3")))))))
					(s-let
						(p-assign (ident "bin_op_result"))
						(e-binop (op "or")
							(e-binop (op "gt")
								(e-binop (op "null_coalesce")
									(e-tag (name "Err")
										(args
											(e-runtime-error (tag "ident_not_in_scope"))))
									(e-num (value "12")))
								(e-binop (op "mul")
									(e-num (value "5"))
									(e-num (value "5"))))
							(e-binop (op "or")
								(e-binop (op "and")
									(e-binop (op "lt")
										(e-binop (op "add")
											(e-num (value "13"))
											(e-num (value "2")))
										(e-num (value "5")))
									(e-binop (op "ge")
										(e-binop (op "sub")
											(e-num (value "10"))
											(e-num (value "1")))
										(e-num (value "16"))))
								(e-binop (op "le")
									(e-num (value "12"))
									(e-binop (op "div")
										(e-num (value "3"))
										(e-num (value "5")))))))
					(s-let
						(p-assign (ident "static_dispatch_style"))
						(e-dot-access (field "unknown")
							(receiver
								(e-dot-access (field "unknown")
									(receiver
										(e-dot-access (field "unknown")
											(receiver
												(e-runtime-error (tag "not_implemented")))))))))
					(s-expr
						(e-runtime-error (tag "not_implemented")))
					(e-call
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-string
							(e-literal (string "How about "))
							(e-call
								(e-runtime-error (tag "ident_not_in_scope"))
								(e-lookup-local
									(p-assign (ident "number"))))
							(e-literal (string " as a string?")))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-apply (name "List") (builtin)
						(ty-malformed))
					(ty-apply (name "Result") (external (module-idx "3") (target-node-idx "0"))
						(ty-record)
						(ty-underscore))))))
	(d-let
		(p-assign (ident "empty"))
		(e-empty_record)
		(annotation
			(declared-type
				(ty-record))))
	(s-alias-decl
		(ty-header (name "Map")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-fn (effectful false)
			(ty-apply (name "List") (builtin)
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(ty-parens
				(ty-fn (effectful false)
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-rigid-var-lookup (ty-rigid-var (name "b")))))
			(ty-apply (name "List") (builtin)
				(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
	(s-alias-decl
		(ty-header (name "MapML")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-fn (effectful false)
			(ty-apply (name "List") (builtin)
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(ty-parens
				(ty-fn (effectful false)
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-rigid-var-lookup (ty-rigid-var (name "b")))))
			(ty-apply (name "List") (builtin)
				(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
	(s-alias-decl
		(ty-header (name "Foo"))
		(ty-tuple
			(ty-malformed)
			(ty-malformed)))
	(s-alias-decl
		(ty-header (name "FooMultiline"))
		(ty-tuple
			(ty-malformed)
			(ty-malformed)))
	(s-alias-decl
		(ty-header (name "Some")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-record
			(field (field "foo")
				(ty-malformed))
			(field (field "bar")
				(ty-malformed))))
	(s-alias-decl
		(ty-header (name "SomeMl")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-record
			(field (field "foo")
				(ty-malformed))
			(field (field "bar")
				(ty-malformed))))
	(s-alias-decl
		(ty-header (name "SomeMultiline")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-record
			(field (field "foo")
				(ty-malformed))
			(field (field "bar")
				(ty-malformed))))
	(s-alias-decl
		(ty-header (name "Maybe")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Some")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(ty-tag-name (name "None"))))
	(s-alias-decl
		(ty-header (name "MaybeMultiline")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Some")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(ty-tag-name (name "None"))))
	(s-alias-decl
		(ty-header (name "SomeFunc")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-fn (effectful false)
			(ty-apply (name "Maybe") (local)
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))
			(ty-apply (name "Maybe") (local)
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
	(s-import (module "pf.Stdout")
		(exposes
			(exposed (name "line!") (wildcard false))
			(exposed (name "write!") (wildcard false))))
	(s-import (module "MALFORMED_IMPORT")
		(exposes
			(exposed (name "line!") (wildcard false))
			(exposed (name "write!") (wildcard false))))
	(s-import (module "pkg.Something")
		(exposes
			(exposed (name "func") (alias "function") (wildcard false))
			(exposed (name "Type") (alias "ValueCategory") (wildcard false))
			(exposed (name "Custom") (wildcard true))))
	(s-import (module "BadName")
		(exposes))
	(s-import (module "BadNameMultiline")
		(exposes))
	(s-expect
		(e-binop (op "eq")
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-num (value "1"))))
	(s-expect
		(e-block
			(s-let
				(p-assign (ident "foo"))
				(e-num (value "1")))
			(s-let
				(p-assign (ident "blah"))
				(e-num (value "1")))
			(e-binop (op "eq")
				(e-lookup-local
					(p-assign (ident "blah")))
				(e-lookup-local
					(p-assign (ident "foo")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Bool -> Num(_size)"))
		(patt (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(patt (type "[Red][Blue, Green]_others, _arg -> Error"))
		(patt (type "List(Error) -> Error"))
		(patt (type "{}")))
	(type_decls
		(alias (type "Map(a, b)")
			(ty-header (name "Map")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(alias (type "MapML(a, b)")
			(ty-header (name "MapML")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(alias (type "Foo")
			(ty-header (name "Foo")))
		(alias (type "FooMultiline")
			(ty-header (name "FooMultiline")))
		(alias (type "Some(a)")
			(ty-header (name "Some")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "SomeMl(a)")
			(ty-header (name "SomeMl")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "SomeMultiline(a)")
			(ty-header (name "SomeMultiline")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Maybe(a)")
			(ty-header (name "Maybe")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "MaybeMultiline(a)")
			(ty-header (name "MaybeMultiline")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "SomeFunc(a)")
			(ty-header (name "SomeFunc")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "Bool -> Num(_size)"))
		(expr (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(expr (type "[Red][Blue, Green]_others, _arg -> Error"))
		(expr (type "List(Error) -> Error"))
		(expr (type "{}"))))
~~~
