# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
# This is a mod comment!
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

main! : List(String) -> Try({}, _)
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
EXPECTED RECORD ACCESSOR - fuzz_crash_023.md:154:2:154:5
EXPECTED RECORD FIELD - fuzz_crash_023.md:178:37:178:38
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_023.md:178:45:178:46
EXPECTED FUNCTION ARROW - fuzz_crash_023.md:178:52:178:54
MOD NOT FOUND - fuzz_crash_023.md:16:1:16:27
MOD NOT FOUND - fuzz_crash_023.md:17:1:20:20
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
NAME NOT IN SCOPE - fuzz_crash_023.md:72:4:72:13
UNUSED VARIABLE - fuzz_crash_023.md:97:3:97:8
UNUSED VARIABLE - fuzz_crash_023.md:1:1:1:1
NOT IMPLEMENTED - fuzz_crash_023.md:108:7:108:12
UNUSED VARIABLE - fuzz_crash_023.md:1:1:1:1
NOT IMPLEMENTED - fuzz_crash_023.md:111:4:111:9
UNUSED VARIABLE - fuzz_crash_023.md:1:1:1:1
NOT IMPLEMENTED - fuzz_crash_023.md:120:7:120:12
NAME NOT IN SCOPE - fuzz_crash_023.md:121:37:121:40
UNUSED VARIABLE - fuzz_crash_023.md:121:21:121:27
UNUSED VARIABLE - fuzz_crash_023.md:127:4:128:9
NOT IMPLEMENTED - fuzz_crash_023.md:130:18:130:23
NOT IMPLEMENTED - fuzz_crash_023.md:133:9:133:14
UNUSED VARIABLE - fuzz_crash_023.md:82:2:82:3
NAME NOT IN SCOPE - fuzz_crash_023.md:141:2:141:6
UNDECLARED TYPE - fuzz_crash_023.md:143:14:143:20
NAME NOT IN SCOPE - fuzz_crash_023.md:147:9:147:13
UNRECOGNIZED SYNTAX - fuzz_crash_023.md:154:2:154:5
NAME NOT IN SCOPE - fuzz_crash_023.md:158:2:158:11
NAME NOT IN SCOPE - fuzz_crash_023.md:175:3:175:15
UNRECOGNIZED SYNTAX - fuzz_crash_023.md:178:11:178:45
UNRECOGNIZED SYNTAX - fuzz_crash_023.md:178:45:178:46
MALFORMED TYPE - fuzz_crash_023.md:178:52:178:71
NAME NOT IN SCOPE - fuzz_crash_023.md:179:42:179:48
INVALID ASSIGNMENT TO ITSELF - fuzz_crash_023.md:179:50:179:55
NAME NOT IN SCOPE - fuzz_crash_023.md:183:3:183:7
NAME NOT IN SCOPE - fuzz_crash_023.md:185:4:185:10
NAME NOT IN SCOPE - fuzz_crash_023.md:188:22:188:25
NAME NOT IN SCOPE - fuzz_crash_023.md:189:26:189:33
NAME NOT IN SCOPE - fuzz_crash_023.md:189:34:189:38
NAME NOT IN SCOPE - fuzz_crash_023.md:190:2:190:14
NAME NOT IN SCOPE - fuzz_crash_023.md:191:2:191:14
DOES NOT EXIST - fuzz_crash_023.md:193:4:193:13
UNUSED VARIABLE - fuzz_crash_023.md:164:2:164:18
UNUSED VARIABLE - fuzz_crash_023.md:178:2:178:8
UNUSED VARIABLE - fuzz_crash_023.md:178:47:178:71
UNUSED VARIABLE - fuzz_crash_023.md:180:2:180:17
UNUSED VARIABLE - fuzz_crash_023.md:188:2:188:15
UNUSED VARIABLE - fuzz_crash_023.md:189:2:189:23
UNDECLARED TYPE - fuzz_crash_023.md:201:9:201:14
TYPE MISMATCH - fuzz_crash_023.md:70:5:70:8
MISSING METHOD - fuzz_crash_023.md:99:3:99:8
MISSING METHOD - fuzz_crash_023.md:101:3:101:8
TYPE MISMATCH - fuzz_crash_023.md:84:2:84:2
DECLARATION HAS NO VALUE - fuzz_crash_023.md:178:47:178:71
TOO FEW ARGS - fuzz_crash_023.md:155:2:157:3
TYPE MISMATCH - fuzz_crash_023.md:167:3:167:3
DECLARATION HAS NO VALUE - fuzz_crash_023.md:178:47:178:71
TYPE MISMATCH - fuzz_crash_023.md:175:26:175:27
TYPE MISMATCH - fuzz_crash_023.md:175:34:175:40
DECLARATION HAS NO VALUE - fuzz_crash_023.md:201:1:201:25
MISSING METHOD - fuzz_crash_023.md:189:26:189:40
MISSING METHOD - fuzz_crash_023.md:189:26:189:66
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 154 2) (end 154 5))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "...")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 154 2) (end 154 5) (annotation error) (line-text "\t..."))))
	(report
		(severity runtime_error)
		(title "Expected Record Field")
		(region (start 178 37) (end 178 38))
		(headline
			(reflow "I was parsing a record expression, and I expected a lowercase field name."))
		(document
			(reflow "Record fields start with lowercase names. After the name, either write ")
			(annotated code ": value")
			(reflow " or omit the value to use field punning.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ name: \"Ada\", age }")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ";")
			(text " here.")
			(line-break)
			(reflow "This token is malformed, so it cannot be used as ordinary Roc syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 178 37) (end 178 38) (annotation error) (line-text "\trecord = { foo: 123, bar: \"Hello\", ;az: tag, qux: Ok(world), punned }"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 178 45) (end 178 46))
		(headline
			(reflow "I was parsing an expression, and this token cannot start an expression here."))
		(document
			(reflow "Expressions can be names, literals, tags, records, lists, tuples, lambdas, blocks, conditionals, matches, or function calls.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "add(1, 2)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ",")
			(text " here.")
			(line-break)
			(reflow "A comma separates items, but there must be a valid item on both sides of it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 178 45) (end 178 46) (annotation error) (line-text "\trecord = { foo: 123, bar: \"Hello\", ;az: tag, qux: Ok(world), punned }"))))
	(report
		(severity runtime_error)
		(title "Expected Function Arrow")
		(region (start 178 52) (end 178 54))
		(headline
			(reflow "I was parsing a function type, and I expected `->` or `=>` before the return type."))
		(document
			(reflow "Function types list argument types first, then an arrow, then the return type.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str, U64 -> Bool")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "Ok")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 178 52) (end 178 54) (annotation error) (line-text "\trecord = { foo: 123, bar: \"Hello\", ;az: tag, qux: Ok(world), punned }"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 16 1) (end 16 27))
		(headline
			(text "The mod ")
			(annotated code "BadName")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 16 1) (end 16 27) (annotation error) (line-text "import BadName as GoodName"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 17 1) (end 20 20))
		(headline
			(text "The mod ")
			(annotated code "BadNameMultiline")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 17 1) (end 20 20) (annotation error) (line-text "import\n\tBadNameMultiline\n\t\tas\n\t\tGoodNameMultiline"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 36 8) (end 36 11))
		(headline
			(reflow "The type ")
			(annotated code "Bar")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 36 8) (end 36 11) (annotation error) (line-text "Foo : (Bar, Baz)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 36 13) (end 36 16))
		(headline
			(reflow "The type ")
			(annotated code "Baz")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 36 13) (end 36 16) (annotation error) (line-text "Foo : (Bar, Baz)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 39 2) (end 39 5))
		(headline
			(reflow "The type ")
			(annotated code "Bar")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 39 2) (end 39 5) (annotation error) (line-text "\tBar, # Comment after pattern tuple item"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 40 2) (end 40 5))
		(headline
			(reflow "The type ")
			(annotated code "Baz")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 40 2) (end 40 5) (annotation error) (line-text "\tBaz, # Another after pattern tuple item"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 43 19) (end 43 21))
		(headline
			(reflow "The type ")
			(annotated code "Ok")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 43 19) (end 43 21) (annotation error) (line-text "Some(a) : { foo : Ok(a), bar : Something }"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 43 32) (end 43 41))
		(headline
			(reflow "The type ")
			(annotated code "Something")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 43 32) (end 43 41) (annotation error) (line-text "Some(a) : { foo : Ok(a), bar : Something }"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 45 8) (end 45 10))
		(headline
			(reflow "The type ")
			(annotated code "Ok")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 45 8) (end 45 10) (annotation error) (line-text "\tfoo : Ok(a), # After field"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 46 8) (end 46 17))
		(headline
			(reflow "The type ")
			(annotated code "Something")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 46 8) (end 46 17) (annotation error) (line-text "\tbar : Something, # After last field"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 52 4) (end 52 6))
		(headline
			(reflow "The type ")
			(annotated code "Ok")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 52 4) (end 52 6) (annotation error) (line-text "\t\t\tOk(a), # Comment after pattern record field"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 53 8) (end 53 17))
		(headline
			(reflow "The type ")
			(annotated code "Something")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 53 8) (end 53 17) (annotation error) (line-text "\tbar : Something, # Another after pattern record field"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 72 4) (end 72 13))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "some_func")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 72 4) (end 72 13) (annotation error) (line-text "\t\t\tsome_func() # After debug expr"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 97 3) (end 97 8))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "lower")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_lower")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 97 3) (end 97 8) (annotation error) (line-text "\t\tlower # After pattern comment"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 1 1) (end 1 1))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "rest")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_rest")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 1 1) (end 1 1) (annotation error) (line-text "# This is a mod comment!"))))
	(report
		(severity fatal)
		(title "Not Implemented")
		(region (start 108 7) (end 108 12))
		(headline
			(reflow "This feature is not yet implemented: ")
			(annotation-start emphasis)
			(text "alternatives pattern outside match expression")
			(annotation-end)
			(reflow "."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 108 7) (end 108 12) (annotation error) (line-text "\t\t[1, 2 | 5, 3, .. as rest] => 123"))
			(line-break)
			(reflow "This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!")
			(line-break)))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 1 1) (end 1 1))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "rest")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_rest")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 1 1) (end 1 1) (annotation error) (line-text "# This is a mod comment!"))))
	(report
		(severity fatal)
		(title "Not Implemented")
		(region (start 111 4) (end 111 9))
		(headline
			(reflow "This feature is not yet implemented: ")
			(annotation-start emphasis)
			(text "alternatives pattern outside match expression")
			(annotation-end)
			(reflow "."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 111 4) (end 111 9) (annotation error) (line-text "\t\t\t2 | 5,"))
			(line-break)
			(reflow "This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!")
			(line-break)))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 1 1) (end 1 1))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "rest")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_rest")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 1 1) (end 1 1) (annotation error) (line-text "# This is a mod comment!"))))
	(report
		(severity fatal)
		(title "Not Implemented")
		(region (start 120 7) (end 120 12))
		(headline
			(reflow "This feature is not yet implemented: ")
			(annotation-start emphasis)
			(text "alternatives pattern outside match expression")
			(annotation-end)
			(reflow "."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 120 7) (end 120 12) (annotation error) (line-text "\t\t(1, 2 | 5, 3) => 123"))
			(line-break)
			(reflow "This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!")
			(line-break)))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 121 37) (end 121 40))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "add")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 121 37) (end 121 40) (annotation error) (line-text "\t\t{ foo: 1, bar: 2, ..rest } => 12->add(34)"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 121 21) (end 121 27))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "rest")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_rest")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 121 21) (end 121 27) (annotation error) (line-text "\t\t{ foo: 1, bar: 2, ..rest } => 12->add(34)"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 127 4) (end 128 9))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "rest")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_rest")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 127 4) (end 128 9) (annotation error) (line-text "\t\t\t.. # After spread operator\n\t\t\t\trest, # After last field"))))
	(report
		(severity fatal)
		(title "Not Implemented")
		(region (start 130 18) (end 130 23))
		(headline
			(reflow "This feature is not yet implemented: ")
			(annotation-start emphasis)
			(text "alternatives pattern outside match expression")
			(annotation-end)
			(reflow "."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 130 18) (end 130 23) (annotation error) (line-text "\t\t{ foo: 1, bar: 2 | 7 } => 12"))
			(line-break)
			(reflow "This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!")
			(line-break)))
	(report
		(severity fatal)
		(title "Not Implemented")
		(region (start 133 9) (end 133 14))
		(headline
			(reflow "This feature is not yet implemented: ")
			(annotation-start emphasis)
			(text "alternatives pattern outside match expression")
			(annotation-end)
			(reflow "."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 133 9) (end 133 14) (annotation error) (line-text "\t\t\tbar: 2 | 7, # After last record field"))
			(line-break)
			(reflow "This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!")
			(line-break)))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 82 2) (end 82 3))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "b")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_b")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 82 2) (end 82 3) (annotation error) (line-text "\tb,"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 141 2) (end 141 6))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "blah")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 141 2) (end 141 6) (annotation error) (line-text "\tblah == 1 # Comment after expect statement"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 143 14) (end 143 20))
		(headline
			(reflow "The type ")
			(annotated code "String")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 143 14) (end 143 20) (annotation error) (line-text "main! : List(String) -> Try({}, _)"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 147 9) (end 147 13))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "blah")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 147 9) (end 147 13) (annotation error) (line-text "\texpect blah == 1"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 154 2) (end 154 5))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 154 2) (end 154 5) (annotation error) (line-text "\t..."))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 158 2) (end 158 11))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "some_func")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 158 2) (end 158 11) (annotation error) (line-text "\tsome_func("))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 175 3) (end 175 15))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "line!")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 175 3) (end 175 15) (annotation error) (line-text "\t\tStdout.line!(\"Adding ${n} to ${number}\")"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 178 11) (end 178 45))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 178 11) (end 178 45) (annotation error) (line-text "\trecord = { foo: 123, bar: \"Hello\", ;az: tag, qux: Ok(world), punned }"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 178 45) (end 178 46))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 178 45) (end 178 46) (annotation error) (line-text "\trecord = { foo: 123, bar: \"Hello\", ;az: tag, qux: Ok(world), punned }"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Malformed Type")
		(region (start 178 52) (end 178 71))
		(headline
			(reflow "This type annotation is malformed or contains invalid syntax."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 178 52) (end 178 71) (annotation error) (line-text "\trecord = { foo: 123, bar: \"Hello\", ;az: tag, qux: Ok(world), punned }"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 179 42) (end 179 48))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "nested")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 179 42) (end 179 48) (annotation error) (line-text "\ttuple = (123, \"World\", tag, Ok(world), (nested, tuple), [1, 2, 3])"))))
	(report
		(severity runtime_error)
		(title "Invalid Assignment To Itself")
		(region (start 179 50) (end 179 55))
		(headline
			(reflow "The value ")
			(annotated symbol-unqualified "tuple")
			(reflow " is assigned to itself, which would cause an infinite loop at runtime."))
		(document
			(reflow "Only functions can reference themselves (for recursion). For non-function values, the right-hand side must be fully computable without referring to the value being assigned.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 179 50) (end 179 55) (annotation error) (line-text "\ttuple = (123, \"World\", tag, Ok(world), (nested, tuple), [1, 2, 3])"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 183 3) (end 183 7))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "tag1")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 183 3) (end 183 7) (annotation error) (line-text "\t\ttag1,"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 185 4) (end 185 10))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "nested")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 185 4) (end 185 10) (annotation error) (line-text "\t\t(nested, tuple),"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 188 22) (end 188 25))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "foo")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 188 22) (end 188 25) (annotation error) (line-text "\tbin_op_result = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 189 26) (end 189 33))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "some_fn")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 189 26) (end 189 33) (annotation error) (line-text "\tstatic_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 189 34) (end 189 38))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "arg1")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 189 34) (end 189 38) (annotation error) (line-text "\tstatic_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 190 2) (end 190 14))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "line!")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 190 2) (end 190 14) (annotation error) (line-text "\tStdout.line!(interpolated)?"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 191 2) (end 191 14))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "line!")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 191 2) (end 191 14) (annotation error) (line-text "\tStdout.line!("))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 193 4) (end 193 13))
		(headline
			(annotated symbol-unqualified "Num.toStr")
			(reflow " does not exist."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 193 4) (end 193 13) (annotation error) (line-text "\t\t\tNum.toStr(number) # Comment after string interpolation expr"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 164 2) (end 164 18))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "tag_with_payload")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_tag_with_payload")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 164 2) (end 164 18) (annotation error) (line-text "\ttag_with_payload = Ok(number)"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 178 2) (end 178 8))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "record")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_record")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 178 2) (end 178 8) (annotation error) (line-text "\trecord = { foo: 123, bar: \"Hello\", ;az: tag, qux: Ok(world), punned }"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 178 47) (end 178 71))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "qux")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_qux")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 178 47) (end 178 71) (annotation error) (line-text "\trecord = { foo: 123, bar: \"Hello\", ;az: tag, qux: Ok(world), punned }"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 180 2) (end 180 17))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "multiline_tuple")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_multiline_tuple")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 180 2) (end 180 17) (annotation error) (line-text "\tmultiline_tuple = ("))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 188 2) (end 188 15))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "bin_op_result")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_bin_op_result")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 188 2) (end 188 15) (annotation error) (line-text "\tbin_op_result = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 189 2) (end 189 23))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "static_dispatch_style")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_static_dispatch_style")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_023.md") (start 189 2) (end 189 23) (annotation error) (line-text "\tstatic_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 201 9) (end 201 14))
		(headline
			(reflow "The type ")
			(annotated code "Value")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 201 9) (end 201 14) (annotation error) (line-text "tuple : Value((a, b, c))"))))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 70 5) (end 70 8))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "if")
			(reflow " ")
			(reflow "condition must evaluate to a")
			(reflow " ")
			(annotated code "Bool")
			(reflow " ")
			(reflow "– either")
			(reflow " ")
			(annotated code "True")
			(reflow " ")
			(reflow "or")
			(reflow " ")
			(annotated code "False")
			(reflow "."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 70 5) (end 70 8) (annotation error) (line-text "\tif num {"))
			(line-break)
			(reflow "It is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U64")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But I need this to be a")
			(reflow " ")
			(annotated code "Bool")
			(reflow " ")
			(reflow "value.")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 99 3) (end 99 8))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_quote")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 99 3) (end 99 8) (annotation error) (line-text "\t\t\"foo\" => # After arrow comment"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "from_quote")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Blue, Green, Red, ..]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 101 3) (end 101 8))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_quote")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 101 3) (end 101 8) (annotation error) (line-text "\t\t\"foo\" | \"bar\" => 200"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "from_quote")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Blue, Green, Red, ..]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 84 2) (end 138 3))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "sixth")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "does not match the previous ones."))
		(document
			(source-underlines
				(display (file "fuzz_crash_023.md") (start 84 2) (end 138 3) (annotation dim) (line-text "\tmatch a {\n\t\tBlue | Green | Red => {\n\t\t\tx = 12\n\t\t\tx\n\t\t}\n\t\tBlue # After pattern in alt\n\t\t| # Before pattern in alt\n\t\t\tGreen\n\t\t| Red # After alt pattern\n\t\t\t=> {\n\t\t\t\tx = 12\n\t\t\t\tx\n\t\t\t}\n\t\tlower # After pattern comment\n\t\t\t=> 1\n\t\t\"foo\" => # After arrow comment\n\t\t\t100\n\t\t\"foo\" | \"bar\" => 200\n\t\t[1, 2, 3, .. as rest] # After pattern comment\n\t\t\t=> # After arrow comment\n\t\t\t\t123 # After branch comment\n\n\t\t# Just a random comment\n\n\t\t[1, 2 | 5, 3, .. as rest] => 123\n\t\t[\n\t\t\t1,\n\t\t\t2 | 5,\n\t\t\t3,\n\t\t\t.. # After DoubleDot\n\t\t\t\tas # Before alias\n\t\t\t\t\trest, # After last pattern in list\n\t\t] => 123\n\t\t3.14 => 314\n\t\t3.14 | 6.28 => 314\n\t\t(1, 2, 3) => 123\n\t\t(1, 2 | 5, 3) => 123\n\t\t{ foo: 1, bar: 2, ..rest } => 12->add(34)\n\t\t{ # After pattern record open\n\t\t\tfoo # After pattern record field name\n\t\t\t\t: # Before pattern record field value\n\t\t\t\t\t1, # After pattern record field\n\t\t\tbar: 2,\n\t\t\t.. # After spread operator\n\t\t\t\trest, # After last field\n\t\t} => 12\n\t\t{ foo: 1, bar: 2 | 7 } => 12\n\t\t{\n\t\t\tfoo: 1,\n\t\t\tbar: 2 | 7, # After last record field\n\t\t} => 12\n\t\tOk(123) => 123\n\t\tOk(Some(dude)) => dude\n\t\tTwoArgs(\"hello\", Some(\"world\")) => 1000\n\t}"))
				(underline (start 102 3) (end 102 24) (annotation error)))
			(line-break)
			(reflow "This")
			(reflow " ")
			(reflow "sixth")
			(reflow " ")
			(reflow "branch is trying to match:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(d)")
			(line-break)
			(indent 1)
			(text "  where [")
			(line-break)
			(indent 1)
			(text "    d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]),")
			(line-break)
			(indent 1)
			(text "    d.is_eq : d, d -> Bool,")
			(line-break)
			(indent 1)
			(text "  ]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the expression between the")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "parenthesis has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Blue, Green, Red, ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "These can never match! Either the pattern or expression has a problem.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 178 47) (end 178 71))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 178 47) (end 178 71) (annotation error) (line-text "\trecord = { foo: 123, bar: \"Hello\", ;az: tag, qux: Ok(world), punned }"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Too Few Args")
		(region (start 155 2) (end 157 3))
		(headline
			(reflow "The")
			(reflow " ")
			(annotated code "match_time")
			(reflow " function expects")
			(reflow " ")
			(reflow "2")
			(reflow " ")
			(reflow "arguments")
			(reflow ",")
			(reflow " ")
			(reflow "but it got")
			(reflow " ")
			(reflow "1")
			(reflow " ")
			(reflow "instead."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 155 2) (end 157 3) (annotation error) (line-text "\tmatch_time(\n\t\t..., # Single args with comment\n\t)"))
			(line-break)
			(reflow "The")
			(reflow " ")
			(annotated code "match_time")
			(reflow " function has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Blue, Green, Red, ..], _arg -> d")
			(line-break)
			(indent 1)
			(text "  where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Are there any missing commas?")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 167 3) (end 170 4))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument being passed to this function has the wrong type."))
		(document
			(source-underlines
				(display (file "fuzz_crash_023.md") (start 167 3) (end 170 4) (annotation dim) (line-text "\t\tadd_one(\n\t\t\tdbg # After dbg in list\n\t\t\t\tnumber, # after dbg expr as arg\n\t\t), # Comment one"))
				(underline (start 168 4) (end 169 11) (annotation error)))
			(line-break)
			(reflow "This argument has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{}")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But")
			(reflow " ")
			(annotated code "add_one")
			(reflow " ")
			(reflow "needs the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U64")
			(annotation-end)))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 178 47) (end 178 71))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 178 47) (end 178 71) (annotation error) (line-text "\trecord = { foo: 123, bar: \"Hello\", ;az: tag, qux: Ok(world), punned }"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 175 26) (end 175 27))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 175 26) (end 175 27) (annotation error) (line-text "\t\tStdout.line!(\"Adding ${n} to ${number}\")"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Dec")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But you are trying to use it as:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 175 34) (end 175 40))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 175 34) (end 175 40) (annotation error) (line-text "\t\tStdout.line!(\"Adding ${n} to ${number}\")"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Dec")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But you are trying to use it as:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end)))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 201 1) (end 201 25))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 201 1) (end 201 25) (annotation error) (line-text "tuple : Value((a, b, c))"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 189 26) (end 189 40))
		(headline
			(reflow "This is trying to dispatch a method named")
			(reflow " ")
			(annotated code "static_dispatch_method")
			(reflow " ")
			(reflow "on an unresolved type variable, but unresolved type variables have no methods."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 189 26) (end 189 40) (annotation error) (line-text "\tstatic_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?"))
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "You can replace this static dispatch call with an ordinary function call, or force the type variable to become more concrete—for example, by adding a type annotation that narrows its type to something that actually has methods.")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 189 26) (end 189 66))
		(headline
			(reflow "This is trying to dispatch a method named")
			(reflow " ")
			(annotated code "next_static_dispatch_method")
			(reflow " ")
			(reflow "on an unresolved type variable, but unresolved type variables have no methods."))
		(document
			(source-region (file "fuzz_crash_023.md") (start 189 26) (end 189 66) (annotation error) (line-text "\tstatic_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?"))
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "You can replace this static dispatch call with an ordinary function call, or force the type variable to become more concrete—for example, by adding a type annotation that narrows its type to something that actually has methods."))))
~~~
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
		(s-import (raw "pf # Comment after qualifier
		.StdoutMultiline")
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
							(p-string (raw """)
								(p-string-text (raw "foo")))
							(e-int (raw "100")))
						(branch
							(p-alternatives
								(p-string (raw """)
									(p-string-text (raw "foo")))
								(p-string (raw """)
									(p-string-text (raw "bar"))))
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
							(e-arrow-call
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
								(p-string (raw """)
									(p-string-text (raw "hello")))
								(p-tag (raw "Some")
									(p-string (raw """)
										(p-string-text (raw "world")))))
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
					(ty (name "Try"))
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
							(e-malformed (reason "expr_dot_suffix_not_allowed")))
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
							(e-malformed (reason "expected_expr_record_field_name")))
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
							(e-question-suffix
								(e-field-access
									(receiver
										(e-question-suffix
											(e-method-call (method ".next_static_dispatch_method")
												(receiver
													(e-question-suffix
														(e-method-call (method ".static_dispatch_method")
															(receiver
																(e-question-suffix
																	(e-apply
																		(e-ident (raw "some_fn"))
																		(e-ident (raw "arg1")))))
															(args))))
												(args))))
									(segment (mode "required") (field "record_field")))))
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
# This is a mod comment!
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
		as GoodNameMultiline

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
		{ foo: 1, bar: 2, ..rest } => 12 |> add(34)
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

main! : List(String) -> Try({}, _)
main! = |_| { # Yeah I can leave a comment here
	world = "World"
	var number = 123
	expect blah == 1
	tag = Blue
	return # Comment after return statement

	# Just a random comment!

		
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
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "match_time"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-runtime-error (tag "erroneous_value_expr"))))
	(d-let
		(p-assign (ident "qux"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "main!"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-malformed))
				(ty-apply (name "Try") (builtin)
					(ty-record)
					(ty-underscore)))))
	(d-let
		(p-assign (ident "empty"))
		(e-empty_record)
		(annotation
			(ty-record)))
	(d-let
		(p-assign (ident "tuple"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed)))
	(s-import (mod "pf.Stdout")
		(exposes
			(exposed (name "line!") (wildcard false))
			(exposed (name "write!") (wildcard false))))
	(s-import (mod "pf.StdoutMultiline")
		(exposes
			(exposed (name "line!") (wildcard false))
			(exposed (name "write!") (wildcard false))))
	(s-import (mod "pkg.Something")
		(exposes
			(exposed (name "func") (alias "function") (wildcard false))
			(exposed (name "Type") (alias "ValueCategory") (wildcard false))
			(exposed (name "Custom") (wildcard true))))
	(s-import (mod "BadName")
		(exposes))
	(s-import (mod "BadNameMultiline")
		(exposes))
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
	(s-expect
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-expect
		(e-block
			(s-let
				(p-assign (ident "foo"))
				(e-num (value "1")))
			(s-let
				(p-assign (ident "blah"))
				(e-num (value "1")))
			(e-method-eq (negated "false")
				(lhs
					(e-lookup-local
						(p-assign (ident "blah"))))
				(rhs
					(e-lookup-local
						(p-assign (ident "foo"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Bool -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "U64 -> U64"))
		(patt (type "[Blue, Green, Red, ..], _arg -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "Error"))
		(patt (type "List(Error) -> Try({}, _d)"))
		(patt (type "{}"))
		(patt (type "Error")))
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
		(alias (type "Error")
			(ty-header (name "Foo")))
		(alias (type "Error")
			(ty-header (name "FooMultiline")))
		(alias (type "Error")
			(ty-header (name "Some")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Error")
			(ty-header (name "SomeMl")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Error")
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
		(expr (type "Bool -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "U64 -> U64"))
		(expr (type "[Blue, Green, Red, ..], _arg -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "Error"))
		(expr (type "List(Error) -> Try({}, _d)"))
		(expr (type "{}"))
		(expr (type "Error"))))
~~~
