# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
# Thnt!
app [main!] { pf: platform "c" }

import pf.Stdout exposing [line!, e!]

import Stdot
		exposing [ #tem
		] # Cose

import pkg.S exposing [func as fry, Custom.*]

import Bae as Gooe
import
	Ba
Map(a, b) : List(a), (a -> b) -> List(b)
MapML( # Cere
	a, # Anre
	b,
) # Ag
	: # Aon
		List( #rg
		),
		(a -> b) -> # row
			List(			b	) #

Foo : (Bar, Baz)

line : ( # Cpen
	Bar, #
	Baz, #m
) # Co
Some(a) : { foo : Ok(a), bar : g }
Ml(a) : { # d
	bar : Som# Afld
}

Soine(a) : { #d
	bar : Som
} #
Maya) : [ #
] #se

Func(a) : Maybe(a), a -> Maybe(a)

ane = |num| if num 2 else 5

add_one : U64 -> U64
add_one = |num| {
	other = 1
	if num {
		dbg # bug
() #r
		0
	} else {
		dbg 123
		other
	}
}

match_time = |
	a, #rg
	b,
| # As
	match a {lue | Red => {
			x x
		}
		Blue		=> 1
		"foo" => # ent
00
		"foo" | "bar" => 20[1, 2, 3, .. as rest] # Aftet
			=> ment


		[1, 2 | 5, 3, .. as rest] => 123
		[
ist
		] => 123
		3.14 => 314
		3.14 | 6.28 => 314
		(1, 2, 3) => 123
		(1, 2 | 5, 3) => 123
		{ foo: 1, bar: 2, ..rest } => 12->add(34)
		{ # Afrd open
			foo #
				: #ue
					1, # Aftd field
			bar: 2,
			..} => 12
		{ foo: 1, bar: 2 | 7 } => 12
		{
			foo: 1,
			} => 12
		Ok(123) => 121000
	}

expect # Commeneyword
	blah == 1 # Commnt

main! : List(String) -> Try({}, _)
main! = |_| { # Yeah Ie
	world = "World"
	var number = 123
	expect blah == 1
	tag = Blue
	return # Comd
		tag

	# Jusnt!

	...
	match_time(
		..., #
	)
	some_func(
		dbg # bug
			42, # Aft expr
	)
	crash "Unreachtement
	tag_with = Ok(number)
	ited = "Hello, ${world}"
	list = [
		add_one(
			dbg # Afin list
e[, # afarg
		),	456, # ee
	]
	for n in list {
	line!("Adding ${n} to ${number}")
		number = number + n
	}
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
	m_tuple = (
		123,
		"World",
		tag1,
		Ok(world), # Thisnt
		(nested, tuple),
		[1, 2, 3],
	)
	bsult = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
	Stdoline!(
		"How about ${ #
			Num.toStr(number) # on expr
		} as a",
	)
} # Commenl decl

empty : {}
empty = {}

tuple : Value((a, b, c))

expect {
	foo = 1 # Thio
	blah = 1
	blah == foo
}
~~~
# EXPECTED
LEADING ZERO - :0:0:0:0
UNCLOSED STRING - fuzz_crash_027.md:118:8:118:22
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_027.md:40:5:40:6
UNEXPECTED STATEMENT - fuzz_crash_027.md:40:7:40:8
UNEXPECTED STATEMENT - fuzz_crash_027.md:40:9:40:10
UNEXPECTED STATEMENT - fuzz_crash_027.md:41:1:41:2
EXPECTED RECORD ACCESSOR - fuzz_crash_027.md:110:2:110:5
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_027.md:124:3:124:4
EXPECTED LIST SEPARATOR - fuzz_crash_027.md:125:3:125:4
EXPECTED CALL ARGUMENT END - fuzz_crash_027.md:126:2:126:3
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_027.md:131:9:131:10
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_027.md:132:8:132:9
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_027.md:133:10:133:11
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_027.md:141:8:141:9
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_027.md:142:8:142:9
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_027.md:148:1:148:2
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_027.md:150:7:150:8
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_027.md:151:7:151:8
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_027.md:153:7:153:8
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_027.md:155:1:155:7
EXPECTED LIST SEPARATOR - fuzz_crash_027.md:160:1:160:1
EXPECTED CLOSING BRACE - fuzz_crash_027.md:160:1:160:1
MOD NOT FOUND - fuzz_crash_027.md:6:1:8:4
MOD NOT FOUND - fuzz_crash_027.md:12:1:12:19
MOD NOT FOUND - fuzz_crash_027.md:13:1:14:4
UNDECLARED TYPE - fuzz_crash_027.md:26:8:26:11
UNDECLARED TYPE - fuzz_crash_027.md:26:13:26:16
UNDECLARED TYPE - fuzz_crash_027.md:29:2:29:5
UNDECLARED TYPE - fuzz_crash_027.md:30:2:30:5
UNDECLARED TYPE - fuzz_crash_027.md:32:19:32:21
UNDECLARED TYPE VARIABLE - fuzz_crash_027.md:32:32:32:33
UNDECLARED TYPE - fuzz_crash_027.md:34:8:34:11
UNDECLARED TYPE - fuzz_crash_027.md:38:8:38:11
UNDECLARED TYPE - fuzz_crash_027.md:43:11:43:16
UNDECLARED TYPE - fuzz_crash_027.md:43:26:43:31
EMPTY TUPLE NOT ALLOWED - fuzz_crash_027.md:52:1:52:3
NAME NOT IN SCOPE - fuzz_crash_027.md:65:4:65:5
NAME NOT IN SCOPE - fuzz_crash_027.md:65:6:65:7
NAME NOT IN SCOPE - fuzz_crash_027.md:71:7:71:11
UNUSED VARIABLE - fuzz_crash_027.md:1:1:1:1
NOT IMPLEMENTED - fuzz_crash_027.md:74:7:74:12
UNUSED VARIABLE - fuzz_crash_027.md:1:1:1:1
UNUSED VARIABLE - fuzz_crash_027.md:76:1:76:4
NOT IMPLEMENTED - fuzz_crash_027.md:81:7:81:12
NAME NOT IN SCOPE - fuzz_crash_027.md:82:37:82:40
UNUSED VARIABLE - fuzz_crash_027.md:82:21:82:27
NOT IMPLEMENTED - fuzz_crash_027.md:89:18:89:23
UNUSED VARIABLE - fuzz_crash_027.md:62:2:62:3
NAME NOT IN SCOPE - fuzz_crash_027.md:97:2:97:6
UNDECLARED TYPE - fuzz_crash_027.md:99:14:99:20
NAME NOT IN SCOPE - fuzz_crash_027.md:103:9:103:13
UNRECOGNIZED SYNTAX - fuzz_crash_027.md:110:2:110:5
NAME NOT IN SCOPE - fuzz_crash_027.md:114:2:114:11
UNRECOGNIZED SYNTAX - fuzz_crash_027.md:1:1:1:1
UNUSED VARIABLE - fuzz_crash_027.md:104:2:104:5
UNUSED VARIABLE - fuzz_crash_027.md:119:2:119:10
UNUSED VARIABLE - fuzz_crash_027.md:120:2:120:6
UNUSED VARIABLE - fuzz_crash_027.md:121:2:121:6
TOO FEW ARGS - fuzz_crash_027.md:21:3:22:4
DECLARATION HAS NO VALUE - fuzz_crash_027.md:28:1:31:2
TYPE MISMATCH - fuzz_crash_027.md:50:5:50:8
TYPE MISMATCH - fuzz_crash_027.md:64:2:64:2
MISSING METHOD - fuzz_crash_027.md:68:3:68:8
MISSING METHOD - fuzz_crash_027.md:70:3:70:8
TYPE MISMATCH - fuzz_crash_027.md:64:2:64:2
TOO FEW ARGS - fuzz_crash_027.md:111:2:113:3
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Leading Zero")
		(headline
			(reflow "Numbers cannot have leading zeros."))
		(document))
	(report
		(severity runtime_error)
		(title "Unclosed String")
		(region (start 118 8) (end 118 22))
		(headline
			(reflow "This string is missing a closing quote."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 118 8) (end 118 22) (annotation error) (line-text "\tcrash \"Unreachtement"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 40 5) (end 40 6))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ")")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 40 5) (end 40 6) (annotation error) (line-text "Maya) : [ #"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 40 7) (end 40 8))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ":")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 40 7) (end 40 8) (annotation error) (line-text "Maya) : [ #"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 40 9) (end 40 10))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 40 9) (end 40 10) (annotation error) (line-text "Maya) : [ #"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 41 1) (end 41 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 41 1) (end 41 2) (annotation error) (line-text "] #se"))))
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 110 2) (end 110 5))
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
			(source-region (file "fuzz_crash_027.md") (start 110 2) (end 110 5) (annotation error) (line-text "\t..."))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 124 3) (end 124 4))
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
			(source-region (file "fuzz_crash_027.md") (start 124 3) (end 124 4) (annotation error) (line-text "e[, # afarg"))))
	(report
		(severity runtime_error)
		(title "Expected List Separator")
		(region (start 125 3) (end 125 4))
		(headline
			(reflow "I was parsing a list expression, and I expected `,` or `]`."))
		(document
			(reflow "Separate list elements with commas and close the list with ")
			(annotated code "]")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[1, 2, 3]")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ")")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 125 3) (end 125 4) (annotation error) (line-text "\t\t),\t456, # ee"))))
	(report
		(severity runtime_error)
		(title "Expected Call Argument End")
		(region (start 126 2) (end 126 3))
		(headline
			(reflow "I was parsing function or method call arguments, and I expected `)`."))
		(document
			(reflow "Function call arguments go inside parentheses and are separated with commas.")
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
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 126 2) (end 126 3) (annotation error) (line-text "\t]"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 131 9) (end 131 10))
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
			(annotated code "=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 131 9) (end 131 10) (annotation error) (line-text "\trecord = { foo: 123, bar: \"Hello\", baz: tag, qux: Ok(world), punned }"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 132 8) (end 132 9))
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
			(annotated code "=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 132 8) (end 132 9) (annotation error) (line-text "\ttuple = (123, \"World\", tag, Ok(world), (nested, tuple), [1, 2, 3])"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 133 10) (end 133 11))
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
			(annotated code "=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 133 10) (end 133 11) (annotation error) (line-text "\tm_tuple = ("))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 141 8) (end 141 9))
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
			(annotated code "=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 141 8) (end 141 9) (annotation error) (line-text "\tbsult = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 142 8) (end 142 9))
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
			(annotated code "=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 142 8) (end 142 9) (annotation error) (line-text "\tstale = some_fn(arg1)?.statod()?.ned()?.recd?"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 148 1) (end 148 2))
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
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 148 1) (end 148 2) (annotation error) (line-text "} # Commenl decl"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 150 7) (end 150 8))
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
			(annotated code ":")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 150 7) (end 150 8) (annotation error) (line-text "empty : {}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 151 7) (end 151 8))
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
			(annotated code "=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 151 7) (end 151 8) (annotation error) (line-text "empty = {}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 153 7) (end 153 8))
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
			(annotated code ":")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 153 7) (end 153 8) (annotation error) (line-text "tuple : Value((a, b, c))"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 155 1) (end 155 7))
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
			(annotated code "expect")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 155 1) (end 155 7) (annotation error) (line-text "expect {"))))
	(report
		(severity runtime_error)
		(title "Expected List Separator")
		(region (start 160 1) (end 160 1))
		(headline
			(reflow "I was parsing a list expression, and I expected `,` or `]`."))
		(document
			(reflow "Separate list elements with commas and close the list with ")
			(annotated code "]")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[1, 2, 3]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 160 1) (end 160 1) (annotation error) (line-text ""))))
	(report
		(severity runtime_error)
		(title "Expected Closing Brace")
		(region (start 160 1) (end 160 1))
		(headline
			(reflow "I was parsing a block expression, and I expected `}` before the file ended."))
		(document
			(reflow "Close the block after its final statement or expression.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{")
			(line-break)
			(indent 1)
			(text "    answer = 42")
			(line-break)
			(indent 1)
			(text "    answer")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 160 1) (end 160 1) (annotation error) (line-text ""))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 6 1) (end 8 4))
		(headline
			(text "The mod ")
			(annotated code "Stdot")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 6 1) (end 8 4) (annotation error) (line-text "import Stdot\n\t\texposing [ #tem\n\t\t] # Cose"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 12 1) (end 12 19))
		(headline
			(text "The mod ")
			(annotated code "Bae")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 12 1) (end 12 19) (annotation error) (line-text "import Bae as Gooe"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 13 1) (end 14 4))
		(headline
			(text "The mod ")
			(annotated code "Ba")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 13 1) (end 14 4) (annotation error) (line-text "import\n\tBa"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 26 8) (end 26 11))
		(headline
			(reflow "The type ")
			(annotated code "Bar")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 26 8) (end 26 11) (annotation error) (line-text "Foo : (Bar, Baz)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 26 13) (end 26 16))
		(headline
			(reflow "The type ")
			(annotated code "Baz")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 26 13) (end 26 16) (annotation error) (line-text "Foo : (Bar, Baz)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 29 2) (end 29 5))
		(headline
			(reflow "The type ")
			(annotated code "Bar")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 29 2) (end 29 5) (annotation error) (line-text "\tBar, #"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 30 2) (end 30 5))
		(headline
			(reflow "The type ")
			(annotated code "Baz")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 30 2) (end 30 5) (annotation error) (line-text "\tBaz, #m"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 32 19) (end 32 21))
		(headline
			(reflow "The type ")
			(annotated code "Ok")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 32 19) (end 32 21) (annotation error) (line-text "Some(a) : { foo : Ok(a), bar : g }"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type Variable")
		(region (start 32 32) (end 32 33))
		(headline
			(reflow "The type variable ")
			(annotated code "g")
			(reflow " is not declared in this scope."))
		(document
			(reflow "Type variables must be introduced in a type annotation before they can be used.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 32 32) (end 32 33) (annotation error) (line-text "Some(a) : { foo : Ok(a), bar : g }"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 34 8) (end 34 11))
		(headline
			(reflow "The type ")
			(annotated code "Som")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 34 8) (end 34 11) (annotation error) (line-text "\tbar : Som# Afld"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 38 8) (end 38 11))
		(headline
			(reflow "The type ")
			(annotated code "Som")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 38 8) (end 38 11) (annotation error) (line-text "\tbar : Som"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 43 11) (end 43 16))
		(headline
			(reflow "The type ")
			(annotated code "Maybe")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 43 11) (end 43 16) (annotation error) (line-text "Func(a) : Maybe(a), a -> Maybe(a)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 43 26) (end 43 31))
		(headline
			(reflow "The type ")
			(annotated code "Maybe")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 43 26) (end 43 31) (annotation error) (line-text "Func(a) : Maybe(a), a -> Maybe(a)"))))
	(report
		(severity runtime_error)
		(title "Empty Tuple Not Allowed")
		(region (start 52 1) (end 52 3))
		(headline
			(reflow "I am part way through parsing this tuple, but it is empty."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 52 1) (end 52 3) (annotation error) (line-text "() #r"))
			(line-break)
			(reflow "If you want to represent nothing, try using an empty record: ")
			(annotated code "{}")
			(reflow ".")))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 65 4) (end 65 5))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "x")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 65 4) (end 65 5) (annotation error) (line-text "\t\t\tx x"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 65 6) (end 65 7))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "x")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 65 6) (end 65 7) (annotation error) (line-text "\t\t\tx x"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 71 7) (end 71 11))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "ment")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 71 7) (end 71 11) (annotation error) (line-text "\t\t\t=> ment"))))
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
			(source-region (file "fuzz_crash_027.md") (start 1 1) (end 1 1) (annotation error) (line-text "# Thnt!"))))
	(report
		(severity fatal)
		(title "Not Implemented")
		(region (start 74 7) (end 74 12))
		(headline
			(reflow "This feature is not yet implemented: ")
			(annotation-start emphasis)
			(text "alternatives pattern outside match expression")
			(annotation-end)
			(reflow "."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 74 7) (end 74 12) (annotation error) (line-text "\t\t[1, 2 | 5, 3, .. as rest] => 123"))
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
			(source-region (file "fuzz_crash_027.md") (start 1 1) (end 1 1) (annotation error) (line-text "# Thnt!"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 76 1) (end 76 4))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "ist")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_ist")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 76 1) (end 76 4) (annotation error) (line-text "ist"))))
	(report
		(severity fatal)
		(title "Not Implemented")
		(region (start 81 7) (end 81 12))
		(headline
			(reflow "This feature is not yet implemented: ")
			(annotation-start emphasis)
			(text "alternatives pattern outside match expression")
			(annotation-end)
			(reflow "."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 81 7) (end 81 12) (annotation error) (line-text "\t\t(1, 2 | 5, 3) => 123"))
			(line-break)
			(reflow "This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!")
			(line-break)))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 82 37) (end 82 40))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "add")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 82 37) (end 82 40) (annotation error) (line-text "\t\t{ foo: 1, bar: 2, ..rest } => 12->add(34)"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 82 21) (end 82 27))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "rest")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_rest")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 82 21) (end 82 27) (annotation error) (line-text "\t\t{ foo: 1, bar: 2, ..rest } => 12->add(34)"))))
	(report
		(severity fatal)
		(title "Not Implemented")
		(region (start 89 18) (end 89 23))
		(headline
			(reflow "This feature is not yet implemented: ")
			(annotation-start emphasis)
			(text "alternatives pattern outside match expression")
			(annotation-end)
			(reflow "."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 89 18) (end 89 23) (annotation error) (line-text "\t\t{ foo: 1, bar: 2 | 7 } => 12"))
			(line-break)
			(reflow "This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!")
			(line-break)))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 62 2) (end 62 3))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "b")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_b")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 62 2) (end 62 3) (annotation error) (line-text "\tb,"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 97 2) (end 97 6))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "blah")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 97 2) (end 97 6) (annotation error) (line-text "\tblah == 1 # Commnt"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 99 14) (end 99 20))
		(headline
			(reflow "The type ")
			(annotated code "String")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 99 14) (end 99 20) (annotation error) (line-text "main! : List(String) -> Try({}, _)"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 103 9) (end 103 13))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "blah")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 103 9) (end 103 13) (annotation error) (line-text "\texpect blah == 1"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 110 2) (end 110 5))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 110 2) (end 110 5) (annotation error) (line-text "\t..."))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 114 2) (end 114 11))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "some_func")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 114 2) (end 114 11) (annotation error) (line-text "\tsome_func("))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 1 1) (end 1 1))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 1 1) (end 1 1) (annotation error) (line-text "# Thnt!"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo.")))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 104 2) (end 104 5))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "tag")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_tag")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 104 2) (end 104 5) (annotation error) (line-text "\ttag = Blue"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 119 2) (end 119 10))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "tag_with")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_tag_with")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 119 2) (end 119 10) (annotation error) (line-text "\ttag_with = Ok(number)"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 120 2) (end 120 6))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "ited")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_ited")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 120 2) (end 120 6) (annotation error) (line-text "\tited = \"Hello, ${world}\""))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 121 2) (end 121 6))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "list")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_list")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_027.md") (start 121 2) (end 121 6) (annotation error) (line-text "\tlist = ["))))
	(report
		(severity runtime_error)
		(title "Too Few Args")
		(region (start 21 3) (end 22 4))
		(headline
			(reflow "The type")
			(reflow " ")
			(annotated type "List")
			(reflow " ")
			(reflow "expects")
			(reflow " ")
			(reflow "1")
			(reflow " ")
			(reflow "argument,")
			(reflow " ")
			(reflow "but got")
			(reflow " ")
			(reflow "0")
			(reflow " ")
			(reflow "instead."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 21 3) (end 22 4) (annotation error) (line-text "\t\tList( #rg\n\t\t),"))))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 28 1) (end 31 2))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 28 1) (end 31 2) (annotation error) (line-text "line : ( # Cpen\n\tBar, #\n\tBaz, #m\n) # Co"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 50 5) (end 50 8))
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
			(source-region (file "fuzz_crash_027.md") (start 50 5) (end 50 8) (annotation error) (line-text "\tif num {"))
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
		(title "Type Mismatch")
		(region (start 64 2) (end 94 3))
		(headline
			(reflow "The")
			(reflow " ")
			(annotated code "lue")
			(reflow " ")
			(reflow "binding in the")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "pattern of the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "does not match the same binding in the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "pattern."))
		(document
			(source-underlines
				(display (file "fuzz_crash_027.md") (start 64 2) (end 94 3) (annotation dim) (line-text "\tmatch a {lue | Red => {\n\t\t\tx x\n\t\t}\n\t\tBlue\t\t=> 1\n\t\t\"foo\" => # ent\n00\n\t\t\"foo\" | \"bar\" => 20[1, 2, 3, .. as rest] # Aftet\n\t\t\t=> ment\n\n\n\t\t[1, 2 | 5, 3, .. as rest] => 123\n\t\t[\nist\n\t\t] => 123\n\t\t3.14 => 314\n\t\t3.14 | 6.28 => 314\n\t\t(1, 2, 3) => 123\n\t\t(1, 2 | 5, 3) => 123\n\t\t{ foo: 1, bar: 2, ..rest } => 12->add(34)\n\t\t{ # Afrd open\n\t\t\tfoo #\n\t\t\t\t: #ue\n\t\t\t\t\t1, # Aftd field\n\t\t\tbar: 2,\n\t\t\t..} => 12\n\t\t{ foo: 1, bar: 2 | 7 } => 12\n\t\t{\n\t\t\tfoo: 1,\n\t\t\t} => 12\n\t\tOk(123) => 121000\n\t}"))
				(underline (start 64 17) (end 64 20) (annotation error)))
			(line-break)
			(reflow "In the")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "pattern,")
			(reflow " ")
			(annotated code "lue")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Red, ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But in the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "pattern,")
			(reflow " ")
			(annotated code "lue")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Red, ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "A name shared across")
			(reflow " ")
			(annotated code "|")
			(reflow " ")
			(reflow "patterns in the same")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "branch must have one compatible type.")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 68 3) (end 68 8))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_quote")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 68 3) (end 68 8) (annotation error) (line-text "\t\t\"foo\" => # ent"))
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
			(text "[Blue, Red, ..]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 70 3) (end 70 8))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_quote")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "fuzz_crash_027.md") (start 70 3) (end 70 8) (annotation error) (line-text "\t\t\"foo\" | \"bar\" => 20[1, 2, 3, .. as rest] # Aftet"))
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
			(text "[Blue, Red, ..]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 64 2) (end 94 3))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "fifth")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "does not match the previous ones."))
		(document
			(source-underlines
				(display (file "fuzz_crash_027.md") (start 64 2) (end 94 3) (annotation dim) (line-text "\tmatch a {lue | Red => {\n\t\t\tx x\n\t\t}\n\t\tBlue\t\t=> 1\n\t\t\"foo\" => # ent\n00\n\t\t\"foo\" | \"bar\" => 20[1, 2, 3, .. as rest] # Aftet\n\t\t\t=> ment\n\n\n\t\t[1, 2 | 5, 3, .. as rest] => 123\n\t\t[\nist\n\t\t] => 123\n\t\t3.14 => 314\n\t\t3.14 | 6.28 => 314\n\t\t(1, 2, 3) => 123\n\t\t(1, 2 | 5, 3) => 123\n\t\t{ foo: 1, bar: 2, ..rest } => 12->add(34)\n\t\t{ # Afrd open\n\t\t\tfoo #\n\t\t\t\t: #ue\n\t\t\t\t\t1, # Aftd field\n\t\t\tbar: 2,\n\t\t\t..} => 12\n\t\t{ foo: 1, bar: 2 | 7 } => 12\n\t\t{\n\t\t\tfoo: 1,\n\t\t\t} => 12\n\t\tOk(123) => 121000\n\t}"))
				(underline (start 70 22) (end 70 43) (annotation error)))
			(line-break)
			(reflow "This")
			(reflow " ")
			(reflow "fifth")
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
			(text "[Blue, Red, ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "These can never match! Either the pattern or expression has a problem.")))
	(report
		(severity runtime_error)
		(title "Too Few Args")
		(region (start 111 2) (end 113 3))
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
			(source-region (file "fuzz_crash_027.md") (start 111 2) (end 113 3) (annotation error) (line-text "\tmatch_time(\n\t\t..., #\n\t)"))
			(line-break)
			(reflow "The")
			(reflow " ")
			(annotated code "match_time")
			(reflow " function has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Blue, Red, ..], _arg -> Error")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Are there any missing commas?"))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,
KwImport,UpperIdent,
KwExposing,OpenSquare,
CloseSquare,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,LowerIdent,KwAs,LowerIdent,Comma,UpperIdent,DotStar,CloseSquare,
KwImport,UpperIdent,KwAs,UpperIdent,
KwImport,
UpperIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,
OpColon,
UpperIdent,NoSpaceOpenRound,
CloseRound,Comma,
OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,OpColon,OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpColon,OpenRound,
UpperIdent,Comma,
UpperIdent,Comma,
CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,
LowerIdent,OpColon,UpperIdent,
CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,
LowerIdent,OpColon,UpperIdent,
CloseCurly,
UpperIdent,CloseRound,OpColon,OpenSquare,
CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,Int,KwElse,Int,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,Int,
KwIf,LowerIdent,OpenCurly,
KwDbg,
OpenRound,CloseRound,
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
KwMatch,LowerIdent,OpenCurly,LowerIdent,OpBar,UpperIdent,OpFatArrow,OpenCurly,
LowerIdent,LowerIdent,
CloseCurly,
UpperIdent,OpFatArrow,Int,
StringStart,StringPart,StringEnd,OpFatArrow,
Int,
StringStart,StringPart,StringEnd,OpBar,StringStart,StringPart,StringEnd,OpFatArrow,Int,OpenSquare,Int,Comma,Int,Comma,Int,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,
OpFatArrow,LowerIdent,
OpenSquare,Int,Comma,Int,OpBar,Int,Comma,Int,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,Int,
OpenSquare,
LowerIdent,
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
DoubleDot,CloseCurly,OpFatArrow,Int,
OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,OpBar,Int,CloseCurly,OpFatArrow,Int,
OpenCurly,
LowerIdent,OpColon,Int,Comma,
CloseCurly,OpFatArrow,Int,
UpperIdent,NoSpaceOpenRound,Int,CloseRound,OpFatArrow,Int,
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
KwCrash,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,OpAssign,OpenSquare,
LowerIdent,NoSpaceOpenRound,
KwDbg,
LowerIdent,OpenSquare,Comma,
CloseRound,Comma,Int,Comma,
CloseSquare,
KwFor,LowerIdent,KwIn,LowerIdent,OpenCurly,
LowerIdent,NoSpaceOpenRound,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,CloseCurly,
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
UpperIdent,NoSpaceOpenRound,
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
				(e-string-part (raw "c"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "c"))))))
	(statements
		(s-import (raw "pf.Stdout")
			(exposing
				(exposed-lower-ident
					(text "line!"))
				(exposed-lower-ident
					(text "e!"))))
		(s-import (raw "Stdot"))
		(s-import (raw "pkg.S")
			(exposing
				(exposed-lower-ident
					(text "func")
					(as "fry"))
				(exposed-upper-ident-star (text "Custom"))))
		(s-import (raw "Bae") (alias "Gooe"))
		(s-import (raw "Ba"))
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
					(ty (name "List")))
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
		(s-type-anno (name "line")
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
					(ty-var (raw "g")))))
		(s-type-decl
			(header (name "Ml")
				(args
					(ty-var (raw "a"))))
			(ty-record
				(anno-record-field (name "bar")
					(ty (name "Som")))))
		(s-type-decl
			(header (name "Soine")
				(args
					(ty-var (raw "a"))))
			(ty-record
				(anno-record-field (name "bar")
					(ty (name "Som")))))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "Func")
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
			(p-ident (raw "ane"))
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
										(e-tuple))
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
								(p-ident (raw "lue"))
								(p-tag (raw "Red")))
							(e-block
								(statements
									(e-ident (raw "x"))
									(e-ident (raw "x")))))
						(branch
							(p-tag (raw "Blue"))
							(e-int (raw "1")))
						(branch
							(p-string (raw """)
								(p-string-text (raw "foo")))
							(e-int (raw "00")))
						(branch
							(p-alternatives
								(p-string (raw """)
									(p-string-text (raw "foo")))
								(p-string (raw """)
									(p-string-text (raw "bar"))))
							(e-int (raw "20")))
						(branch
							(p-list
								(p-int (raw "1"))
								(p-int (raw "2"))
								(p-int (raw "3"))
								(p-list-rest (name "rest")))
							(e-ident (raw "ment")))
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
								(p-ident (raw "ist")))
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
								(field (rest true)))
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
									(p-int (raw "1"))))
							(e-int (raw "12")))
						(branch
							(p-tag (raw "Ok")
								(p-int (raw "123")))
							(e-int (raw "121000")))))))
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
								(e-string-part (raw "Unreachtement"))))
						(s-decl
							(p-ident (raw "tag_with"))
							(e-apply
								(e-tag (raw "Ok"))
								(e-ident (raw "number"))))
						(s-decl
							(p-ident (raw "ited"))
							(e-string
								(e-string-part (raw "Hello, "))
								(e-ident (raw "world"))
								(e-string-part (raw ""))))
						(s-decl
							(p-ident (raw "list"))
							(e-malformed (reason "expected_expr_close_square_or_comma")))))))))
~~~
# FORMATTED
~~~roc
# Thnt!
app [main!] { pf: platform "c" }

import pf.Stdout exposing [line!, e!]

import Stdot # Cose

import pkg.S exposing [func as fry, Custom.*]

import Bae as Gooe
import
	Ba
Map(a, b) : List(a), (a -> b) -> List(b)

MapML( # Cere
	a, # Anre
	b,
) # Ag
	: # Aon
		List( # rg
		),
		(a -> b) -> # row
			List(b) #

Foo : (Bar, Baz)

line : ( # Cpen
	Bar, #
	Baz, # m
) # Co

Some(a) : { foo : Ok(a), bar : g }

Ml(a) : { # d
	bar : Som, # Afld
}

Soine(a) : { # d
	bar : Som,
}
#
# se

Func(a) : Maybe(a), a -> Maybe(a)

ane = |num| if num 2 else 5

add_one : U64 -> U64
add_one = |num| {
	other = 1
	if num {
		dbg # bug
			() # r
		0
	} else {
		dbg 123
		other
	}
}

match_time = |
	a, # rg
	b,
| # As
	match a {
		lue | Red => {
			x
			x
		}
		Blue => 1
		"foo" => # ent
			00
		"foo" | "bar" => 20
		[1, 2, 3, .. as rest] # Aftet
			=> ment

		[1, 2 | 5, 3, .. as rest] => 123
		[ist] => 123
		3.14 => 314
		3.14 | 6.28 => 314
		(1, 2, 3) => 123
		(1, 2 | 5, 3) => 123
		{ foo: 1, bar: 2, ..rest } => 12 |> add(34)
		{ # Afrd open
			foo #
				: # ue
					1, # Aftd field
			bar: 2,
			..,
		} => 12
		{ foo: 1, bar: 2 | 7 } => 12
		{
			foo: 1,
		} => 12
		Ok(123) => 121000
	}

expect # Commeneyword
	blah == 1 # Commnt

main! : List(String) -> Try({}, _)
main! = |_| { # Yeah Ie
	world = "World"
	var number = 123
	expect blah == 1
	tag = Blue
	return

	# Jusnt!

		
	match_time(
		..., #
	)
	some_func(
		dbg # bug
			42, # Aft expr
	)
	crash "Unreachtement"
	tag_with = Ok(number)
	ited = "Hello, ${world}"
	list =
		
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "line"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-tuple
				(ty-malformed)
				(ty-malformed))))
	(d-let
		(p-assign (ident "ane"))
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
		(e-runtime-error (tag "erroneous_value_expr")))
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
	(s-import (mod "pf.Stdout")
		(exposes
			(exposed (name "line!") (wildcard false))
			(exposed (name "e!") (wildcard false))))
	(s-import (mod "Stdot")
		(exposes))
	(s-import (mod "pkg.S")
		(exposes
			(exposed (name "func") (alias "fry") (wildcard false))
			(exposed (name "Custom") (wildcard true))))
	(s-import (mod "Bae")
		(exposes))
	(s-import (mod "Ba")
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
			(ty-apply (name "List") (builtin))
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
		(ty-header (name "Some")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-record
			(field (field "foo")
				(ty-malformed))
			(field (field "bar")
				(ty-malformed))))
	(s-alias-decl
		(ty-header (name "Ml")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-record
			(field (field "bar")
				(ty-malformed))))
	(s-alias-decl
		(ty-header (name "Soine")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-record
			(field (field "bar")
				(ty-malformed))))
	(s-alias-decl
		(ty-header (name "Func")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-fn (effectful false)
			(ty-malformed)
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))
			(ty-malformed)))
	(s-expect
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(Error, Error)"))
		(patt (type "Bool -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "U64 -> U64"))
		(patt (type "[Blue, Red, ..], _arg -> Error"))
		(patt (type "List(Error) -> Try({}, _d)")))
	(type_decls
		(alias (type "Map(a, b)")
			(ty-header (name "Map")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(alias (type "Error")
			(ty-header (name "MapML")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(alias (type "Error")
			(ty-header (name "Foo")))
		(alias (type "Error")
			(ty-header (name "Some")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Error")
			(ty-header (name "Ml")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Error")
			(ty-header (name "Soine")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Error")
			(ty-header (name "Func")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "(Error, Error)"))
		(expr (type "Bool -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "U64 -> U64"))
		(expr (type "[Blue, Red, ..], _arg -> Error"))
		(expr (type "List(Error) -> Try({}, _d)"))))
~~~
