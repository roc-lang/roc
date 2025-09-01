# META
~~~ini
description=A grab bag of all v0.1 syntax, heavily commented to show multiline formatting
type=file
~~~
# SOURCE
~~~roc
# This is a module comment!
app { pf: "../basic-cli/platform.roc" platform [main!] }

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
# TOKENS
~~~text
LineComment KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent OpBang Comma LowerIdent OpBang CloseSquare BlankLine KwImport LineComment LowerIdent LineComment Dot UpperIdent LineComment KwExposing OpenSquare LineComment LowerIdent OpBang Comma LineComment LowerIdent OpBang Comma LineComment CloseSquare LineComment BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent KwAs LowerIdent Comma UpperIdent KwAs UpperIdent Comma UpperIdent Dot OpStar CloseSquare BlankLine KwImport UpperIdent KwAs UpperIdent KwImport UpperIdent KwAs UpperIdent BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound LineComment LowerIdent Comma LineComment LowerIdent Comma CloseRound LineComment OpColon LineComment UpperIdent OpenRound LineComment LowerIdent Comma LineComment CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow LineComment UpperIdent OpenRound LineComment LowerIdent Comma CloseRound LineComment BlankLine UpperIdent OpColon OpenRound UpperIdent Comma UpperIdent CloseRound BlankLine UpperIdent OpColon OpenRound LineComment UpperIdent Comma LineComment UpperIdent Comma LineComment CloseRound LineComment BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon UpperIdent CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LineComment LowerIdent OpColon UpperIdent Comma LineComment CloseCurly BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LineComment LowerIdent LineComment OpColon LineComment UpperIdent OpenRound LowerIdent CloseRound Comma LineComment LowerIdent OpColon UpperIdent Comma LineComment CloseCurly LineComment BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare LineComment UpperIdent OpenRound LowerIdent CloseRound Comma LineComment UpperIdent Comma LineComment CloseSquare LineComment BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar KwIf LowerIdent Int KwElse Int BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign Int KwIf LowerIdent OpenCurly KwDbg LineComment LowerIdent OpenRound CloseRound LineComment Int CloseCurly KwElse OpenCurly KwDbg Int LowerIdent CloseCurly CloseCurly BlankLine LowerIdent OpAssign OpBar LowerIdent Comma LineComment LowerIdent Comma OpBar LineComment KwMatch LowerIdent OpenCurly UpperIdent OpBar UpperIdent OpBar UpperIdent OpFatArrow OpenCurly LowerIdent OpAssign Int LowerIdent CloseCurly UpperIdent LineComment OpBar LineComment UpperIdent OpBar UpperIdent LineComment OpFatArrow OpenCurly LowerIdent OpAssign Int LowerIdent CloseCurly LowerIdent LineComment OpFatArrow Int String OpFatArrow LineComment Int String OpBar String OpFatArrow Int OpenSquare Int Comma Int Comma Int Comma DoubleDot KwAs LowerIdent CloseSquare LineComment OpFatArrow LineComment Int LineComment BlankLine LineComment BlankLine OpenSquare Int Comma Int OpBar Int Comma Int Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow Int OpenSquare Int Comma Int OpBar Int Comma Int Comma DoubleDot LineComment KwAs LineComment LowerIdent Comma LineComment CloseSquare OpFatArrow Int Float OpFatArrow Int Float OpBar Float OpFatArrow Int OpenRound Int Comma Int Comma Int CloseRound OpFatArrow Int OpenRound Int Comma Int OpBar Int Comma Int CloseRound OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int Comma DoubleDot LowerIdent CloseCurly OpFatArrow Int OpArrow LowerIdent OpenRound Int CloseRound OpenCurly LineComment LowerIdent LineComment OpColon LineComment Int Comma LineComment LowerIdent OpColon Int Comma DoubleDot LineComment LowerIdent Comma LineComment CloseCurly OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int OpBar Int CloseCurly OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int OpBar Int Comma LineComment CloseCurly OpFatArrow Int UpperIdent OpenRound Int CloseRound OpFatArrow Int UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound OpFatArrow LowerIdent UpperIdent OpenRound String Comma UpperIdent OpenRound String CloseRound CloseRound OpFatArrow Int CloseCurly BlankLine KwExpect LineComment LowerIdent OpEquals Int LineComment BlankLine LowerIdent OpBang OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound OpenCurly CloseCurly Comma Underscore CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign String KwVar LowerIdent OpAssign Int KwExpect LowerIdent OpEquals Int LowerIdent OpAssign UpperIdent KwReturn LineComment LowerIdent LineComment BlankLine LineComment BlankLine TripleDot LowerIdent OpenRound TripleDot Comma LineComment CloseRound LowerIdent OpenRound KwDbg LineComment Int Comma LineComment CloseRound KwCrash LineComment String LineComment LowerIdent OpAssign UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign String LowerIdent OpAssign OpenSquare LowerIdent OpenRound KwDbg LineComment LowerIdent Comma LineComment CloseRound Comma LineComment Int Comma LineComment Int Comma LineComment CloseSquare KwFor LowerIdent KwIn LowerIdent OpenCurly UpperIdent Dot LowerIdent OpBang OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpPlus LowerIdent CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon String Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent CloseCurly LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent OpenRound LowerIdent CloseRound Comma LineComment OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare Comma CloseRound LowerIdent OpAssign UpperIdent OpenRound LowerIdent CloseRound OpDoubleQuestion Int OpGreaterThan Int OpStar Int OpOr Int OpPlus Int OpLessThan Int OpAnd Int OpBinaryMinus Int OpGreaterThanOrEq Int OpOr Int OpLessThanOrEq Int OpSlash Int LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpQuestion UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent CloseRound OpQuestion UpperIdent Dot LowerIdent OpBang OpenRound MalformedString UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LineComment CloseCurly KwAs LowerIdent LowerIdent OpQuestion MalformedString CloseRound CloseCurly LineComment BlankLine LowerIdent OpColon OpenCurly CloseCurly LowerIdent OpAssign OpenCurly CloseCurly BlankLine LowerIdent OpColon UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound CloseRound BlankLine KwExpect OpenCurly LowerIdent OpAssign Int LineComment LowerIdent OpAssign Int LowerIdent OpEquals LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
# This is a module comment!
app { pf: "../basic-cli/platform.roc" platform [main!] }

import pf.Stdout exposing [line!, write!]

import # Comment after import keyword
pf. # Comment after qualifier
StdoutMultiline exposing [ # Comment after ident
	# Comment after exposing open
	line!, # Comment after exposed item
	write!, # Another after exposed item
]
# Comment after exposing close

import pkg.Something exposing [func]
as 
function
, 
Type
as 
ValueCategory
, 
.*
]

import BadName as GoodName
import BadNameMultiline as GoodNameMultiline

Map((a, b)) : List a -> (a -> b) -> List b
# Comment here
# And here
# And after the last arg
MapML(
	(
		a,
		b,
	),
) : # And after the colon
List # Inside Tag args
a -> ( # After tag arg
a -> b) -> # After arrow
List # Inside tag args
b
# And after the type decl

Foo : (Bar, Baz)

FooMultiline : # Comment after pattern tuple open
(
	Bar, # Comment after pattern tuple item
	Baz,
)
# Another after pattern tuple item
# Comment after pattern tuple close

Some(a) : {foo : Ok a, bar : Something}
SomeMl(a) :
	{
		 # After record open
foo : Ok a,
		 # After field
bar : Something
	}
# After last field

SomeMultiline(a) :
	{
		 # Comment after pattern record open
foo : # After field name
		# Before field anno
Ok a,
		 # Comment after pattern record field
bar : Something
	}
# Another after pattern record field
# Comment after pattern record close

Maybe(a) : [Some(a), None]

MaybeMultiline(a) : [ # Comment after tag union open
	Some(a), # Comment after tag union member
	None, # Another after tag union member
]
# Comment after tag union close

SomeFunc(a) : Maybe a -> a -> Maybe a

add_one_oneline = |num| if num 2 else 5

add_one : U64 -> U64
add_one = |num| {
	other = 1
	if num
		{
			dbg # After debug
			
			some_func()
			# After debug expr
			0
		}
	else {
		dbg 
		123
		other : other
	}
}

match_time = |a, # After arg
b| # After args
match a
	(Blue || Green) || Red => 
		x = 12
		x : x
	(Blue || # After pattern in alt
	# Before pattern in alt
Green) || Red => 
		 # After alt pattern
{
			x = 12
			x : x
		}
		lower => # After pattern comment
1
		"foo" => # After arrow comment
100
		"foo"
		=> 
		200
		[1, 2, 3, ..as ]
		rest
		] # After pattern comment
			 => # After arrow comment
123
		 # After branch comment

		# Just a random comment

[1, 2]
		rest
		] => 123
		[1, 2]
		 # After DoubleDot
		# Before alias
rest
		 # After last pattern in list
] => 123
		3.14 => 314
		3.14
		=> 
		123((1, 2))
		) => 123
		({ foo : 1, bar : 2, ..rest } => 12-) -> add(34)
		{ # After pattern record open
			foo : # After pattern record field name
			# Before pattern record field value
1, # After pattern record field
			bar : 2,
			..rest, # After spread operator
		# After last field
		} => 12
		{ foo : 1, bar : 2 }
		} => 12
		{ foo : 1, bar : 2 }
		 # After last record field
=> 
		12
	Ok(123) => 123
	Ok(Some(dude)) => dude
	TwoArgs("hello", Some("world")) => 1000

expect # Comment after expect keyword
blah == 1
# Comment after expect statement

main! : List String -> Result({}, _)
main! = |
	_,
| { # Yeah I can leave a comment here
	world = "World",
	var number = 123,
	expect blah == 1,
	tag = Blue,
	return # Comment after return keyword
tag, # Comment after return statement

	# Just a random comment!

	...,
	match_time(
		...,
	), # Single args with comment
	some_func(dbg # After debug
			),
	42, # After debug expr
}

crash # Comment after crash keyword
"Unreachable!"
# Comment after crash statement
tag_with_payload = Ok(number)
interpolated = "Hello, ${world}"
list = [
	add_one(dbg # After dbg in list
				),
]

number
, # after dbg expr as arg
		
)
, # Comment one
		
456
, # Comment two
		
789
, # Comment three
	
]
	
for n in list {
	{
		Stdout.line!("Adding ${n} to ${number}")
		number = number + n
	}
}
record = { foo : 123, bar : "Hello", baz : tag, qux : Ok world, punned }
tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
multiline_tuple = (
	123,
	"World",
	tag1,
	Ok(world), # This one has a comment
	(nested, tuple),
	[1, 2, 3],
)

bin_op_result = (Err(foo) ?? 12 > 5 * 5 || 13 + 2 < 5 && 10 - 1 >= 16) || 12 <= 3 / 5
static_dispatch_style = some_fn(arg1)
? | .static_dispatch_method()
? | .next_static_dispatch_method()
? | .record_field
?
	
Stdout.line!(interpolated)
?
	
Stdout.line!("How about ${ # Comment after string interpolation open
			)
Num.toStr(number)
# Comment after string interpolation expr
} 
as 
a
string
?
",
	
)
} # Comment after top-level decl

empty : {}
empty = {}

tuple : Value(a, b, c)

expect {
	foo = 1
	# This should work too
	blah = 1
	blah == foo
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:14:37:14:40:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:14:48:14:50:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                               ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:14:55:14:58:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                                      ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:14:71:14:73:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                                                      ^^


**PARSE ERROR**
A parsing error occurred: **expr_dot_suffix_not_allowed**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:14:79:14:81:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                                                              ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:14:81:16:1:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]

import BadName as GoodName
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg # After debug
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:71:3:72:4:**
```roc
		dbg # After debug
			some_func() # After debug expr
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:75:3:75:7:**
```roc
		dbg 123
```
		^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:101:17:101:20:**
```roc
		"foo" | "bar" => 200
```
		              ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:102:16:102:19:**
```roc
		[1, 2, 3, .. as rest] # After pattern comment
```
		             ^^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**syntax_grab_bag.md:102:3:102:19:**
```roc
		[1, 2, 3, .. as rest] # After pattern comment
```
		^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] # After pattern comment
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:102:23:103:4:**
```roc
		[1, 2, 3, .. as rest] # After pattern comment
			=> # After arrow comment
```


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**syntax_grab_bag.md:108:3:108:9:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:108:20:108:23:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		                 ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **rest** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:108:23:108:27:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		                    ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:108:27:108:29:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		                        ^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**syntax_grab_bag.md:109:3:111:6:**
```roc
		[
			1,
			2 | 5,
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as # Before alias
					** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:114:5:115:6:**
```roc
				as # Before alias
					rest, # After last pattern in list
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **rest** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:115:6:115:10:**
```roc
					rest, # After last pattern in list
```
					^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:116:3:116:5:**
```roc
		] => 123
```
		^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:118:15:118:18:**
```roc
		3.14 | 6.28 => 314
```
		            ^^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:119:16:120:9:**
```roc
		(1, 2, 3) => 123
		(1, 2 | 5, 3) => 123
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **) ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:120:15:120:17:**
```roc
		(1, 2 | 5, 3) => 123
```
		            ^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:130:3:130:20:**
```roc
		{ foo: 1, bar: 2 | 7 } => 12
```
		^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:130:24:130:26:**
```roc
		{ foo: 1, bar: 2 | 7 } => 12
```
		                     ^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:131:3:133:11:**
```roc
		{
			foo: 1,
			bar: 2 | 7, # After last record field
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:134:3:134:5:**
```roc
		} => 12
```
		^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:134:5:134:8:**
```roc
		} => 12
```
		  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg # After debug
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:159:3:160:4:**
```roc
		dbg # After debug
			42, # After debug expr
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:158:2:160:4:**
```roc
	some_func(
		dbg # After debug
			42, # After debug expr
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:161:2:162:2:**
```roc
	)
	crash # Comment after crash keyword
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:144:13:162:2:**
```roc
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
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg # After dbg in list
				** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:168:4:169:5:**
```roc
			dbg # After dbg in list
				number, # after dbg expr as arg
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:167:3:169:5:**
```roc
		add_one(
			dbg # After dbg in list
				number, # after dbg expr as arg
```


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**syntax_grab_bag.md:166:9:169:5:**
```roc
	list = [
		add_one(
			dbg # After dbg in list
				number, # after dbg expr as arg
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, # after dbg expr as arg
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:169:11:170:3:**
```roc
				number, # after dbg expr as arg
		), # Comment one
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:170:3:170:4:**
```roc
		), # Comment one
```
		^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, # Comment one
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:170:4:171:3:**
```roc
		), # Comment one
		456, # Comment two
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, # Comment two
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:171:6:172:3:**
```roc
		456, # Comment two
		789, # Comment three
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, # Comment three
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:172:6:173:2:**
```roc
		789, # Comment three
	]
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:173:2:174:2:**
```roc
	]
	for n in list {
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:189:39:189:40:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
	                                     ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:189:65:189:66:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
	                                                               ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:189:96:189:97:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
	                                                                                              ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:189:110:190:2:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
	Stdout.line!(interpolated)?
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:190:28:191:2:**
```roc
	Stdout.line!(interpolated)?
	Stdout.line!(
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"How about ${ # Comment after string interpolation open
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:192:3:193:4:**
```roc
		"How about ${ # Comment after string interpolation open
			Num.toStr(number) # Comment after string interpolation expr
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**syntax_grab_bag.md:191:2:193:4:**
```roc
	Stdout.line!(
		"How about ${ # Comment after string interpolation open
			Num.toStr(number) # Comment after string interpolation expr
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:194:3:194:5:**
```roc
		} as a string?",
```
		^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:194:5:194:8:**
```roc
		} as a string?",
```
		  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:194:16:194:17:**
```roc
		} as a string?",
```
		             ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **",
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:194:17:195:2:**
```roc
		} as a string?",
	)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:195:2:196:1:**
```roc
	)
} # Comment after top-level decl
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} # Comment after top-level decl

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**syntax_grab_bag.md:196:1:198:1:**
```roc
} # Comment after top-level decl

empty : {}
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:14:37:14:40:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                    ^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**syntax_grab_bag.md:14:40:14:48:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                       ^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:14:48:14:50:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                               ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:14:50:14:54:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                                 ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:14:55:14:58:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                                      ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:14:58:14:71:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                                         ^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:14:71:14:73:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                                                      ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:14:79:14:81:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
```
                                                                              ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:14:81:16:1:**
```roc
import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]

import BadName as GoodName
```


**UNDEFINED VARIABLE**
Nothing is named **some_func** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:72:4:72:13:**
```roc
			some_func() # After debug expr
```
			^^^^^^^^^


**UNUSED VARIABLE**
Variable **other** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_other` to suppress this warning.
The unused variable is declared here:

**syntax_grab_bag.md:76:3:76:8:**
```roc
		other
```
		^^^^^


**UNUSED VARIABLE**
Variable **other** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_other` to suppress this warning.
The unused variable is declared here:

**syntax_grab_bag.md:69:2:69:7:**
```roc
	other = 1
```
	^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:85:3:88:4:**
```roc
		Blue | Green | Red => {
			x = 12
			x
		}
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:89:3:92:8:**
```roc
		Blue # After pattern in alt
		| # Before pattern in alt
			Green
		| Red # After alt pattern
```


**UNUSED VARIABLE**
Variable **x** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:

**syntax_grab_bag.md:95:5:95:6:**
```roc
				x
```
				^


**UNDEFINED VARIABLE**
Nothing is named **rest** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:102:19:102:23:**
```roc
		[1, 2, 3, .. as rest] # After pattern comment
```
		                ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:102:23:103:4:**
```roc
		[1, 2, 3, .. as rest] # After pattern comment
			=> # After arrow comment
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:108:27:108:29:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		                        ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:116:3:116:5:**
```roc
		] => 123
```
		^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:120:15:120:17:**
```roc
		(1, 2 | 5, 3) => 123
```
		            ^^


**UNDEFINED VARIABLE**
Nothing is named **add** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:121:37:121:40:**
```roc
		{ foo: 1, bar: 2, ..rest } => 12->add(34)
```
		                                  ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:130:24:130:26:**
```roc
		{ foo: 1, bar: 2 | 7 } => 12
```
		                     ^^


**UNUSED VARIABLE**
Variable **.. # After spread operator
				rest** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_.. # After spread operator
				rest` to suppress this warning.
The unused variable is declared here:

**syntax_grab_bag.md:127:4:128:9:**
```roc
			.. # After spread operator
				rest, # After last field
```


**UNUSED VARIABLE**
Variable **lower** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_lower` to suppress this warning.
The unused variable is declared here:

**syntax_grab_bag.md:97:3:97:8:**
```roc
		lower # After pattern comment
```
		^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:135:3:135:17:**
```roc
		Ok(123) => 123
```
		^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:137:3:137:42:**
```roc
		TwoArgs("hello", Some("world")) => 1000
```
		^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **b** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:

**syntax_grab_bag.md:82:2:82:3:**
```roc
	b,
```
	^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:140:1:141:11:**
```roc
expect # Comment after expect keyword
	blah == 1 # Comment after expect statement
```


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:145:2:145:7:**
```roc
	world = "World"
```
	^^^^^


**UNDEFINED VARIABLE**
Nothing is named **var number** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:146:2:146:12:**
```roc
	var number = 123
```
	^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **blah** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:147:9:147:13:**
```roc
	expect blah == 1
```
	       ^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:148:2:148:5:**
```roc
	tag = Blue
```
	^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:150:3:150:6:**
```roc
		tag # Comment after return statement
```
		^^^


**UNDEFINED VARIABLE**
Nothing is named **some_func** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:158:2:158:11:**
```roc
	some_func(
```
	^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **number** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:164:24:164:30:**
```roc
	tag_with_payload = Ok(number)
```
	                      ^^^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**syntax_grab_bag.md:169:5:169:11:**
```roc
				number, # after dbg expr as arg
```
				^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:169:11:170:3:**
```roc
				number, # after dbg expr as arg
		), # Comment one
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:170:3:170:4:**
```roc
		), # Comment one
```
		^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:170:4:171:3:**
```roc
		), # Comment one
		456, # Comment two
```


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**syntax_grab_bag.md:171:3:171:6:**
```roc
		456, # Comment two
```
		^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:171:6:172:3:**
```roc
		456, # Comment two
		789, # Comment three
```


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**syntax_grab_bag.md:172:3:172:6:**
```roc
		789, # Comment three
```
		^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:172:6:173:2:**
```roc
		789, # Comment three
	]
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:173:2:174:2:**
```roc
	]
	for n in list {
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:174:2:177:3:**
```roc
	for n in list {
		Stdout.line!("Adding ${n} to ${number}")
		number = number + n
	}
```


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:178:42:178:45:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```
	                                        ^^^


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:178:55:178:60:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```
	                                                     ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **punned** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:178:63:178:69:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```
	                                                             ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:179:25:179:28:**
```roc
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
```
	                       ^^^


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:179:33:179:38:**
```roc
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
```
	                               ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **nested** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:179:42:179:48:**
```roc
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
```
	                                        ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag1** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:183:3:183:7:**
```roc
		tag1,
```
		^^^^


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:184:6:184:11:**
```roc
		Ok(world), # This one has a comment
```
		   ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **nested** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:185:4:185:10:**
```roc
		(nested, tuple),
```
		 ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **foo** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:188:22:188:25:**
```roc
	bin_op_result = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
```
	                    ^^^


**UNDEFINED VARIABLE**
Nothing is named **some_fn** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:189:26:189:33:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
	                        ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **arg1** in this scope.
Is there an **import** or **exposing** missing up-top?

**syntax_grab_bag.md:189:34:189:38:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
	                                ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:189:39:189:65:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
	                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:189:65:189:96:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
	                                                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:189:96:189:110:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
	                                                                                              ^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:189:110:190:2:**
```roc
	static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
	Stdout.line!(interpolated)?
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:190:2:190:28:**
```roc
	Stdout.line!(interpolated)?
```
	^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:190:28:191:2:**
```roc
	Stdout.line!(interpolated)?
	Stdout.line!(
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:191:2:193:7:**
```roc
	Stdout.line!(
		"How about ${ # Comment after string interpolation open
			Num.toStr(number) # Comment after string interpolation expr
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:193:4:193:21:**
```roc
			Num.toStr(number) # Comment after string interpolation expr
```
			^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:194:3:194:5:**
```roc
		} as a string?",
```
		^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:194:5:194:8:**
```roc
		} as a string?",
```
		  ^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**syntax_grab_bag.md:194:8:194:9:**
```roc
		} as a string?",
```
		     ^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**syntax_grab_bag.md:194:10:194:16:**
```roc
		} as a string?",
```
		       ^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:194:16:194:17:**
```roc
		} as a string?",
```
		             ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:194:17:195:2:**
```roc
		} as a string?",
	)
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:195:2:196:1:**
```roc
	)
} # Comment after top-level decl
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:196:1:198:1:**
```roc
} # Comment after top-level decl

empty : {}
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**syntax_grab_bag.md:203:1:207:2:**
```roc
expect {
	foo = 1 # This should work too
	blah = 1
	blah == foo
}
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.import)
  (Stmt.import)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.import)
  (Stmt.import)
  (Stmt.type_anno
    (name node:apply_uc)
    (type binop_thin_arrow)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type binop_thin_arrow)
  )
  (Stmt.type_anno
    (name node:uc)
    (type tuple_literal)
  )
  (Stmt.type_anno
    (name node:uc)
    (type tuple_literal)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type record_literal)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type record_literal)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type record_literal)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type list_literal)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type list_literal)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "add_one_oneline"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "add_one")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "add_one"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "match_time"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.malformed)
  (Stmt.type_anno
    (name "main")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.crash)
  (Stmt.assign
    (pattern (Patt.ident "tag_with_payload"))
    (Expr.apply_tag)
  )
  (Stmt.assign
    (pattern (Patt.ident "interpolated"))
    (Expr.str_literal_big)
  )
  (Stmt.assign
    (pattern (Patt.ident "list"))
    (Expr.list_literal)
  )
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.assign
    (pattern (Patt.ident "record"))
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.malformed)
        (Expr.num_literal_i32 123)
      )
      (Expr.binop_colon
        (Expr.malformed)
        (Expr.str_literal_big)
      )
      (Expr.binop_colon
        (Expr.malformed)
        (Expr.lookup "tag")
      )
      (Expr.binop_colon
        (Expr.malformed)
        (Expr.apply_tag)
      )
      (Expr.lookup "punned")
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "tuple"))
    (Expr.tuple_literal
      (Expr.num_literal_i32 123)
      (Expr.str_literal_big)
      (Expr.lookup "tag")
      (Expr.apply_tag)
      (Expr.tuple_literal
        (Expr.lookup "nested")
        (Expr.lookup "tuple")
      )
      (Expr.list_literal)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "multiline_tuple"))
    (Expr.tuple_literal
      (Expr.num_literal_i32 123)
      (Expr.str_literal_big)
      (Expr.lookup "tag1")
      (Expr.apply_tag)
      (Expr.tuple_literal
        (Expr.lookup "nested")
        (Expr.lookup "tuple")
      )
      (Expr.list_literal)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "bin_op_result"))
    (Expr.binop_or
      (Expr.binop_or
        (Expr.binop_gt
          (Expr.binop_double_question
            (Expr.apply_tag)
            (Expr.num_literal_i32 12)
          )
          (Expr.binop_star
            (Expr.num_literal_i32 5)
            (Expr.num_literal_i32 5)
          )
        )
        (Expr.binop_and
          (Expr.binop_lt
            (Expr.binop_plus
              (Expr.num_literal_i32 13)
              (Expr.num_literal_i32 2)
            )
            (Expr.num_literal_i32 5)
          )
          (Expr.binop_gte
            (Expr.binop_minus
              (Expr.num_literal_i32 10)
              (Expr.num_literal_i32 1)
            )
            (Expr.num_literal_i32 16)
          )
        )
      )
      (Expr.binop_lte
        (Expr.num_literal_i32 12)
        (Expr.binop_slash
          (Expr.num_literal_i32 3)
          (Expr.num_literal_i32 5)
        )
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "static_dispatch_style"))
    (Expr.apply_ident)
  )
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.type_anno
    (name "empty")
    (type record_literal)
  )
  (Stmt.assign
    (pattern (Patt.ident "empty"))
    (Expr.record_literal
    )
  )
  (Stmt.type_anno
    (name "tuple")
    (type apply_uc)
  )
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_d")
~~~
# TYPES
~~~roc
~~~
