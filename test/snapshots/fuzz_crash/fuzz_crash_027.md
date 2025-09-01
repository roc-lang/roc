# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
# Thnt!
app { pf: "c" platform [main!] }

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

main! : List(String) -> Result({}, _)
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
# TOKENS
~~~text
LineComment KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent OpBang Comma LowerIdent OpBang CloseSquare BlankLine KwImport UpperIdent KwExposing OpenSquare LineComment CloseSquare LineComment BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent KwAs LowerIdent Comma UpperIdent Dot OpStar CloseSquare BlankLine KwImport UpperIdent KwAs UpperIdent KwImport UpperIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound LineComment LowerIdent Comma LineComment LowerIdent Comma CloseRound LineComment OpColon LineComment UpperIdent OpenRound LineComment CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow LineComment UpperIdent OpenRound LowerIdent CloseRound LineComment BlankLine UpperIdent OpColon OpenRound UpperIdent Comma UpperIdent CloseRound BlankLine LowerIdent OpColon OpenRound LineComment UpperIdent Comma LineComment UpperIdent Comma LineComment CloseRound LineComment UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon LowerIdent CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LineComment LowerIdent OpColon UpperIdent LineComment CloseCurly BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LineComment LowerIdent OpColon UpperIdent CloseCurly LineComment UpperIdent CloseRound OpColon OpenSquare LineComment CloseSquare LineComment BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar KwIf LowerIdent Int KwElse Int BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign Int KwIf LowerIdent OpenCurly KwDbg LineComment OpenRound CloseRound LineComment Int CloseCurly KwElse OpenCurly KwDbg Int LowerIdent CloseCurly CloseCurly BlankLine LowerIdent OpAssign OpBar LowerIdent Comma LineComment LowerIdent Comma OpBar LineComment KwMatch LowerIdent OpenCurly LowerIdent OpBar UpperIdent OpFatArrow OpenCurly LowerIdent LowerIdent CloseCurly UpperIdent OpFatArrow Int String OpFatArrow LineComment Int String OpBar String OpFatArrow Int OpenSquare Int Comma Int Comma Int Comma DoubleDot KwAs LowerIdent CloseSquare LineComment OpFatArrow LowerIdent BlankLine OpenSquare Int Comma Int OpBar Int Comma Int Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow Int OpenSquare LowerIdent CloseSquare OpFatArrow Int Float OpFatArrow Int Float OpBar Float OpFatArrow Int OpenRound Int Comma Int Comma Int CloseRound OpFatArrow Int OpenRound Int Comma Int OpBar Int Comma Int CloseRound OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int Comma DoubleDot LowerIdent CloseCurly OpFatArrow Int OpArrow LowerIdent OpenRound Int CloseRound OpenCurly LineComment LowerIdent LineComment OpColon LineComment Int Comma LineComment LowerIdent OpColon Int Comma DoubleDot CloseCurly OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int OpBar Int CloseCurly OpFatArrow Int OpenCurly LowerIdent OpColon Int Comma CloseCurly OpFatArrow Int UpperIdent OpenRound Int CloseRound OpFatArrow Int CloseCurly BlankLine KwExpect LineComment LowerIdent OpEquals Int LineComment BlankLine LowerIdent OpBang OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound OpenCurly CloseCurly Comma Underscore CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpAssign String KwVar LowerIdent OpAssign Int KwExpect LowerIdent OpEquals Int LowerIdent OpAssign UpperIdent KwReturn LineComment LowerIdent BlankLine LineComment BlankLine TripleDot LowerIdent OpenRound TripleDot Comma LineComment CloseRound LowerIdent OpenRound KwDbg LineComment Int Comma LineComment CloseRound KwCrash MalformedString LowerIdent OpAssign UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign String LowerIdent OpAssign OpenSquare LowerIdent OpenRound KwDbg LineComment LowerIdent OpenSquare Comma LineComment CloseRound Comma Int Comma LineComment CloseSquare KwFor LowerIdent KwIn LowerIdent OpenCurly LowerIdent OpBang OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpPlus LowerIdent CloseCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon String Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent CloseCurly LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound LowerIdent OpAssign OpenRound Int Comma String Comma LowerIdent Comma UpperIdent OpenRound LowerIdent CloseRound Comma LineComment OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenSquare Int Comma Int Comma Int CloseSquare Comma CloseRound LowerIdent OpAssign UpperIdent OpenRound LowerIdent CloseRound OpDoubleQuestion Int OpGreaterThan Int OpStar Int OpOr Int OpPlus Int OpLessThan Int OpAnd Int OpBinaryMinus Int OpGreaterThanOrEq Int OpOr Int OpLessThanOrEq Int OpSlash Int LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpQuestion UpperIdent OpBang OpenRound MalformedString UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LineComment CloseCurly KwAs LowerIdent MalformedString CloseRound CloseCurly LineComment BlankLine LowerIdent OpColon OpenCurly CloseCurly LowerIdent OpAssign OpenCurly CloseCurly BlankLine LowerIdent OpColon UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound CloseRound BlankLine KwExpect OpenCurly LowerIdent OpAssign Int LineComment LowerIdent OpAssign Int LowerIdent OpEquals LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_small "c")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
# Thnt!
app { pf: "c" platform [main!] }

import pf.Stdout exposing [line!, e!]

import Stdot exposing [, #tem
]
# Cose

import pkg.S exposing [func]
as 
fry
, 
.*
]

import Bae as Gooe
import Ba
Map((a, b)) : List a -> (a -> b) -> List b
# Cere
# Anre
# Ag
MapML(
	(
		a,
		b,
	),
) : # Aon
List -> ( #rg
a -> b) -> # row
List b
#

Foo : (Bar, Baz)

line : # Cpen
(
	Bar, #
	Baz,
)
#m
# Co
Some(a) : {foo : Ok a, bar : g}
Ml(a) : {
	# d
	bar : Som
}
# Afld

Soine(a) : {
	#d
	bar : Som
}
#
Maya
) : [, #
]
#se

Func(a) : Maybe a -> a -> Maybe a

ane = |num| if num 2 else 5

add_one : U64 -> U64
add_one = |num| {
	other = 1
	if num
		{
			dbg # bug
()
			#r
			0
		}
	else {
		dbg 
		123
		other : other
	}
}

match_time = |a, #rg
b| # As
match a
	lue || Red => 
		x : x
		x : x
	Blue => 
		1
		"foo" => # ent
00
		"foo"
		=> 
		20
		[1, 2, 3, ..as ]
		rest
		] # Aftet
			 => ment
		
[1, 2]
		rest
		] => 123
		[ist] => 123
		3.14 => 314
		3.14
		=> 
		123((1, 2))
		) => 123
		({ foo : 1, bar : 2, ..rest } => 12-) -> add(34)
		{ # Afrd open
			foo : #
			#ue
1, # Aftd field
			bar : 2,
			_,
		} => 12
		{ foo : 1, bar : 2 }
		} => 12
		{
			foo : 1,
		} => 12
	Ok(123) => 121000

expect # Commeneyword
blah == 1
# Commnt

main! : List String -> Result({}, _)
main! = |
	_,
| { # Yeah Ie
	world = "World",
	var number = 123,
	expect blah == 1,
	tag = Blue,
	return # Comd
tag,

	# Jusnt!

	...,
	match_time(
		...,
	), #
	some_func(dbg # bug
			),
	42, # Aft expr
}

crash "Unreachtement
	
tag_with = Ok(number)
ited = "Hello, ${world}"
list = [
	add_one(dbg # Afin list
),
]

e
[
	, # afarg
		,
]
)
,	
456
, # ee
	
]
	
for n in list {
	{
		line!("Adding ${n} to ${number}")
		number = number + n
	}
}
record = { foo : 123, bar : "Hello", baz : tag, qux : Ok world, punned }
tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
m_tuple = (
	123,
	"World",
	tag1,
	Ok(world), # Thisnt
	(nested, tuple),
	[1, 2, 3],
)

bsult = (Err(foo) ?? 12 > 5 * 5 || 13 + 2 < 5 && 10 - 1 >= 16) || 12 <= 3 / 5
stale = some_fn(arg1)
? | .statod()
? | .ned()
? | .recd
?
	
Stdoline
!"How about ${ #
			
Num.toStr(number)
# on expr
} 
as 
a
",
	
)
} # Commenl decl

empty : {}
empty = {}

tuple : Value(a, b, c)

expect {
	foo = 1
	# Thio
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

**fuzz_crash_027.md:10:29:10:32:**
```roc
import pkg.S exposing [func as fry, Custom.*]
```
                            ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:10:35:10:37:**
```roc
import pkg.S exposing [func as fry, Custom.*]
```
                                  ^^


**PARSE ERROR**
A parsing error occurred: **expr_dot_suffix_not_allowed**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:10:43:10:45:**
```roc
import pkg.S exposing [func as fry, Custom.*]
```
                                          ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:10:45:12:1:**
```roc
import pkg.S exposing [func as fry, Custom.*]

import Bae as Gooe
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **) ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:40:5:40:7:**
```roc
Maya) : [ #
```
    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg # bug
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:51:3:52:1:**
```roc
		dbg # bug
() #r
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:55:3:55:7:**
```roc
		dbg 123
```
		^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:70:17:70:20:**
```roc
		"foo" | "bar" => 20[1, 2, 3, .. as rest] # Aftet
```
		              ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:70:35:70:38:**
```roc
		"foo" | "bar" => 20[1, 2, 3, .. as rest] # Aftet
```
		                                ^^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**fuzz_crash_027.md:70:22:70:38:**
```roc
		"foo" | "bar" => 20[1, 2, 3, .. as rest] # Aftet
```
		                   ^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] # Aftet
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:70:42:71:4:**
```roc
		"foo" | "bar" => 20[1, 2, 3, .. as rest] # Aftet
			=> ment
```


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**fuzz_crash_027.md:74:3:74:9:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:74:20:74:23:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		                 ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **rest** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:74:23:74:27:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		                    ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:74:27:74:29:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		                        ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:79:15:79:18:**
```roc
		3.14 | 6.28 => 314
```
		            ^^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:80:16:81:9:**
```roc
		(1, 2, 3) => 123
		(1, 2 | 5, 3) => 123
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **) ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:81:15:81:17:**
```roc
		(1, 2 | 5, 3) => 123
```
		            ^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:89:3:89:20:**
```roc
		{ foo: 1, bar: 2 | 7 } => 12
```
		^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:89:24:89:26:**
```roc
		{ foo: 1, bar: 2 | 7 } => 12
```
		                     ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg # bug
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:115:3:116:4:**
```roc
		dbg # bug
			42, # Aft expr
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:114:2:116:4:**
```roc
	some_func(
		dbg # bug
			42, # Aft expr
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:117:2:118:2:**
```roc
	)
	crash "Unreachtement
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:100:13:118:2:**
```roc
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
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"Unreachtement
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:118:8:119:2:**
```roc
	crash "Unreachtement
	tag_with = Ok(number)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg # Afin list
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:123:4:124:1:**
```roc
			dbg # Afin list
e[, # afarg
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:122:3:124:1:**
```roc
		add_one(
			dbg # Afin list
e[, # afarg
```


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**fuzz_crash_027.md:121:9:124:1:**
```roc
	list = [
		add_one(
			dbg # Afin list
e[, # afarg
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, # afarg
		** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:124:3:125:3:**
```roc
e[, # afarg
		),	456, # ee
```


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**fuzz_crash_027.md:124:2:125:3:**
```roc
e[, # afarg
		),	456, # ee
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:125:3:125:4:**
```roc
		),	456, # ee
```
		^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:125:4:125:6:**
```roc
		),	456, # ee
```
		 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, # ee
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:125:9:126:2:**
```roc
		),	456, # ee
	]
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:126:2:127:2:**
```roc
	]
	for n in list {
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:142:23:142:24:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
```
	                     ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:142:33:142:34:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
```
	                               ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:142:40:142:41:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
```
	                                      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **?
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:142:46:143:2:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
	Stdoline!(
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"How about ${ #
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:144:3:145:4:**
```roc
		"How about ${ #
			Num.toStr(number) # on expr
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_round_or_comma**
This is an unexpected parsing error. Please check your syntax.



**UNEXPECTED TOKEN IN EXPRESSION**
The token **} ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:146:3:146:5:**
```roc
		} as a",
```
		^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:146:5:146:8:**
```roc
		} as a",
```
		  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **",
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:146:9:147:2:**
```roc
		} as a",
	)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:147:2:148:1:**
```roc
	)
} # Commenl decl
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} # Commenl decl

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:148:1:150:1:**
```roc
} # Commenl decl

empty : {}
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:10:29:10:32:**
```roc
import pkg.S exposing [func as fry, Custom.*]
```
                            ^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_027.md:10:32:10:35:**
```roc
import pkg.S exposing [func as fry, Custom.*]
```
                               ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:10:35:10:37:**
```roc
import pkg.S exposing [func as fry, Custom.*]
```
                                  ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:10:43:10:45:**
```roc
import pkg.S exposing [func as fry, Custom.*]
```
                                          ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:10:45:12:1:**
```roc
import pkg.S exposing [func as fry, Custom.*]

import Bae as Gooe
```


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**fuzz_crash_027.md:33:9:35:2:**
```roc
Ml(a) : { # d
	bar : Som# Afld
}
```


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**fuzz_crash_027.md:37:12:39:2:**
```roc
Soine(a) : { #d
	bar : Som
} #
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:40:1:40:5:**
```roc
Maya) : [ #
```
^^^^


**UNUSED VARIABLE**
Variable **other** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_other` to suppress this warning.
The unused variable is declared here:

**fuzz_crash_027.md:56:3:56:8:**
```roc
		other
```
		^^^^^


**UNUSED VARIABLE**
Variable **other** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_other` to suppress this warning.
The unused variable is declared here:

**fuzz_crash_027.md:49:2:49:7:**
```roc
	other = 1
```
	^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:64:11:66:4:**
```roc
	match a {lue | Red => {
			x x
		}
```


**UNDEFINED VARIABLE**
Nothing is named **rest** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:70:38:70:42:**
```roc
		"foo" | "bar" => 20[1, 2, 3, .. as rest] # Aftet
```
		                                   ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:70:42:71:4:**
```roc
		"foo" | "bar" => 20[1, 2, 3, .. as rest] # Aftet
			=> ment
```


**UNDEFINED VARIABLE**
Nothing is named **ment** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:71:7:71:11:**
```roc
			=> ment
```
			   ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:74:27:74:29:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
		                        ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:81:15:81:17:**
```roc
		(1, 2 | 5, 3) => 123
```
		            ^^


**UNDEFINED VARIABLE**
Nothing is named **add** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:82:37:82:40:**
```roc
		{ foo: 1, bar: 2, ..rest } => 12->add(34)
```
		                                  ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:89:24:89:26:**
```roc
		{ foo: 1, bar: 2 | 7 } => 12
```
		                     ^^


**UNUSED VARIABLE**
Variable **..rest** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_..rest` to suppress this warning.
The unused variable is declared here:

**fuzz_crash_027.md:82:21:82:27:**
```roc
		{ foo: 1, bar: 2, ..rest } => 12->add(34)
```
		                  ^^^^^^


**UNUSED VARIABLE**
Variable **ist** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_ist` to suppress this warning.
The unused variable is declared here:

**fuzz_crash_027.md:76:1:76:4:**
```roc
ist
```
^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:93:3:93:20:**
```roc
		Ok(123) => 121000
```
		^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **b** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:

**fuzz_crash_027.md:62:2:62:3:**
```roc
	b,
```
	^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:96:1:97:11:**
```roc
expect # Commeneyword
	blah == 1 # Commnt
```


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:101:2:101:7:**
```roc
	world = "World"
```
	^^^^^


**UNDEFINED VARIABLE**
Nothing is named **var number** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:102:2:102:12:**
```roc
	var number = 123
```
	^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **blah** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:103:9:103:13:**
```roc
	expect blah == 1
```
	       ^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:104:2:104:5:**
```roc
	tag = Blue
```
	^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:106:3:106:6:**
```roc
		tag
```
		^^^


**UNDEFINED VARIABLE**
Nothing is named **some_func** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:114:2:114:11:**
```roc
	some_func(
```
	^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **number** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:119:16:119:22:**
```roc
	tag_with = Ok(number)
```
	              ^^^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_027.md:124:1:124:2:**
```roc
e[, # afarg
```
^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:124:2:125:3:**
```roc
e[, # afarg
		),	456, # ee
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:125:3:125:4:**
```roc
		),	456, # ee
```
		^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:125:4:125:6:**
```roc
		),	456, # ee
```
		 ^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_027.md:125:6:125:9:**
```roc
		),	456, # ee
```
		  	^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:125:9:126:2:**
```roc
		),	456, # ee
	]
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:126:2:127:2:**
```roc
	]
	for n in list {
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:127:2:130:3:**
```roc
	for n in list {
	line!("Adding ${n} to ${number}")
		number = number + n
	}
```


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:131:42:131:45:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```
	                                        ^^^


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:131:55:131:60:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```
	                                                     ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **punned** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:131:63:131:69:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```
	                                                             ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:132:25:132:28:**
```roc
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
```
	                       ^^^


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:132:33:132:38:**
```roc
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
```
	                               ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **nested** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:132:42:132:48:**
```roc
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
```
	                                        ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **tag1** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:136:3:136:7:**
```roc
		tag1,
```
		^^^^


**UNDEFINED VARIABLE**
Nothing is named **world** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:137:6:137:11:**
```roc
		Ok(world), # Thisnt
```
		   ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **nested** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:138:4:138:10:**
```roc
		(nested, tuple),
```
		 ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **foo** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:141:14:141:17:**
```roc
	bsult = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
```
	            ^^^


**UNDEFINED VARIABLE**
Nothing is named **some_fn** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:142:10:142:17:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
```
	        ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **arg1** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_027.md:142:18:142:22:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
```
	                ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:142:23:142:33:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
```
	                     ^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:142:33:142:40:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
```
	                               ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:142:40:142:46:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
```
	                                      ^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:142:46:143:2:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
	Stdoline!(
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:143:2:143:10:**
```roc
	Stdoline!(
```
	^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:143:10:143:11:**
```roc
	Stdoline!(
```
	        ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:145:4:145:21:**
```roc
			Num.toStr(number) # on expr
```
			^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:146:3:146:5:**
```roc
		} as a",
```
		^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:146:5:146:8:**
```roc
		} as a",
```
		  ^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_027.md:146:8:146:9:**
```roc
		} as a",
```
		     ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:146:9:147:2:**
```roc
		} as a",
	)
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:147:2:148:1:**
```roc
	)
} # Commenl decl
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:148:1:150:1:**
```roc
} # Commenl decl

empty : {}
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_027.md:155:1:159:2:**
```roc
expect {
	foo = 1 # Thio
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
    (name "line")
    (type tuple_literal)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type record_literal)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type block)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type block)
  )
  (Stmt.malformed)
  (Stmt.type_anno
    (name node:malformed)
    (type list_literal)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "ane"))
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
    (pattern (Patt.ident "tag_with"))
    (Expr.apply_tag)
  )
  (Stmt.assign
    (pattern (Patt.ident "ited"))
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
    (pattern (Patt.ident "m_tuple"))
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
    (pattern (Patt.ident "bsult"))
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
    (pattern (Patt.ident "stale"))
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
