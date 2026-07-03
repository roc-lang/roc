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
PARSE ERROR - fuzz_crash_027.md:40:5:40:6
PARSE ERROR - fuzz_crash_027.md:40:7:40:8
PARSE ERROR - fuzz_crash_027.md:40:9:40:10
PARSE ERROR - fuzz_crash_027.md:41:1:41:2
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_027.md:124:3:124:4
LIST NOT CLOSED - fuzz_crash_027.md:125:3:125:4
PARSE ERROR - fuzz_crash_027.md:126:2:126:3
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_027.md:131:9:131:10
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_027.md:132:8:132:9
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_027.md:133:10:133:11
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_027.md:141:8:141:9
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_027.md:142:8:142:9
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_027.md:148:1:148:2
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_027.md:150:7:150:8
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_027.md:151:7:151:8
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_027.md:153:7:153:8
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_027.md:155:1:155:7
LIST NOT CLOSED - fuzz_crash_027.md:160:1:160:1
PARSE ERROR - fuzz_crash_027.md:160:1:160:1
MODULE NOT FOUND - fuzz_crash_027.md:6:1:8:4
MODULE NOT FOUND - fuzz_crash_027.md:12:1:12:19
MODULE NOT FOUND - fuzz_crash_027.md:13:1:14:4
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
NAME NOT IN SCOPE - fuzz_crash_027.md:114:2:114:11
UNRECOGNIZED SYNTAX - fuzz_crash_027.md:1:1:1:1
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
TYPE MISMATCH - fuzz_crash_027.md:106:3:106:6
TOO FEW ARGS - fuzz_crash_027.md:111:2:113:3
# PROBLEMS

LEADING ZERO

Numbers cannot have leading zeros.



┌─────────────────┐
│ UNCLOSED STRING ├─ This string is missing a closing quote. ─────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  crash "Unreachtement                                                      │
 │        ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                      │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:118:8 ┘



┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  Maya) : [ #                                                               │
 │      ‾                                                                     │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:40:5 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Maya) : [ #                                                               │
 │        ‾                                                                   │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:40:7 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Maya) : [ #                                                               │
 │          ‾                                                                 │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:40:9 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  ] #se                                                                     │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:41:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token , is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  e[, # afarg                                                               │
 │    ‾                                                                       │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:124:3 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌─────────────────┐
│ LIST NOT CLOSED ├─ This list is missing a closing bracket or has a syntax ──┐
└┬────────────────┘  error.                                                   │
 │                                                                            │
 │  ), 456, # ee                                                              │
 │  ‾                                                                         │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:125:3 ┘

    Lists must be closed with ] and list items must be separated by commas.
    For example:     [1, 2, 3]


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: expected_expr_apply_close_round ───┐
└┬────────────┘                                                               │
 │                                                                            │
 │  ]                                                                         │
 │  ‾                                                                         │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:126:2 ┘

    This is an unexpected parsing error. Please check your syntax.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token = is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }     │
 │         ‾                                                                  │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:131:9 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token = is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])        │
 │        ‾                                                                   │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:132:8 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token = is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  m_tuple = (                                                               │
 │          ‾                                                                 │
 └────────────────────────────────────────────────── fuzz_crash_027.md:133:10 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token = is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  bsult = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3… │
 │        ‾                                                                   │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:141:8 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token = is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  stale = some_fn(arg1)?.statod()?.ned()?.recd?                             │
 │        ‾                                                                   │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:142:8 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token } is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  } # Commenl decl                                                          │
 │  ‾                                                                         │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:148:1 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token : is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  empty : {}                                                                │
 │        ‾                                                                   │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:150:7 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token = is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  empty = {}                                                                │
 │        ‾                                                                   │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:151:7 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token : is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  tuple : Value((a, b, c))                                                  │
 │        ‾                                                                   │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:153:7 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token expect is not expected in an ───┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  expect {                                                                  │
 │  ‾‾‾‾‾‾                                                                    │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:155:1 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌─────────────────┐
│ LIST NOT CLOSED ├─ This list is missing a closing bracket or has a syntax ──┐
└┬────────────────┘  error.                                                   │
 │                                                                            │
 │                                                                            │
 │  ‾                                                                         │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:160:1 ┘

    Lists must be closed with ] and list items must be separated by commas.
    For example:     [1, 2, 3]


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: expected_expr_close_curly ─────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │                                                                            │
 │  ‾                                                                         │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:160:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌──────────────────┐
│ MODULE NOT FOUND ├─ The module `Stdot` was not found in this Roc project. ──┐
└┬─────────────────┘                                                          │
 │                                                                            │
 │  import Stdot                                                              │
 │          exposing [ #tem                                                   │
 │          ] # Cose                                                          │
 │                                                                            │
 └───────────────────────────────────────────────────── fuzz_crash_027.md:6:1 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ The module `Bae` was not found in this Roc project. ────┐
└┬─────────────────┘                                                          │
 │                                                                            │
 │  import Bae as Gooe                                                        │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                        │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:12:1 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ The module `Ba` was not found in this Roc project. ─────┐
└┬─────────────────┘                                                          │
 │                                                                            │
 │  import                                                                    │
 │      Ba                                                                    │
 │                                                                            │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:13:1 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Bar` is not declared in this scope. ───────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Foo : (Bar, Baz)                                                          │
 │         ‾‾‾                                                                │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:26:8 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Baz` is not declared in this scope. ───────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Foo : (Bar, Baz)                                                          │
 │              ‾‾‾                                                           │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:26:13 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Bar` is not declared in this scope. ───────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Bar, #                                                                    │
 │  ‾‾‾                                                                       │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:29:2 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Baz` is not declared in this scope. ───────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Baz, #m                                                                   │
 │  ‾‾‾                                                                       │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:30:2 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Ok` is not declared in this scope. ────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Some(a) : { foo : Ok(a), bar : g }                                        │
 │                    ‾‾                                                      │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:32:19 ┘



┌──────────────────────────┐
│ UNDECLARED TYPE VARIABLE ├─ The type variable `g` is not declared in this ──┐
└┬─────────────────────────┘  scope.                                          │
 │                                                                            │
 │  Some(a) : { foo : Ok(a), bar : g }                                        │
 │                                 ‾                                          │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:32:32 ┘

    Type variables must be introduced in a type annotation before they can be
    used.


┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Som` is not declared in this scope. ───────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  bar : Som# Afld                                                           │
 │        ‾‾‾                                                                 │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:34:8 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Som` is not declared in this scope. ───────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  bar : Som                                                                 │
 │        ‾‾‾                                                                 │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:38:8 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Maybe` is not declared in this scope. ─────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Func(a) : Maybe(a), a -> Maybe(a)                                         │
 │            ‾‾‾‾‾                                                           │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:43:11 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Maybe` is not declared in this scope. ─────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Func(a) : Maybe(a), a -> Maybe(a)                                         │
 │                           ‾‾‾‾‾                                            │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:43:26 ┘



┌─────────────────────────┐
│ EMPTY TUPLE NOT ALLOWED ├─ I am part way through parsing this tuple, but ───┐
└┬────────────────────────┘  it is empty.                                     │
 │                                                                            │
 │  () #r                                                                     │
 │  ‾‾                                                                        │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:52:1 ┘

    If you want to represent nothing, try using an empty record: `{}`.


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `x` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  x x                                                                       │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:65:4 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `x` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  x x                                                                       │
 │    ‾                                                                       │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:65:6 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `ment` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  => ment                                                                   │
 │     ‾‾‾‾                                                                   │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:71:7 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `rest` is defined here and then never used. ────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  # Thnt!                                                                   │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_027.md:1:1 ┘

    If you don't need this variable, prefix it with an underscore like `_rest`
    to suppress this warning.


┌─────────────────┐
│ NOT IMPLEMENTED ├─ This feature is not yet implemented: alternatives ───────┐
└┬────────────────┘  pattern outside match expression.                        │
 │                                                                            │
 │  [1, 2 | 5, 3, .. as rest] => 123                                          │
 │      ‾‾‾‾‾                                                                 │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:74:7 ┘

    This error doesn't have a proper diagnostic report yet. Let us know if you
    want to help improve Roc's error messages!


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `rest` is defined here and then never used. ────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  # Thnt!                                                                   │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_027.md:1:1 ┘

    If you don't need this variable, prefix it with an underscore like `_rest`
    to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `ist` is defined here and then never used. ─────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  ist                                                                       │
 │  ‾‾‾                                                                       │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:76:1 ┘

    If you don't need this variable, prefix it with an underscore like `_ist`
    to suppress this warning.


┌─────────────────┐
│ NOT IMPLEMENTED ├─ This feature is not yet implemented: alternatives ───────┐
└┬────────────────┘  pattern outside match expression.                        │
 │                                                                            │
 │  (1, 2 | 5, 3) => 123                                                      │
 │      ‾‾‾‾‾                                                                 │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:81:7 ┘

    This error doesn't have a proper diagnostic report yet. Let us know if you
    want to help improve Roc's error messages!


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `add` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  { foo: 1, bar: 2, ..rest } => 12->add(34)                                 │
 │                                    ‾‾‾                                     │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:82:37 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `rest` is defined here and then never used. ────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  { foo: 1, bar: 2, ..rest } => 12->add(34)                                 │
 │                    ‾‾‾‾‾‾                                                  │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:82:21 ┘

    If you don't need this variable, prefix it with an underscore like `_rest`
    to suppress this warning.


┌─────────────────┐
│ NOT IMPLEMENTED ├─ This feature is not yet implemented: alternatives ───────┐
└┬────────────────┘  pattern outside match expression.                        │
 │                                                                            │
 │  { foo: 1, bar: 2 | 7 } => 12                                              │
 │                 ‾‾‾‾‾                                                      │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:89:18 ┘

    This error doesn't have a proper diagnostic report yet. Let us know if you
    want to help improve Roc's error messages!


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `b` is defined here and then never used. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  b,                                                                        │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:62:2 ┘

    If you don't need this variable, prefix it with an underscore like `_b` to
    suppress this warning.


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `blah` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  blah == 1 # Commnt                                                        │
 │  ‾‾‾‾                                                                      │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:97:2 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `String` is not declared in this scope. ────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  main! : List(String) -> Try({}, _)                                        │
 │               ‾‾‾‾‾‾                                                       │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:99:14 ┘



┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `blah` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  expect blah == 1                                                          │
 │         ‾‾‾‾                                                               │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:103:9 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `some_func` in this scope. ───────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  some_func(                                                                │
 │  ‾‾‾‾‾‾‾‾‾                                                                 │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:114:2 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────────┐
│ UNRECOGNIZED SYNTAX ├─ I don't recognize this syntax. ──────────────────────┐
└┬────────────────────┘                                                       │
 │                                                                            │
 │  # Thnt!                                                                   │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_027.md:1:1 ┘

    This might be a syntax error, an unsupported language feature, or a typo.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `tag_with` is defined here and then never ──────┐
└┬────────────────┘  used.                                                    │
 │                                                                            │
 │  tag_with = Ok(number)                                                     │
 │  ‾‾‾‾‾‾‾‾                                                                  │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:119:2 ┘

    If you don't need this variable, prefix it with an underscore like
    `_tag_with` to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `ited` is defined here and then never used. ────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  ited = "Hello, ${world}"                                                  │
 │  ‾‾‾‾                                                                      │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:120:2 ┘

    If you don't need this variable, prefix it with an underscore like `_ited`
    to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `list` is defined here and then never used. ────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  list = [                                                                  │
 │  ‾‾‾‾                                                                      │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:121:2 ┘

    If you don't need this variable, prefix it with an underscore like `_list`
    to suppress this warning.


┌──────────────┐
│ TOO FEW ARGS ├─ The type List expects 1 argument, but got 0 instead. ───────┐
└┬─────────────┘                                                              │
 │                                                                            │
 │  List( #rg                                                                 │
 │  ),                                                                        │
 │                                                                            │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:21:3 ┘



┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  line : ( # Cpen                                                           │
 │      Bar, #                                                                │
 │      Baz, #m                                                               │
 │  ) # Co                                                                    │
 │                                                                            │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:28:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌───────────────┐
│ TYPE MISMATCH ├─ This `if` condition must evaluate to a `Bool` – either ────┐
└┬──────────────┘  `True` or `False`.                                         │
 │                                                                            │
 │  if num {                                                                  │
 │     ‾‾‾                                                                    │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:50:5 ┘

    It is:

        U64

    But I need this to be a `Bool` value.


┌───────────────┐
│ TYPE MISMATCH ├─ The `lue` binding in the second pattern of the first ──────┐
└┬──────────────┘  branch of this `match` does not match the same binding     │
 │                 in the first pattern.                                      │
 │                                                                            │
 │      match a {lue | Red => {                                               │
 │              x x                                                           │
 │          }                                                                 │
 │          Blue  => 1                                                        │
 │          "foo" => # ent                                                    │
 │  00                                                                        │
 │          "foo" | "bar" => 20[1, 2, 3, .. as rest] # Aftet                  │
 │              => ment                                                       │
 │                                                                            │
 │                                                                            │
 │          [1, 2 | 5, 3, .. as rest] => 123                                  │
 │          [                                                                 │
 │  ist                                                                       │
 │          ] => 123                                                          │
 │          3.14 => 314                                                       │
 │          3.14 | 6.28 => 314                                                │
 │          (1, 2, 3) => 123                                                  │
 │          (1, 2 | 5, 3) => 123                                              │
 │          { foo: 1, bar: 2, ..rest } => 12->add(34)                         │
 │          { # Afrd open                                                     │
 │              foo #                                                         │
 │                  : #ue                                                     │
 │                      1, # Aftd field                                       │
 │              bar: 2,                                                       │
 │              ..} => 12                                                     │
 │          { foo: 1, bar: 2 | 7 } => 12                                      │
 │          {                                                                 │
 │              foo: 1,                                                       │
 │              } => 12                                                       │
 │          Ok(123) => 121000                                                 │
 │      }                                                                     │
 │                                                                            │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:64:17 ┘

    In the second pattern, `lue` is:

        [Red, ..]

    But in the first pattern, `lue` is:

        [Red, ..]

    A name shared across `|` patterns in the same `match` branch must have one
    compatible type.


┌────────────────┐
│ MISSING METHOD ├─ This `from_quote` method is being called on a value ──────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  "foo" => # ent                                                            │
 │  ‾‾‾‾‾                                                                     │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:68:3 ┘

    The value's type, which does not have a method named `from_quote`, is:

        [Blue, Red, ..]


┌────────────────┐
│ MISSING METHOD ├─ This `from_quote` method is being called on a value ──────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  "foo" | "bar" => 20[1, 2, 3, .. as rest] # Aftet                          │
 │  ‾‾‾‾‾                                                                     │
 └──────────────────────────────────────────────────── fuzz_crash_027.md:70:3 ┘

    The value's type, which does not have a method named `from_quote`, is:

        [Blue, Red, ..]


┌───────────────┐
│ TYPE MISMATCH ├─ The fifth branch of this `match` does not match the ───────┐
└┬──────────────┘  previous ones.                                             │
 │                                                                            │
 │      match a {lue | Red => {                                               │
 │              x x                                                           │
 │          }                                                                 │
 │          Blue  => 1                                                        │
 │          "foo" => # ent                                                    │
 │  00                                                                        │
 │          "foo" | "bar" => 20[1, 2, 3, .. as rest] # Aftet                  │
 │              => ment                                                       │
 │                                                                            │
 │                                                                            │
 │          [1, 2 | 5, 3, .. as rest] => 123                                  │
 │          [                                                                 │
 │  ist                                                                       │
 │          ] => 123                                                          │
 │          3.14 => 314                                                       │
 │          3.14 | 6.28 => 314                                                │
 │          (1, 2, 3) => 123                                                  │
 │          (1, 2 | 5, 3) => 123                                              │
 │          { foo: 1, bar: 2, ..rest } => 12->add(34)                         │
 │          { # Afrd open                                                     │
 │              foo #                                                         │
 │                  : #ue                                                     │
 │                      1, # Aftd field                                       │
 │              bar: 2,                                                       │
 │              ..} => 12                                                     │
 │          { foo: 1, bar: 2 | 7 } => 12                                      │
 │          {                                                                 │
 │              foo: 1,                                                       │
 │              } => 12                                                       │
 │          Ok(123) => 121000                                                 │
 │      }                                                                     │
 │                                                                            │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:64:22 ┘

    This fifth branch is trying to match:

        List(d)
          where [
            d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]),
            d.is_eq : d, d -> Bool,
          ]

    But the expression between the `match` parenthesis has the type:

        [Blue, Red, ..]

    These can never match! Either the pattern or expression has a problem.


┌───────────────┐
│ TYPE MISMATCH ├─ This `return` does not match the function's return type. ──┐
└┬──────────────┘                                                             │
 │                                                                            │
 │  tag                                                                       │
 │  ‾‾‾                                                                       │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:106:3 ┘

    It has the type:

        [Blue, ..]

    But the function's return type is:

        Try({}, _d)

    Hint: All `return` statements and the final expression in a function must
    have the same type.


┌──────────────┐
│ TOO FEW ARGS ├─ The `match_time` function expects 2 arguments, but it got ──┐
└┬─────────────┘  1 instead.                                                  │
 │                                                                            │
 │  match_time(                                                               │
 │      ..., #                                                                │
 │  )                                                                         │
 │                                                                            │
 └─────────────────────────────────────────────────── fuzz_crash_027.md:111:2 ┘

    The `match_time` function has the type:

        [Blue, Red, ..], _arg -> Error

    Are there any missing commas?

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
		List(),
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
		[
			ist,
		] => 123
		3.14 => 314
		3.14 | 6.28 => 314
		(1, 2, 3) => 123
		(1, 2 | 5, 3) => 123
		{ foo: 1, bar: 2, ..rest } => 12->add(34)
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
		(e-anno-only)
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
									(e-runtime-error (tag "empty_tuple")))
								(e-num (value "0")))))
					(if-else
						(e-block
							(s-dbg
								(e-num (value "123")))
							(e-lookup-local
								(p-assign (ident "other"))))))))
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
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "a"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-assign (ident "lue")))
								(pattern (degenerate true)
									(p-applied-tag)))
							(value
								(e-block
									(s-expr
										(e-runtime-error (tag "ident_not_in_scope")))
									(e-runtime-error (tag "ident_not_in_scope")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-num (value "1"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-str (text "foo"))))
							(value
								(e-num (value "0"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-str (text "foo")))
								(pattern (degenerate false)
									(p-str (text "bar"))))
							(value
								(e-num (value "20"))))
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
								(e-runtime-error (tag "ident_not_in_scope"))))
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
											(p-assign (ident "ist"))))))
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
												(rest-pattern
													(p-assign (ident "rest"))))))))
							(value
								(e-call
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-num (value "12"))
									(e-num (value "34")))))
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
											(record-destruct (label "#others") (ident "#others")
												(rest-pattern
													(p-underscore)))))))
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
													(p-num (value "1"))))))))
							(value
								(e-num (value "12"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-num (value "121000")))))))))
	(d-let
		(p-assign (ident "main!"))
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
					(e-method-eq (negated "false")
						(lhs
							(e-runtime-error (tag "ident_not_in_scope")))
						(rhs
							(e-num (value "1")))))
				(s-let
					(p-assign (ident "tag"))
					(e-tag (name "Blue")))
				(s-return
					(e-lookup-local
						(p-assign (ident "tag"))))
				(s-expr
					(e-not-implemented))
				(s-expr
					(e-call (constraint-fn-var 3592)
						(e-lookup-local
							(p-assign (ident "match_time")))
						(e-not-implemented)))
				(s-expr
					(e-call
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-dbg
							(e-num (value "42")))))
				(s-crash (msg "Unreachtement"))
				(s-let
					(p-assign (ident "tag_with"))
					(e-tag (name "Ok")
						(args
							(e-lookup-local
								(p-assign (ident "number"))))))
				(s-let
					(p-assign (ident "ited"))
					(e-block
						(s-let
							(p-assign (ident "#interp_0"))
							(e-lookup-local
								(p-assign (ident "world"))))
						(e-interpolation (constraint-fn-var 3683)
							(first
								(e-literal (string "Hello, ")))
							(parts
								(e-lookup-local
									(p-assign (ident "#interp_0")))
								(e-literal (string ""))))))
				(s-let
					(p-assign (ident "list"))
					(e-runtime-error (tag "expr_not_canonicalized")))
				(e-empty_record)))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-malformed))
				(ty-apply (name "Try") (builtin)
					(ty-record)
					(ty-underscore)))))
	(s-import (module "pf.Stdout")
		(exposes
			(exposed (name "line!") (wildcard false))
			(exposed (name "e!") (wildcard false))))
	(s-import (module "Stdot")
		(exposes))
	(s-import (module "pkg.S")
		(exposes
			(exposed (name "func") (alias "fry") (wildcard false))
			(exposed (name "Custom") (wildcard true))))
	(s-import (module "Bae")
		(exposes))
	(s-import (module "Ba")
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
		(e-method-eq (negated "false")
			(lhs
				(e-runtime-error (tag "ident_not_in_scope")))
			(rhs
				(e-num (value "1"))))))
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
		(alias (type "MapML(a, b)")
			(ty-header (name "MapML")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(alias (type "Foo")
			(ty-header (name "Foo")))
		(alias (type "Some(a)")
			(ty-header (name "Some")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Ml(a)")
			(ty-header (name "Ml")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Soine(a)")
			(ty-header (name "Soine")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Func(a)")
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
