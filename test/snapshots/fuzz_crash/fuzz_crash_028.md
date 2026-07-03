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

import p

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
add_ne = |num| {
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
		"foo" | "bar" => 20[1, 2, 3, .. as rest] # t
			=> ment
		[1, 2 | 5, 3, .. as rest] => 123
		[
		] => 1	3.14 => 314
		3.14 | 6.28 => 314
		(1, 2, 3) => 123
		(1, 2 | 5, 3) => 123
		{ foo: 1, bar: 2, ..rest } => 12->add(34)
		{ # Afpen
oo #
				: #ue
	1, #eld
ar: 2,
			..} => 12
		{ foo: 1, bar: 2 | 7 } => 12
		{
	o: 1,
			} =>212
		Ok(123) => 12
	}

expect # Cord
	blah == 1 # nt

main! : (String) -> Result({}, _)
ma= |_| { # Yee
	world = "d"
	var number = 123
	expect blah == 1
	tag = Blue
	return #d
		tag  Jus
	...
	match_time(
		...
	)
nc(
		dbg # bug
2,
	)
	crash "Unrnt
	tag_ = Ok(number)
	i= "H, ${world}"
t = [
		add_one(dbg # Afist
er, # afarg
		),	456, # ee
	]
	for n in list {
	line!("Ag ${n} to ${er}")
		+ n
	}
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
	tuple = (123, "World", tag, Ok(world), (nd, tuple), [1, 2, 3])
	mle = (
		123,
		"World",ag1,
		Ok(world), # nt
		(ne, tuple),
		[1, 2, 3],
	)
	b = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
le =(arg1)?.od()?.ned()?.recd?
	line!(
		"Ho${ #
			r(number) # xpr
		} ",
	)
} # Cocl

y : {}
e = {}

t : V((a,c))

expect {
	f= 1
h == foo
}
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
ASCII CONTROL CHARACTER - :0:0:0:0
LEADING ZERO - :0:0:0:0
UNCLOSED STRING - fuzz_crash_028.md:111:8:111:14
PARSE ERROR - fuzz_crash_028.md:10:1:10:7
PARSE ERROR - fuzz_crash_028.md:12:12:12:14
PARSE ERROR - fuzz_crash_028.md:13:1:13:7
PARSE ERROR - fuzz_crash_028.md:15:1:15:4
PARSE ERROR - fuzz_crash_028.md:15:4:15:5
PARSE ERROR - fuzz_crash_028.md:15:5:15:6
PARSE ERROR - fuzz_crash_028.md:15:6:15:7
PARSE ERROR - fuzz_crash_028.md:15:8:15:9
PARSE ERROR - fuzz_crash_028.md:15:9:15:10
PARSE ERROR - fuzz_crash_028.md:15:11:15:12
PARSE ERROR - fuzz_crash_028.md:15:20:15:21
PARSE ERROR - fuzz_crash_028.md:15:22:15:23
PARSE ERROR - fuzz_crash_028.md:15:23:15:24
PARSE ERROR - fuzz_crash_028.md:15:25:15:27
PARSE ERROR - fuzz_crash_028.md:15:28:15:29
PARSE ERROR - fuzz_crash_028.md:15:29:15:30
PARSE ERROR - fuzz_crash_028.md:15:31:15:33
PARSE ERROR - fuzz_crash_028.md:16:1:16:6
PARSE ERROR - fuzz_crash_028.md:16:6:16:7
PARSE ERROR - fuzz_crash_028.md:17:2:17:3
PARSE ERROR - fuzz_crash_028.md:17:3:17:4
PARSE ERROR - fuzz_crash_028.md:18:2:18:3
PARSE ERROR - fuzz_crash_028.md:18:3:18:4
PARSE ERROR - fuzz_crash_028.md:19:1:19:2
PARSE ERROR - fuzz_crash_028.md:20:2:20:3
PARSE ERROR - fuzz_crash_028.md:22:4:22:5
PARSE ERROR - fuzz_crash_028.md:23:3:23:4
PARSE ERROR - fuzz_crash_028.md:23:4:23:5
PARSE ERROR - fuzz_crash_028.md:23:6:23:8
PARSE ERROR - fuzz_crash_028.md:23:9:23:10
PARSE ERROR - fuzz_crash_028.md:23:10:23:11
PARSE ERROR - fuzz_crash_028.md:23:12:23:14
PARSE ERROR - fuzz_crash_028.md:26:1:26:4
PARSE ERROR - fuzz_crash_028.md:26:5:26:6
PARSE ERROR - fuzz_crash_028.md:26:7:26:8
PARSE ERROR - fuzz_crash_028.md:26:11:26:12
PARSE ERROR - fuzz_crash_028.md:26:16:26:17
PARSE ERROR - fuzz_crash_028.md:40:5:40:6
PARSE ERROR - fuzz_crash_028.md:40:7:40:8
PARSE ERROR - fuzz_crash_028.md:40:9:40:10
PARSE ERROR - fuzz_crash_028.md:41:1:41:2
PARSE ERROR - fuzz_crash_028.md:48:1:48:5
MODULE NOT FOUND - fuzz_crash_028.md:6:1:8:4
UNDECLARED TYPE - fuzz_crash_028.md:29:2:29:5
UNDECLARED TYPE - fuzz_crash_028.md:30:2:30:5
UNDECLARED TYPE - fuzz_crash_028.md:32:19:32:21
UNDECLARED TYPE VARIABLE - fuzz_crash_028.md:32:32:32:33
UNDECLARED TYPE - fuzz_crash_028.md:34:8:34:11
UNDECLARED TYPE - fuzz_crash_028.md:38:8:38:11
UNDECLARED TYPE - fuzz_crash_028.md:43:11:43:16
UNDECLARED TYPE - fuzz_crash_028.md:43:26:43:31
EMPTY TUPLE NOT ALLOWED - fuzz_crash_028.md:52:1:52:3
NAME NOT IN SCOPE - fuzz_crash_028.md:65:4:65:5
NAME NOT IN SCOPE - fuzz_crash_028.md:65:6:65:7
NAME NOT IN SCOPE - fuzz_crash_028.md:71:7:71:11
UNUSED VARIABLE - fuzz_crash_028.md:1:1:1:1
NOT IMPLEMENTED - fuzz_crash_028.md:72:7:72:12
UNUSED VARIABLE - fuzz_crash_028.md:1:1:1:1
NOT IMPLEMENTED - fuzz_crash_028.md:77:7:77:12
NAME NOT IN SCOPE - fuzz_crash_028.md:78:37:78:40
UNUSED VARIABLE - fuzz_crash_028.md:78:21:78:27
NOT IMPLEMENTED - fuzz_crash_028.md:85:18:85:23
UNUSED VARIABLE - fuzz_crash_028.md:62:2:62:3
NAME NOT IN SCOPE - fuzz_crash_028.md:93:2:93:6
UNDECLARED TYPE - fuzz_crash_028.md:95:10:95:16
UNDECLARED TYPE - fuzz_crash_028.md:95:21:95:27
NAME NOT IN SCOPE - fuzz_crash_028.md:99:9:99:13
NAME NOT IN SCOPE - fuzz_crash_028.md:107:1:107:3
NAME NOT IN SCOPE - fuzz_crash_028.md:116:1:116:3
NAME NOT IN SCOPE - fuzz_crash_028.md:119:11:119:15
NAME NOT IN SCOPE - fuzz_crash_028.md:120:2:120:7
NAME NOT IN SCOPE - fuzz_crash_028.md:120:22:120:24
NAME NOT IN SCOPE - fuzz_crash_028.md:123:54:123:57
NAME NOT IN SCOPE - fuzz_crash_028.md:124:42:124:44
INVALID ASSIGNMENT TO ITSELF - fuzz_crash_028.md:124:46:124:51
NAME NOT IN SCOPE - fuzz_crash_028.md:127:11:127:14
NAME NOT IN SCOPE - fuzz_crash_028.md:132:10:132:13
NAME NOT IN SCOPE - fuzz_crash_028.md:133:6:133:10
NAME NOT IN SCOPE - fuzz_crash_028.md:134:2:134:7
NAME NOT IN SCOPE - fuzz_crash_028.md:136:4:136:5
UNUSED VARIABLE - fuzz_crash_028.md:112:2:112:6
UNUSED VARIABLE - fuzz_crash_028.md:113:2:113:3
UNUSED VARIABLE - fuzz_crash_028.md:114:1:114:2
UNUSED VARIABLE - fuzz_crash_028.md:123:2:123:4
UNUSED VARIABLE - fuzz_crash_028.md:125:2:125:5
UNUSED VARIABLE - fuzz_crash_028.md:132:2:132:3
UNUSED VARIABLE - fuzz_crash_028.md:133:1:133:3
UNDECLARED TYPE - fuzz_crash_028.md:144:5:144:6
NAME NOT IN SCOPE - fuzz_crash_028.md:148:1:148:2
NAME NOT IN SCOPE - fuzz_crash_028.md:148:6:148:9
UNUSED VARIABLE - fuzz_crash_028.md:147:2:147:3
EXPOSED BUT NOT DEFINED - fuzz_crash_028.md:2:6:2:11
DECLARATION HAS NO VALUE - fuzz_crash_028.md:28:1:31:2
DECLARATION HAS NO VALUE - fuzz_crash_028.md:47:1:47:21
TYPE MISMATCH - fuzz_crash_028.md:64:2:64:2
MISSING METHOD - fuzz_crash_028.md:68:3:68:8
MISSING METHOD - fuzz_crash_028.md:70:3:70:8
TYPE MISMATCH - fuzz_crash_028.md:64:2:64:2
DECLARATION HAS NO VALUE - fuzz_crash_028.md:95:1:95:34
TYPE MISMATCH - fuzz_crash_028.md:102:8:102:11
TOO FEW ARGS - fuzz_crash_028.md:104:2:106:3
TYPE MISMATCH - fuzz_crash_028.md:115:3:115:3
TYPE MISMATCH - fuzz_crash_028.md:133:5:133:12
DECLARATION HAS NO VALUE - fuzz_crash_028.md:141:1:141:7
DECLARATION HAS NO VALUE - fuzz_crash_028.md:144:1:144:13
MISSING METHOD - fuzz_crash_028.md:133:5:133:12
MISSING METHOD - fuzz_crash_028.md:133:5:133:18
# PROBLEMS

ASCII CONTROL CHARACTER

ASCII control characters are not allowed in Roc source code.



ASCII CONTROL CHARACTER

ASCII control characters are not allowed in Roc source code.



LEADING ZERO

Numbers cannot have leading zeros.



┌─────────────────┐
│ UNCLOSED STRING ├─ This string is missing a closing quote. ─────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  crash "Unrnt                                                              │
 │        ‾‾‾‾‾‾                                                              │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:111:8 ┘



┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: incomplete_import ─────────────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  import p                                                                 │
 │  ‾‾‾‾‾‾                                                                    │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:10:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  import Bae as Gooe                                                        │
 │             ‾‾                                                             │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:12:12 ┘

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
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  import                                                                    │
 │  ‾‾‾‾‾‾                                                                    │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:13:1 ┘

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
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  Map(a, b) : List(a), (a -> b) -> List(b)                                  │
 │  ‾‾‾                                                                       │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:15:1 ┘

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
 │  Map(a, b) : List(a), (a -> b) -> List(b)                                  │
 │     ‾                                                                      │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:15:4 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Map(a, b) : List(a), (a -> b) -> List(b)                                  │
 │      ‾                                                                     │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:15:5 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Map(a, b) : List(a), (a -> b) -> List(b)                                  │
 │       ‾                                                                    │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:15:6 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Map(a, b) : List(a), (a -> b) -> List(b)                                  │
 │         ‾                                                                  │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:15:8 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Map(a, b) : List(a), (a -> b) -> List(b)                                  │
 │          ‾                                                                 │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:15:9 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Map(a, b) : List(a), (a -> b) -> List(b)                                  │
 │            ‾                                                               │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:15:11 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  Map(a, b) : List(a), (a -> b) -> List(b)                                  │
 │                     ‾                                                      │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:15:20 ┘

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
 │  Map(a, b) : List(a), (a -> b) -> List(b)                                  │
 │                       ‾                                                    │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:15:22 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Map(a, b) : List(a), (a -> b) -> List(b)                                  │
 │                        ‾                                                   │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:15:23 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Function types with multiple arrows need parentheses. ───────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Map(a, b) : List(a), (a -> b) -> List(b)                                  │
 │                          ‾‾                                                │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:15:25 ┘

    Instead of writing a -> b -> c, use parentheses to clarify which you mean:
            a -> (b -> c) for a curried function (a function that returns
            another function)
            (a -> b) -> c for a higher-order function (a function that takes
            another function)


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Map(a, b) : List(a), (a -> b) -> List(b)                                  │
 │                             ‾                                              │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:15:28 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Map(a, b) : List(a), (a -> b) -> List(b)                                  │
 │                              ‾                                             │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:15:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Function types with multiple arrows need parentheses. ───────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Map(a, b) : List(a), (a -> b) -> List(b)                                  │
 │                                ‾‾                                          │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:15:31 ┘

    Instead of writing a -> b -> c, use parentheses to clarify which you mean:
            a -> (b -> c) for a curried function (a function that returns
            another function)
            (a -> b) -> c for a higher-order function (a function that takes
            another function)


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  MapML( # Cere                                                             │
 │  ‾‾‾‾‾                                                                     │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:16:1 ┘

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
 │  MapML( # Cere                                                             │
 │       ‾                                                                    │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:16:6 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  a, # Anre                                                                 │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:17:2 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  a, # Anre                                                                 │
 │   ‾                                                                        │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:17:3 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  b,                                                                        │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:18:2 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  b,                                                                        │
 │   ‾                                                                        │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:18:3 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  ) # Ag                                                                    │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:19:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  : # Aon                                                                   │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:20:2 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  ),                                                                        │
 │   ‾                                                                        │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:22:4 ┘

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
 │  (a -> b) -> # row                                                         │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:23:3 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  (a -> b) -> # row                                                         │
 │   ‾                                                                        │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:23:4 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Function types with multiple arrows need parentheses. ───────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  (a -> b) -> # row                                                         │
 │     ‾‾                                                                     │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:23:6 ┘

    Instead of writing a -> b -> c, use parentheses to clarify which you mean:
            a -> (b -> c) for a curried function (a function that returns
            another function)
            (a -> b) -> c for a higher-order function (a function that takes
            another function)


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  (a -> b) -> # row                                                         │
 │        ‾                                                                   │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:23:9 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  (a -> b) -> # row                                                         │
 │         ‾                                                                  │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:23:10 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Function types with multiple arrows need parentheses. ───────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  (a -> b) -> # row                                                         │
 │           ‾‾                                                               │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:23:12 ┘

    Instead of writing a -> b -> c, use parentheses to clarify which you mean:
            a -> (b -> c) for a curried function (a function that returns
            another function)
            (a -> b) -> c for a higher-order function (a function that takes
            another function)


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  Foo : (Bar, Baz)                                                          │
 │  ‾‾‾                                                                       │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:26:1 ┘

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
 │  Foo : (Bar, Baz)                                                          │
 │      ‾                                                                     │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:26:5 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Foo : (Bar, Baz)                                                          │
 │        ‾                                                                   │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:26:7 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  Foo : (Bar, Baz)                                                          │
 │            ‾                                                               │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:26:11 ┘

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
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  Foo : (Bar, Baz)                                                          │
 │                 ‾                                                          │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:26:16 ┘

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
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  Maya) : [ #                                                               │
 │      ‾                                                                     │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:40:5 ┘

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
 └──────────────────────────────────────────────────── fuzz_crash_028.md:40:7 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Maya) : [ #                                                               │
 │          ‾                                                                 │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:40:9 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  ] #se                                                                     │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:41:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  add_ne = |num| {                                                         │
 │  ‾‾‾‾                                                                      │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:48:1 ┘

    This is an unexpected parsing error. Please check your syntax.


┌──────────────────┐
│ MODULE NOT FOUND ├─ The module `Stdot` was not found in this Roc project. ──┐
└┬─────────────────┘                                                          │
 │                                                                            │
 │  import Stdot                                                              │
 │          exposing [ #tem                                                   │
 │          ] # Cose                                                          │
 │                                                                            │
 └───────────────────────────────────────────────────── fuzz_crash_028.md:6:1 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Bar` is not declared in this scope. ───────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Bar, #                                                                    │
 │  ‾‾‾                                                                       │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:29:2 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Baz` is not declared in this scope. ───────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Baz, #m                                                                   │
 │  ‾‾‾                                                                       │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:30:2 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Ok` is not declared in this scope. ────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Some(a) : { foo : Ok(a), bar : g }                                        │
 │                    ‾‾                                                      │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:32:19 ┘



┌──────────────────────────┐
│ UNDECLARED TYPE VARIABLE ├─ The type variable `g` is not declared in this ──┐
└┬─────────────────────────┘  scope.                                          │
 │                                                                            │
 │  Some(a) : { foo : Ok(a), bar : g }                                        │
 │                                 ‾                                          │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:32:32 ┘

    Type variables must be introduced in a type annotation before they can be
    used.


┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Som` is not declared in this scope. ───────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  bar : Som# Afld                                                           │
 │        ‾‾‾                                                                 │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:34:8 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Som` is not declared in this scope. ───────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  bar : Som                                                                 │
 │        ‾‾‾                                                                 │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:38:8 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Maybe` is not declared in this scope. ─────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Func(a) : Maybe(a), a -> Maybe(a)                                         │
 │            ‾‾‾‾‾                                                           │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:43:11 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Maybe` is not declared in this scope. ─────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Func(a) : Maybe(a), a -> Maybe(a)                                         │
 │                           ‾‾‾‾‾                                            │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:43:26 ┘



┌─────────────────────────┐
│ EMPTY TUPLE NOT ALLOWED ├─ I am part way through parsing this tuple, but ───┐
└┬────────────────────────┘  it is empty.                                     │
 │                                                                            │
 │  () #r                                                                     │
 │  ‾‾                                                                        │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:52:1 ┘

    If you want to represent nothing, try using an empty record: `{}`.


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `x` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  x x                                                                       │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:65:4 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `x` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  x x                                                                       │
 │    ‾                                                                       │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:65:6 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `ment` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  => ment                                                                   │
 │     ‾‾‾‾                                                                   │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:71:7 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `rest` is defined here and then never used. ────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  # Thnt!                                                                   │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_028.md:1:1 ┘

    If you don't need this variable, prefix it with an underscore like `_rest`
    to suppress this warning.


┌─────────────────┐
│ NOT IMPLEMENTED ├─ This feature is not yet implemented: alternatives ───────┐
└┬────────────────┘  pattern outside match expression.                        │
 │                                                                            │
 │  [1, 2 | 5, 3, .. as rest] => 123                                          │
 │      ‾‾‾‾‾                                                                 │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:72:7 ┘

    This error doesn't have a proper diagnostic report yet. Let us know if you
    want to help improve Roc's error messages!


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `rest` is defined here and then never used. ────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  # Thnt!                                                                   │
 │  ‾                                                                         │
 └───────────────────────────────────────────────────── fuzz_crash_028.md:1:1 ┘

    If you don't need this variable, prefix it with an underscore like `_rest`
    to suppress this warning.


┌─────────────────┐
│ NOT IMPLEMENTED ├─ This feature is not yet implemented: alternatives ───────┐
└┬────────────────┘  pattern outside match expression.                        │
 │                                                                            │
 │  (1, 2 | 5, 3) => 123                                                      │
 │      ‾‾‾‾‾                                                                 │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:77:7 ┘

    This error doesn't have a proper diagnostic report yet. Let us know if you
    want to help improve Roc's error messages!


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `add` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  { foo: 1, bar: 2, ..rest } => 12->add(34)                                 │
 │                                    ‾‾‾                                     │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:78:37 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `rest` is defined here and then never used. ────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  { foo: 1, bar: 2, ..rest } => 12->add(34)                                 │
 │                    ‾‾‾‾‾‾                                                  │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:78:21 ┘

    If you don't need this variable, prefix it with an underscore like `_rest`
    to suppress this warning.


┌─────────────────┐
│ NOT IMPLEMENTED ├─ This feature is not yet implemented: alternatives ───────┐
└┬────────────────┘  pattern outside match expression.                        │
 │                                                                            │
 │  { foo: 1, bar: 2 | 7 } => 12                                              │
 │                 ‾‾‾‾‾                                                      │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:85:18 ┘

    This error doesn't have a proper diagnostic report yet. Let us know if you
    want to help improve Roc's error messages!


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `b` is defined here and then never used. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  b,                                                                        │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:62:2 ┘

    If you don't need this variable, prefix it with an underscore like `_b` to
    suppress this warning.


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `blah` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  blah == 1 # nt                                                            │
 │  ‾‾‾‾                                                                      │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:93:2 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `String` is not declared in this scope. ────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  main! : (String) -> Result({}, _)                                         │
 │           ‾‾‾‾‾‾                                                           │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:95:10 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Result` is not declared in this scope. ────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  main! : (String) -> Result({}, _)                                         │
 │                      ‾‾‾‾‾‾                                                │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:95:21 ┘



┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `blah` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  expect blah == 1                                                          │
 │         ‾‾‾‾                                                               │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:99:9 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `nc` in this scope. ──────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  nc(                                                                       │
 │  ‾‾                                                                        │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:107:1 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `er` in this scope. ──────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  er, # afarg                                                               │
 │  ‾‾                                                                        │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:116:1 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `list` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  for n in list {                                                           │
 │           ‾‾‾‾                                                             │
 └────────────────────────────────────────────────── fuzz_crash_028.md:119:11 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `line!` in this scope. ───────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  line!("Ag ${n} to ${er}")                                                 │
 │  ‾‾‾‾‾                                                                     │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:120:2 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `er` in this scope. ──────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  line!("Ag ${n} to ${er}")                                                 │
 │                      ‾‾                                                    │
 └────────────────────────────────────────────────── fuzz_crash_028.md:120:22 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `ned` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }                 │
 │                                                      ‾‾‾                   │
 └────────────────────────────────────────────────── fuzz_crash_028.md:123:54 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `nd` in this scope. ──────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  tuple = (123, "World", tag, Ok(world), (nd, tuple), [1, 2, 3])            │
 │                                          ‾‾                                │
 └────────────────────────────────────────────────── fuzz_crash_028.md:124:42 ┘

    Is it misspelled, or is there an import missing?


┌──────────────────────────────┐
│ INVALID ASSIGNMENT TO ITSELF ├─ The value `tuple` is assigned to itself, ───┐
└┬─────────────────────────────┘  which would cause an infinite loop at       │
 │                                runtime.                                    │
 │                                                                            │
 │  tuple = (123, "World", tag, Ok(world), (nd, tuple), [1, 2, 3])            │
 │                                              ‾‾‾‾‾                         │
 └────────────────────────────────────────────────── fuzz_crash_028.md:124:46 ┘

    Only functions can reference themselves (for recursion). For non-function
    values, the right-hand side must be fully computable without referring to
    the value being assigned.


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `ag1` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  "World",ag1,                                                              │
 │          ‾‾‾                                                               │
 └────────────────────────────────────────────────── fuzz_crash_028.md:127:11 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `foo` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  b = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5  │
 │          ‾‾‾                                                               │
 └────────────────────────────────────────────────── fuzz_crash_028.md:132:10 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `arg1` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  le =(arg1)?.od()?.ned()?.recd?                                            │
 │       ‾‾‾‾                                                                 │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:133:6 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `line!` in this scope. ───────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  line!(                                                                    │
 │  ‾‾‾‾‾                                                                     │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:134:2 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `r` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  r(number) # xpr                                                           │
 │  ‾                                                                         │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:136:4 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `tag_` is defined here and then never used. ────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  tag_ = Ok(number)                                                         │
 │  ‾‾‾‾                                                                      │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:112:2 ┘

    If you don't need this variable, prefix it with an underscore like `_tag_`
    to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `i` is defined here and then never used. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  i= "H, ${world}"                                                          │
 │  ‾                                                                         │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:113:2 ┘

    If you don't need this variable, prefix it with an underscore like `_i` to
    suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `t` is defined here and then never used. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  t = [                                                                     │
 │  ‾                                                                         │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:114:1 ┘

    If you don't need this variable, prefix it with an underscore like `_t` to
    suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `rd` is defined here and then never used. ──────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }                 │
 │  ‾‾                                                                        │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:123:2 ┘

    If you don't need this variable, prefix it with an underscore like `_rd` to
    suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `mle` is defined here and then never used. ─────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  mle = (                                                                   │
 │  ‾‾‾                                                                       │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:125:2 ┘

    If you don't need this variable, prefix it with an underscore like `_mle`
    to suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `b` is defined here and then never used. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  b = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5  │
 │  ‾                                                                         │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:132:2 ┘

    If you don't need this variable, prefix it with an underscore like `_b` to
    suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `le` is defined here and then never used. ──────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  le =(arg1)?.od()?.ned()?.recd?                                            │
 │  ‾‾                                                                        │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:133:1 ┘

    If you don't need this variable, prefix it with an underscore like `_le` to
    suppress this warning.


┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `V` is not declared in this scope. ─────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  t : V((a,c))                                                              │
 │      ‾                                                                     │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:144:5 ┘



┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `h` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  h == foo                                                                  │
 │  ‾                                                                         │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:148:1 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `foo` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  h == foo                                                                  │
 │       ‾‾‾                                                                  │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:148:6 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `f` is defined here and then never used. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  f= 1                                                                      │
 │  ‾                                                                         │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:147:2 ┘

    If you don't need this variable, prefix it with an underscore like `_f` to
    suppress this warning.


┌─────────────────────────┐
│ EXPOSED BUT NOT DEFINED ├─ The module header says that `main!` is ──────────┐
└┬────────────────────────┘  exposed, but it is not defined anywhere in       │
 │                           this module.                                     │
 │                                                                            │
 │  app [main!] { pf: platform "c" }                                          │
 │       ‾‾‾‾‾                                                                │
 └───────────────────────────────────────────────────── fuzz_crash_028.md:2:6 ┘

    You can fix this by either defining `main!` in this module, or by removing
    it from the list of exposed values.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  line : ( # Cpen                                                           │
 │      Bar, #                                                                │
 │      Baz, #m                                                               │
 │  ) # Co                                                                    │
 │                                                                            │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:28:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  add_one : U64 -> U64                                                      │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                      │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:47:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


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
 │          "foo" | "bar" => 20[1, 2, 3, .. as rest] # t                      │
 │              => ment                                                       │
 │          [1, 2 | 5, 3, .. as rest] => 123                                  │
 │          [                                                                 │
 │          ] => 1 3.14 => 314                                                │
 │          3.14 | 6.28 => 314                                                │
 │          (1, 2, 3) => 123                                                  │
 │          (1, 2 | 5, 3) => 123                                              │
 │          { foo: 1, bar: 2, ..rest } => 12->add(34)                         │
 │          { # Afpen                                                         │
 │  oo #                                                                      │
 │                  : #ue                                                     │
 │      1, #eld                                                               │
 │  ar: 2,                                                                    │
 │              ..} => 12                                                     │
 │          { foo: 1, bar: 2 | 7 } => 12                                      │
 │          {                                                                 │
 │      o: 1,                                                                 │
 │              } =>212                                                       │
 │          Ok(123) => 12                                                     │
 │      }                                                                     │
 │                                                                            │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:64:17 ┘

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
 └──────────────────────────────────────────────────── fuzz_crash_028.md:68:3 ┘

    The value's type, which does not have a method named `from_quote`, is:

        [Blue, Red, ..]


┌────────────────┐
│ MISSING METHOD ├─ This `from_quote` method is being called on a value ──────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  "foo" | "bar" => 20[1, 2, 3, .. as rest] # t                              │
 │  ‾‾‾‾‾                                                                     │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:70:3 ┘

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
 │          "foo" | "bar" => 20[1, 2, 3, .. as rest] # t                      │
 │              => ment                                                       │
 │          [1, 2 | 5, 3, .. as rest] => 123                                  │
 │          [                                                                 │
 │          ] => 1 3.14 => 314                                                │
 │          3.14 | 6.28 => 314                                                │
 │          (1, 2, 3) => 123                                                  │
 │          (1, 2 | 5, 3) => 123                                              │
 │          { foo: 1, bar: 2, ..rest } => 12->add(34)                         │
 │          { # Afpen                                                         │
 │  oo #                                                                      │
 │                  : #ue                                                     │
 │      1, #eld                                                               │
 │  ar: 2,                                                                    │
 │              ..} => 12                                                     │
 │          { foo: 1, bar: 2 | 7 } => 12                                      │
 │          {                                                                 │
 │      o: 1,                                                                 │
 │              } =>212                                                       │
 │          Ok(123) => 12                                                     │
 │      }                                                                     │
 │                                                                            │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:64:22 ┘

    This fifth branch is trying to match:

        List(d)
          where [
            d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]),
            d.is_eq : d, d -> Bool,
          ]

    But the expression between the `match` parenthesis has the type:

        [Blue, Red, ..]

    These can never match! Either the pattern or expression has a problem.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  main! : (String) -> Result({}, _)                                         │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                         │
 └──────────────────────────────────────────────────── fuzz_crash_028.md:95:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌───────────────┐
│ TYPE MISMATCH ├─ This expression produces a value, but it's not being ──────┐
└┬──────────────┘  used.                                                      │
 │                                                                            │
 │  tag  Jus                                                                  │
 │       ‾‾‾                                                                  │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:102:8 ┘

    It has the type:

        [Jus, ..]

    Since this expression is used as a statement, it must evaluate to `{}`.
    If you don't need the value, you can ignore it with `_ =`.


┌──────────────┐
│ TOO FEW ARGS ├─ The `match_time` function expects 2 arguments, but it got ──┐
└┬─────────────┘  1 instead.                                                  │
 │                                                                            │
 │  match_time(                                                               │
 │      ...                                                                   │
 │  )                                                                         │
 │                                                                            │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:104:2 ┘

    The `match_time` function has the type:

        [Blue, Red, ..], _arg -> Error

    Are there any missing commas?


┌───────────────┐
│ TYPE MISMATCH ├─ The first argument being passed to this function has the ──┐
└┬──────────────┘  wrong type.                                                │
 │                                                                            │
 │          add_one(dbg # Afist                                               │
 │  er, # afarg                                                               │
 │          ), 456, # ee                                                      │
 │                                                                            │
 └────────────────────────────────────────────────── fuzz_crash_028.md:115:11 ┘

    This argument has the type:

        {}

    But `add_one` needs the first argument to be:

        U64


┌───────────────┐
│ TYPE MISMATCH ├─ This `?` may return early with a type that doesn't match ──┐
└┬──────────────┘  the function body.                                         │
 │                                                                            │
 │  le =(arg1)?.od()?.ned()?.recd?                                            │
 │      ‾‾‾‾‾‾‾                                                               │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:133:5 ┘

    On error, this would return:

        Try(ok, err)

    But the function body evaluates to:

        [Blue, ..]

    Hint: The error types from all `?` operators and the function body must be
    compatible since any of them could be the actual return value.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  y : {}                                                                    │
 │  ‾‾‾‾‾‾                                                                    │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:141:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  t : V((a,c))                                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾                                                              │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:144:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌────────────────┐
│ MISSING METHOD ├─ This is trying to dispatch a method named `od` on an ─────┐
└┬───────────────┘  unresolved type variable, but unresolved type variables   │
 │                  have no methods.                                          │
 │                                                                            │
 │  le =(arg1)?.od()?.ned()?.recd?                                            │
 │      ‾‾‾‾‾‾‾                                                               │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:133:5 ┘

    Hint: You can replace this static dispatch call with an ordinary function
    call, or force the type variable to become more concrete—for example, by
    adding a type annotation that narrows its type to something that actually
    has methods.


┌────────────────┐
│ MISSING METHOD ├─ This is trying to dispatch a method named `ned` on an ────┐
└┬───────────────┘  unresolved type variable, but unresolved type variables   │
 │                  have no methods.                                          │
 │                                                                            │
 │  le =(arg1)?.od()?.ned()?.recd?                                            │
 │      ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                         │
 └─────────────────────────────────────────────────── fuzz_crash_028.md:133:5 ┘

    Hint: You can replace this static dispatch call with an ordinary function
    call, or force the type variable to become more concrete—for example, by
    adding a type annotation that narrows its type to something that actually
    has methods.

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,
KwImport,UpperIdent,
KwExposing,OpenSquare,
CloseSquare,
KwImport,LowerIdent,
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
LowerIdent,LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
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
CloseSquare,OpFatArrow,Int,Float,OpFatArrow,Int,
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
LowerIdent,OpColon,OpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,Underscore,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
KwVar,LowerIdent,OpAssign,Int,
KwExpect,LowerIdent,OpEquals,Int,
LowerIdent,OpAssign,UpperIdent,
KwReturn,
LowerIdent,UpperIdent,
TripleDot,
LowerIdent,NoSpaceOpenRound,
TripleDot,
CloseRound,
LowerIdent,NoSpaceOpenRound,
KwDbg,
Int,Comma,
CloseRound,
KwCrash,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,OpAssign,OpenSquare,
LowerIdent,NoSpaceOpenRound,KwDbg,
LowerIdent,Comma,
CloseRound,Comma,Int,Comma,
CloseSquare,
KwFor,LowerIdent,KwIn,LowerIdent,OpenCurly,
LowerIdent,NoSpaceOpenRound,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,CloseRound,
OpPlus,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,CloseCurly,
LowerIdent,OpAssign,OpenRound,Int,Comma,StringStart,StringPart,StringEnd,Comma,LowerIdent,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpenRound,
Int,Comma,
StringStart,StringPart,StringEnd,Comma,LowerIdent,Comma,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,
OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,Comma,
CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpDoubleQuestion,Int,OpGreaterThan,Int,OpStar,Int,OpOr,Int,OpPlus,Int,OpLessThan,Int,OpAnd,Int,OpBinaryMinus,Int,OpGreaterThanOrEq,Int,OpOr,Int,OpLessThanOrEq,Int,OpSlash,Int,
LowerIdent,OpAssign,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpQuestion,
LowerIdent,NoSpaceOpenRound,
StringStart,StringPart,OpenStringInterpolation,
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseStringInterpolation,StringPart,StringEnd,Comma,
CloseRound,
CloseCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,
LowerIdent,OpAssign,OpenCurly,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,CloseRound,
KwExpect,OpenCurly,
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
		(s-malformed (tag "incomplete_import"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
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
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "ne"))
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
							(p-list)
							(e-int (raw "1")))
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
								(field (name "oo") (rest false)
									(p-int (raw "1")))
								(field (name "ar") (rest false)
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
								(field (name "o") (rest false)
									(p-int (raw "1"))))
							(e-int (raw "212")))
						(branch
							(p-tag (raw "Ok")
								(p-int (raw "123")))
							(e-int (raw "12")))))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "blah"))
				(e-int (raw "1"))))
		(s-type-anno (name "main!")
			(ty-fn
				(ty (name "String"))
				(ty-apply
					(ty (name "Result"))
					(ty-record)
					(_))))
		(s-decl
			(p-ident (raw "ma"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "world"))
							(e-string
								(e-string-part (raw "d"))))
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
						(e-tag (raw "Jus"))
						(e-ellipsis)
						(e-apply
							(e-ident (raw "match_time"))
							(e-ellipsis))
						(e-apply
							(e-ident (raw "nc"))
							(e-dbg
								(e-int (raw "2"))))
						(s-crash
							(e-string
								(e-string-part (raw "Unrnt"))))
						(s-decl
							(p-ident (raw "tag_"))
							(e-apply
								(e-tag (raw "Ok"))
								(e-ident (raw "number"))))
						(s-decl
							(p-ident (raw "i"))
							(e-string
								(e-string-part (raw "H, "))
								(e-ident (raw "world"))
								(e-string-part (raw ""))))
						(s-decl
							(p-ident (raw "t"))
							(e-list
								(e-apply
									(e-ident (raw "add_one"))
									(e-dbg
										(e-ident (raw "er"))))
								(e-int (raw "456"))))
						(s-for
							(p-ident (raw "n"))
							(e-ident (raw "list"))
							(e-block
								(statements
									(e-binop (op "+")
										(e-apply
											(e-ident (raw "line!"))
											(e-string
												(e-string-part (raw "Ag "))
												(e-ident (raw "n"))
												(e-string-part (raw " to "))
												(e-ident (raw "er"))
												(e-string-part (raw ""))))
										(e-ident (raw "n"))))))
						(s-decl
							(p-ident (raw "rd"))
							(e-record
								(field (field "foo")
									(e-int (raw "123")))
								(field (field "bar")
									(e-string
										(e-string-part (raw "H"))))
								(field (field "baz")
									(e-ident (raw "tag")))
								(field (field "qux")
									(e-apply
										(e-tag (raw "Ok"))
										(e-ident (raw "world"))))
								(field (field "ned"))))
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
									(e-ident (raw "nd"))
									(e-ident (raw "tuple")))
								(e-list
									(e-int (raw "1"))
									(e-int (raw "2"))
									(e-int (raw "3")))))
						(s-decl
							(p-ident (raw "mle"))
							(e-tuple
								(e-int (raw "123"))
								(e-string
									(e-string-part (raw "World")))
								(e-ident (raw "ag1"))
								(e-apply
									(e-tag (raw "Ok"))
									(e-ident (raw "world")))
								(e-tuple
									(e-ident (raw "ne"))
									(e-ident (raw "tuple")))
								(e-list
									(e-int (raw "1"))
									(e-int (raw "2"))
									(e-int (raw "3")))))
						(s-decl
							(p-ident (raw "b"))
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
							(p-ident (raw "le"))
							(e-question-suffix
								(e-field-access
									(e-question-suffix
										(e-method-call (method ".ned")
											(receiver
												(e-question-suffix
													(e-method-call (method ".od")
														(receiver
															(e-question-suffix
																(e-tuple
																	(e-ident (raw "arg1")))))
														(args))))
											(args)))
									(e-ident (raw "recd")))))
						(e-apply
							(e-ident (raw "line!"))
							(e-string
								(e-string-part (raw "Ho"))
								(e-apply
									(e-ident (raw "r"))
									(e-ident (raw "number")))
								(e-string-part (raw " "))))))))
		(s-type-anno (name "y")
			(ty-record))
		(s-decl
			(p-ident (raw "e"))
			(e-record))
		(s-type-anno (name "t")
			(ty-apply
				(ty (name "V"))
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "c")))))
		(s-expect
			(e-block
				(statements
					(s-decl
						(p-ident (raw "f"))
						(e-int (raw "1")))
					(e-binop (op "==")
						(e-ident (raw "h"))
						(e-ident (raw "foo"))))))))
~~~
# FORMATTED
~~~roc
# Thnt!
app [main!] { pf: platform "c" }

import pf.Stdout exposing [line!, e!]

import Stdot # Cose




# Cere
# Anre

# Ag

#



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
ne = |num| {
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
		[1, 2, 3, .. as rest] # t
			=> ment
		[1, 2 | 5, 3, .. as rest] => 123
		[] => 1
		3.14 => 314
		3.14 | 6.28 => 314
		(1, 2, 3) => 123
		(1, 2 | 5, 3) => 123
		{ foo: 1, bar: 2, ..rest } => 12->add(34)
		{ # Afpen
			oo #
				: # ue
					1, # eld
			ar: 2,
			..,
		} => 12
		{ foo: 1, bar: 2 | 7 } => 12
		{
			o: 1,
		} => 212
		Ok(123) => 12
	}

expect # Cord
	blah == 1 # nt

main! : (String # Thnt!
) -> Result({}, _)

ma = |_| { # Yee
	world = "d"
	var number = 123
	expect blah == 1
	tag = Blue
	return # d
		tag
	Jus
	...
	match_time(
		...,
	)
	nc(
		dbg # bug
			2,
	)
	crash "Unrnt"
	tag_ = Ok(number)
	i = "H, ${world}"
	t = [
		add_one(
			dbg # Afist
				er, # afarg
		),
		456, # ee
	]
	for n in list {
		line!("Ag ${n} to ${er}")
			+ n
	}
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world), ned }
	tuple = (123, "World", tag, Ok(world), (nd, tuple), [1, 2, 3])
	mle = (
		123,
		"World",
		ag1,
		Ok(world), # nt
		(ne, tuple),
		[1, 2, 3],
	)
	b = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
	le = (arg1)?.od()?.ned()?.recd?
	line!(
		"Ho${ #
			r(number) # xpr
		} ",
	)
} # Cocl

y : {}

e = {}

t : V((a, c))

expect {
	f = 1
	h == foo
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
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "ne"))
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
								(p-assign (ident "other")))))))))
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
										(patterns))))
							(value
								(e-num (value "1"))))
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
											(record-destruct (label "oo") (ident "oo")
												(sub-pattern
													(p-num (value "1"))))
											(record-destruct (label "ar") (ident "ar")
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
											(record-destruct (label "o") (ident "o")
												(sub-pattern
													(p-num (value "1"))))))))
							(value
								(e-num (value "212"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-num (value "12")))))))))
	(d-let
		(p-assign (ident "main!"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-parens
					(ty-malformed))
				(ty-malformed))))
	(d-let
		(p-assign (ident "ma"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-assign (ident "world"))
					(e-string
						(e-literal (string "d"))))
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
					(e-tag (name "Jus")))
				(s-expr
					(e-not-implemented))
				(s-expr
					(e-call (constraint-fn-var 3709)
						(e-lookup-local
							(p-assign (ident "match_time")))
						(e-not-implemented)))
				(s-expr
					(e-call
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-dbg
							(e-num (value "2")))))
				(s-crash (msg "Unrnt"))
				(s-let
					(p-assign (ident "tag_"))
					(e-tag (name "Ok")
						(args
							(e-lookup-local
								(p-assign (ident "number"))))))
				(s-let
					(p-assign (ident "i"))
					(e-block
						(s-let
							(p-assign (ident "#interp_0"))
							(e-lookup-local
								(p-assign (ident "world"))))
						(e-interpolation (constraint-fn-var 3800)
							(first
								(e-literal (string "H, ")))
							(parts
								(e-lookup-local
									(p-assign (ident "#interp_0")))
								(e-literal (string ""))))))
				(s-let
					(p-assign (ident "t"))
					(e-list
						(elems
							(e-call
								(e-lookup-local
									(p-assign (ident "add_one")))
								(e-dbg
									(e-runtime-error (tag "ident_not_in_scope"))))
							(e-num (value "456")))))
				(s-for
					(p-assign (ident "n"))
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-block
						(e-dispatch-call (method "plus") (constraint-fn-var 3978)
							(receiver
								(e-call
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-block
										(s-let
											(p-assign (ident "#interp_1"))
											(e-lookup-local
												(p-assign (ident "n"))))
										(s-let
											(p-assign (ident "#interp_2"))
											(e-runtime-error (tag "ident_not_in_scope")))
										(e-interpolation
											(first
												(e-literal (string "Ag ")))
											(parts
												(e-lookup-local
													(p-assign (ident "#interp_1")))
												(e-literal (string " to "))
												(e-lookup-local
													(p-assign (ident "#interp_2")))
												(e-literal (string "")))))))
							(args
								(e-lookup-local
									(p-assign (ident "n")))))))
				(s-let
					(p-assign (ident "rd"))
					(e-record
						(fields
							(field (name "foo")
								(e-num (value "123")))
							(field (name "bar")
								(e-string
									(e-literal (string "H"))))
							(field (name "baz")
								(e-lookup-local
									(p-assign (ident "tag"))))
							(field (name "qux")
								(e-tag (name "Ok")
									(args
										(e-lookup-local
											(p-assign (ident "world"))))))
							(field (name "ned")
								(e-runtime-error (tag "ident_not_in_scope"))))))
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
									(e-runtime-error (tag "self_referential_definition"))))
							(e-list
								(elems
									(e-num (value "1"))
									(e-num (value "2"))
									(e-num (value "3")))))))
				(s-let
					(p-assign (ident "mle"))
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
									(e-lookup-local
										(p-assign (ident "ne")))
									(e-lookup-local
										(p-assign (ident "tuple")))))
							(e-list
								(elems
									(e-num (value "1"))
									(e-num (value "2"))
									(e-num (value "3")))))))
				(s-let
					(p-assign (ident "b"))
					(e-if
						(if-branches
							(if-branch
								(e-dispatch-call (method "is_gt") (constraint-fn-var 4464)
									(receiver
										(e-match
											(match
												(cond
													(e-tag (name "Err")
														(args
															(e-runtime-error (tag "ident_not_in_scope")))))
												(branches
													(branch
														(patterns
															(pattern (degenerate false)
																(p-nominal-external (builtin)
																	(p-applied-tag))))
														(value
															(e-lookup-local
																(p-assign (ident "#ok")))))
													(branch
														(patterns
															(pattern (degenerate false)
																(p-nominal-external (builtin)
																	(p-applied-tag))))
														(value
															(e-num (value "12"))))))))
									(args
										(e-dispatch-call (method "times") (constraint-fn-var 4459)
											(receiver
												(e-num (value "5")))
											(args
												(e-num (value "5"))))))
								(e-nominal-external
									(builtin)
									(e-tag (name "True")))))
						(if-else
							(e-if
								(if-branches
									(if-branch
										(e-if
											(if-branches
												(if-branch
													(e-dispatch-call (method "is_lt") (constraint-fn-var 4581)
														(receiver
															(e-dispatch-call (method "plus") (constraint-fn-var 4543)
																(receiver
																	(e-num (value "13")))
																(args
																	(e-num (value "2")))))
														(args
															(e-num (value "5"))))
													(e-dispatch-call (method "is_gte") (constraint-fn-var 4690)
														(receiver
															(e-dispatch-call (method "minus") (constraint-fn-var 4652)
																(receiver
																	(e-num (value "10")))
																(args
																	(e-num (value "1")))))
														(args
															(e-num (value "16"))))))
											(if-else
												(e-nominal-external
													(builtin)
													(e-tag (name "False")))))
										(e-nominal-external
											(builtin)
											(e-tag (name "True")))))
								(if-else
									(e-dispatch-call (method "is_lte") (constraint-fn-var 4809)
										(receiver
											(e-num (value "12")))
										(args
											(e-dispatch-call (method "div_by") (constraint-fn-var 4804)
												(receiver
													(e-num (value "3")))
												(args
													(e-num (value "5")))))))))))
				(s-let
					(p-assign (ident "le"))
					(e-match
						(match
							(cond
								(e-field-access (field "recd")
									(receiver
										(e-match
											(match
												(cond
													(e-dispatch-call (method "ned") (constraint-fn-var 4875)
														(receiver
															(e-match
																(match
																	(cond
																		(e-dispatch-call (method "od") (constraint-fn-var 4842)
																			(receiver
																				(e-match
																					(match
																						(cond
																							(e-runtime-error (tag "ident_not_in_scope")))
																						(branches
																							(branch
																								(patterns
																									(pattern (degenerate false)
																										(p-nominal-external (builtin)
																											(p-applied-tag))))
																								(value
																									(e-runtime-error (tag "erroneous_value_expr"))))
																							(branch
																								(patterns
																									(pattern (degenerate false)
																										(p-nominal-external (builtin)
																											(p-applied-tag))))
																								(value
																									(e-return
																										(e-nominal-external
																											(builtin)
																											(e-tag (name "Err")
																												(args
																													(e-lookup-local
																														(p-assign (ident "#err")))))))))))))
																			(args)))
																	(branches
																		(branch
																			(patterns
																				(pattern (degenerate false)
																					(p-nominal-external (builtin)
																						(p-applied-tag))))
																			(value
																				(e-runtime-error (tag "erroneous_value_expr"))))
																		(branch
																			(patterns
																				(pattern (degenerate false)
																					(p-nominal-external (builtin)
																						(p-applied-tag))))
																			(value
																				(e-return
																					(e-nominal-external
																						(builtin)
																						(e-tag (name "Err")
																							(args
																								(e-lookup-local
																									(p-assign (ident "#err")))))))))))))
														(args)))
												(branches
													(branch
														(patterns
															(pattern (degenerate false)
																(p-nominal-external (builtin)
																	(p-applied-tag))))
														(value
															(e-lookup-local
																(p-assign (ident "#ok")))))
													(branch
														(patterns
															(pattern (degenerate false)
																(p-nominal-external (builtin)
																	(p-applied-tag))))
														(value
															(e-return
																(e-nominal-external
																	(builtin)
																	(e-tag (name "Err")
																		(args
																			(e-lookup-local
																				(p-assign (ident "#err")))))))))))))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal-external (builtin)
												(p-applied-tag))))
									(value
										(e-lookup-local
											(p-assign (ident "#ok")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal-external (builtin)
												(p-applied-tag))))
									(value
										(e-return
											(e-nominal-external
												(builtin)
												(e-tag (name "Err")
													(args
														(e-lookup-local
															(p-assign (ident "#err")))))))))))))
				(e-call
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-block
						(s-let
							(p-assign (ident "#interp_3"))
							(e-call
								(e-runtime-error (tag "ident_not_in_scope"))
								(e-lookup-local
									(p-assign (ident "number")))))
						(e-interpolation
							(first
								(e-literal (string "Ho")))
							(parts
								(e-lookup-local
									(p-assign (ident "#interp_3")))
								(e-literal (string " ")))))))))
	(d-let
		(p-assign (ident "y"))
		(e-anno-only)
		(annotation
			(ty-record)))
	(d-let
		(p-assign (ident "e"))
		(e-empty_record))
	(d-let
		(p-assign (ident "t"))
		(e-anno-only)
		(annotation
			(ty-malformed)))
	(s-import (module "pf.Stdout")
		(exposes
			(exposed (name "line!") (wildcard false))
			(exposed (name "e!") (wildcard false))))
	(s-import (module "Stdot")
		(exposes))
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
				(e-num (value "1")))))
	(s-expect
		(e-block
			(s-let
				(p-assign (ident "f"))
				(e-num (value "1")))
			(e-method-eq (negated "false")
				(lhs
					(e-runtime-error (tag "ident_not_in_scope")))
				(rhs
					(e-runtime-error (tag "ident_not_in_scope")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(Error, Error)"))
		(patt (type "Bool -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "Error -> U64"))
		(patt (type "Bool -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "[Blue, Red, ..], _arg -> Error"))
		(patt (type "Error -> Error"))
		(patt (type "_arg -> Error"))
		(patt (type "{}"))
		(patt (type "{}"))
		(patt (type "Error")))
	(type_decls
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
		(expr (type "Error -> U64"))
		(expr (type "Bool -> d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "[Blue, Red, ..], _arg -> Error"))
		(expr (type "Error -> Error"))
		(expr (type "_arg -> Error"))
		(expr (type "{}"))
		(expr (type "{}"))
		(expr (type "Error"))))
~~~
