# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
# Thnt!
app [main!] { pf: platform "c" }

import pf.Stdout exposing [line!]

import Stdot
		exposing [ #tem
Cust]

import Bae as Gooe
import
	Ba
Map(a, b) : Lis, (ab) -> List(b)
MapML( # Cb,
) # Ag
	: # Aon
		List( #rg
		),
		(ab) -> # row
			List(			b	) #z)

line : ( # Cm
) # Co
Som : { foo : O, bar : g }
Ml(a) : { # ld
}

Soine(a) : { #
} #
Maybe(a) : [Somne]

Mayine(a) : [ #
] #)

ane = |num| if num 2 else 5

one : U6
add = |Rum| {
	1
	if num {
		dbg # bug
			s exp0
	} else {
		dbg 123
		r
	}
}

me = |
	a, #b,
| # As
	match a {lue  {
	x
		}
		Blue=> {x
			}
	er #ent
			1	"for" => 20[1, ] # t
		ment
		[1, 2, 3,est]123
		[
		] 23
		3.1 314
		3.14 | 6.28 => 314
		(1, ) => 123
		(1, 2, 3)123
		{ 	} => 12
		Ok(123) => 12
	}

expect # Cord
	nt

main! : Listlt({}, _)
ma= |_| { e
	w = "d"
	var er = 123
	expect blaue
	return #d
		tag

	#
	...
	me(
		..., # r
	)crash ke"Unr!" #)
	i= "H, ${d}"
t = [
		one(er, 		),	456, # two
9, #ee
	]
	for n in list {
	line!("Ag ${n} to ${er}")
		ber + n
	}
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
	t = (123, "World", tag, O, (nd, t), [1, 2, 3])
	m (
		123,
		"World",ag1,
		O, # nt
		(ne, tuple),
		[1, 2, 3],
	)
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
	Stdo!(
		"Ho${ #
			r(nu) # xpr
		} ",
	)
} # Cocl

y : {}
e = {}

t : V((a,c))

expect {
	foo == 1
h == foo
}
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_020.md:52:16:52:16
PARSE ERROR - fuzz_crash_020.md:58:4:58:4
PARSE ERROR - fuzz_crash_020.md:59:3:59:3
PARSE ERROR - fuzz_crash_020.md:60:16:60:16
PARSE ERROR - fuzz_crash_020.md:62:5:62:5
PARSE ERROR - fuzz_crash_020.md:63:7:63:7
PARSE ERROR - fuzz_crash_020.md:66:12:66:12
MODULE NOT FOUND - fuzz_crash_020.md:6:1:8:6
MODULE NOT FOUND - fuzz_crash_020.md:10:1:10:19
MODULE NOT FOUND - fuzz_crash_020.md:11:1:12:4
UNDECLARED TYPE - fuzz_crash_020.md:13:13:13:16
UNDECLARED TYPE VARIABLE - fuzz_crash_020.md:13:19:13:21
UNDECLARED TYPE VARIABLE - fuzz_crash_020.md:19:4:19:6
UNDECLARED TYPE VARIABLE - fuzz_crash_020.md:20:12:20:13
UNDECLARED TYPE - fuzz_crash_020.md:24:15:24:16
UNDECLARED TYPE VARIABLE - fuzz_crash_020.md:24:24:24:25
UNDECLARED TYPE - fuzz_crash_020.md:37:7:37:9
NAME NOT IN SCOPE - fuzz_crash_020.md:40:5:40:8
NAME NOT IN SCOPE - fuzz_crash_020.md:42:4:42:5
NAME NOT IN SCOPE - fuzz_crash_020.md:42:6:42:10
NAME NOT IN SCOPE - fuzz_crash_020.md:45:3:45:4
NAME NOT IN SCOPE - fuzz_crash_020.md:53:2:53:3
UNUSED VARIABLE - fuzz_crash_020.md:52:11:52:14
NAME NOT IN SCOPE - fuzz_crash_020.md:55:11:55:12
UNUSED VARIABLE - fuzz_crash_020.md:57:2:57:4
NAME NOT IN SCOPE - fuzz_crash_020.md:59:3:59:7
UNUSED VARIABLE - fuzz_crash_020.md:60:12:60:15
NAME NOT IN SCOPE - fuzz_crash_020.md:72:2:72:4
UNDECLARED TYPE - fuzz_crash_020.md:74:9:74:15
NAME NOT IN SCOPE - fuzz_crash_020.md:78:9:78:14
NAME NOT IN SCOPE - fuzz_crash_020.md:80:3:80:6
CRASH EXPECTS STRING - fuzz_crash_020.md:86:3:86:11
NAME NOT IN SCOPE - fuzz_crash_020.md:87:11:87:12
NAME NOT IN SCOPE - fuzz_crash_020.md:92:11:92:15
NAME NOT IN SCOPE - fuzz_crash_020.md:93:2:93:7
NAME NOT IN SCOPE - fuzz_crash_020.md:94:3:94:6
NAME NOT IN SCOPE - fuzz_crash_020.md:96:34:96:37
NAME NOT IN SCOPE - fuzz_crash_020.md:96:47:96:52
NAME NOT IN SCOPE - fuzz_crash_020.md:96:54:96:57
DUPLICATE DEFINITION - fuzz_crash_020.md:97:2:97:3
NAME NOT IN SCOPE - fuzz_crash_020.md:97:21:97:24
NAME NOT IN SCOPE - fuzz_crash_020.md:97:30:97:32
INVALID ASSIGNMENT TO ITSELF - fuzz_crash_020.md:97:34:97:35
NAME NOT IN SCOPE - fuzz_crash_020.md:98:2:98:3
NAME NOT IN SCOPE - fuzz_crash_020.md:100:11:100:14
NAME NOT IN SCOPE - fuzz_crash_020.md:102:4:102:6
NAME NOT IN SCOPE - fuzz_crash_020.md:102:8:102:13
NAME NOT IN SCOPE - fuzz_crash_020.md:105:2:105:3
NAME NOT IN SCOPE - fuzz_crash_020.md:105:55:105:59
NAME NOT IN SCOPE - fuzz_crash_020.md:105:60:105:64
NAME NOT IN SCOPE - fuzz_crash_020.md:108:4:108:5
NAME NOT IN SCOPE - fuzz_crash_020.md:108:6:108:8
UNUSED VARIABLE - fuzz_crash_020.md:76:2:76:3
UNUSED VARIABLE - fuzz_crash_020.md:87:2:87:3
UNUSED VARIABLE - fuzz_crash_020.md:96:2:96:4
UNUSED VARIABLE - fuzz_crash_020.md:97:2:97:3
UNDECLARED TYPE - fuzz_crash_020.md:116:5:116:6
NAME NOT IN SCOPE - fuzz_crash_020.md:119:2:119:5
NAME NOT IN SCOPE - fuzz_crash_020.md:120:1:120:2
NAME NOT IN SCOPE - fuzz_crash_020.md:120:6:120:9
EXPOSED BUT NOT DEFINED - fuzz_crash_020.md:2:6:2:11
TOO FEW ARGS - fuzz_crash_020.md:17:3:18:4
DECLARATION HAS NO VALUE - fuzz_crash_020.md:22:1:23:2
DECLARATION HAS NO VALUE - fuzz_crash_020.md:37:1:37:9
MISSING METHOD - fuzz_crash_020.md:39:2:39:3
MISSING METHOD - fuzz_crash_020.md:58:6:58:11
TYPE MISMATCH - fuzz_crash_020.md:52:2:52:2
DECLARATION HAS NO VALUE - fuzz_crash_020.md:74:1:74:22
MISSING METHOD - fuzz_crash_020.md:86:11:86:17
TYPE MISMATCH - fuzz_crash_020.md:98:4:104:3
TYPE MISMATCH - fuzz_crash_020.md:105:2:105:54
TYPE MISMATCH - fuzz_crash_020.md:93:22:93:24
DECLARATION HAS NO VALUE - fuzz_crash_020.md:113:1:113:7
DECLARATION HAS NO VALUE - fuzz_crash_020.md:116:1:116:13
TYPE MISMATCH - fuzz_crash_020.md:119:2:119:10
MISSING METHOD - fuzz_crash_020.md:105:55:105:66
MISSING METHOD - fuzz_crash_020.md:105:55:105:72
# PROBLEMS

┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: match_branch_missing_arrow ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  match a {lue  {                                                           │
 │                ‾                                                           │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:52:16 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: match_branch_missing_arrow ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  1 "for" => 20[1, ] # t                                                    │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:58:4 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: match_branch_missing_arrow ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  ment                                                                      │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:59:3 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: match_branch_missing_arrow ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  [1, 2, 3,est]123                                                          │
 │               ‾                                                            │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:60:16 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: match_branch_missing_arrow ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  ] 23                                                                      │
 │    ‾                                                                       │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:62:5 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: match_branch_missing_arrow ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  3.1 314                                                                   │
 │      ‾                                                                     │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:63:7 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: match_branch_missing_arrow ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  (1, 2, 3)123                                                              │
 │           ‾                                                                │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:66:12 ┘

    This is an unexpected parsing error. Please check your syntax.


┌──────────────────┐
│ MODULE NOT FOUND ├─ The module `Stdot` was not found in this Roc project. ──┐
└┬─────────────────┘                                                          │
 │                                                                            │
 │  import Stdot                                                              │
 │          exposing [ #tem                                                   │
 │  Cust]                                                                     │
 │                                                                            │
 └───────────────────────────────────────────────────── fuzz_crash_020.md:6:1 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ The module `Bae` was not found in this Roc project. ────┐
└┬─────────────────┘                                                          │
 │                                                                            │
 │  import Bae as Gooe                                                        │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                        │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:10:1 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ The module `Ba` was not found in this Roc project. ─────┐
└┬─────────────────┘                                                          │
 │                                                                            │
 │  import                                                                    │
 │      Ba                                                                    │
 │                                                                            │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:11:1 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Lis` is not declared in this scope. ───────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Map(a, b) : Lis, (ab) -> List(b)                                          │
 │              ‾‾‾                                                           │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:13:13 ┘



┌──────────────────────────┐
│ UNDECLARED TYPE VARIABLE ├─ The type variable `ab` is not declared in ──────┐
└┬─────────────────────────┘  this scope.                                     │
 │                                                                            │
 │  Map(a, b) : Lis, (ab) -> List(b)                                          │
 │                    ‾‾                                                      │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:13:19 ┘

    Type variables must be introduced in a type annotation before they can be
    used.


┌──────────────────────────┐
│ UNDECLARED TYPE VARIABLE ├─ The type variable `ab` is not declared in ──────┐
└┬─────────────────────────┘  this scope.                                     │
 │                                                                            │
 │  (ab) -> # row                                                             │
 │   ‾‾                                                                       │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:19:4 ┘

    Type variables must be introduced in a type annotation before they can be
    used.


┌──────────────────────────┐
│ UNDECLARED TYPE VARIABLE ├─ The type variable `b` is not declared in this ──┐
└┬─────────────────────────┘  scope.                                          │
 │                                                                            │
 │  List(   b ) #z)                                                           │
 │          ‾                                                                 │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:20:12 ┘

    Type variables must be introduced in a type annotation before they can be
    used.


┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `O` is not declared in this scope. ─────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Som : { foo : O, bar : g }                                                │
 │                ‾                                                           │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:24:15 ┘



┌──────────────────────────┐
│ UNDECLARED TYPE VARIABLE ├─ The type variable `g` is not declared in this ──┐
└┬─────────────────────────┘  scope.                                          │
 │                                                                            │
 │  Som : { foo : O, bar : g }                                                │
 │                         ‾                                                  │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:24:24 ┘

    Type variables must be introduced in a type annotation before they can be
    used.


┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `U6` is not declared in this scope. ────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  one : U6                                                                  │
 │        ‾‾                                                                  │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:37:7 ┘



┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `num` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  if num {                                                                  │
 │     ‾‾‾                                                                    │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:40:5 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `s` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  s exp0                                                                    │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:42:4 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `exp0` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  s exp0                                                                    │
 │    ‾‾‾‾                                                                    │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:42:6 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `r` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  r                                                                         │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:45:3 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `x` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  x                                                                         │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:53:2 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `lue` is defined here and then never used. ─────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  match a {lue  {                                                           │
 │           ‾‾‾                                                              │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:52:11 ┘

    If you don't need this variable, prefix it with an underscore like `_lue`
    to suppress this warning.


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `x` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  Blue=> {x                                                                 │
 │          ‾                                                                 │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:55:11 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `er` is defined here and then never used. ──────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  er #ent                                                                   │
 │  ‾‾                                                                        │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:57:2 ┘

    If you don't need this variable, prefix it with an underscore like `_er` to
    suppress this warning.


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `ment` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  ment                                                                      │
 │  ‾‾‾‾                                                                      │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:59:3 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `est` is defined here and then never used. ─────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  [1, 2, 3,est]123                                                          │
 │           ‾‾‾                                                              │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:60:12 ┘

    If you don't need this variable, prefix it with an underscore like `_est`
    to suppress this warning.


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `nt` in this scope. ──────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  nt                                                                        │
 │  ‾‾                                                                        │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:72:2 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Listlt` is not declared in this scope. ────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  main! : Listlt({}, _)                                                     │
 │          ‾‾‾‾‾‾                                                            │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:74:9 ┘



┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `blaue` in this scope. ───────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  expect blaue                                                              │
 │         ‾‾‾‾‾                                                              │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:78:9 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `tag` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  tag                                                                       │
 │  ‾‾‾                                                                       │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:80:3 ┘

    Is it misspelled, or is there an import missing?


┌──────────────────────┐
│ CRASH EXPECTS STRING ├─ The `crash` keyword expects a string literal as ────┐
└┬─────────────────────┘  its argument.                                       │
 │                                                                            │
 │  )crash ke"Unr!" #)                                                        │
 │   ‾‾‾‾‾‾‾‾                                                                 │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:86:3 ┘

    For example: `crash "Something went wrong"`


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `d` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  i= "H, ${d}"                                                              │
 │           ‾                                                                │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:87:11 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `list` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  for n in list {                                                           │
 │           ‾‾‾‾                                                             │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:92:11 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `line!` in this scope. ───────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  line!("Ag ${n} to ${er}")                                                 │
 │  ‾‾‾‾‾                                                                     │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:93:2 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `ber` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  ber + n                                                                   │
 │  ‾‾‾                                                                       │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:94:3 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `tag` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }                 │
 │                                  ‾‾‾                                       │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:96:34 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `world` in this scope. ───────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }                 │
 │                                               ‾‾‾‾‾                        │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:96:47 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `ned` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }                 │
 │                                                      ‾‾‾                   │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:96:54 ┘

    Is it misspelled, or is there an import missing?


┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `t` is being redeclared here. ─────────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  t = (123, "World", tag, O, (nd, t), [1, 2, 3])                            │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:97:2 ┘

    In this scope, `t` was already defined here:
       ┌──────────────────────────────────────────────────────────────────────┐
    88 │  t = [                                                               │
       │  ‾                                                                   │
       └────────────────────────────────────────────── fuzz_crash_020.md:88:1 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `tag` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  t = (123, "World", tag, O, (nd, t), [1, 2, 3])                            │
 │                     ‾‾‾                                                    │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:97:21 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `nd` in this scope. ──────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  t = (123, "World", tag, O, (nd, t), [1, 2, 3])                            │
 │                              ‾‾                                            │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:97:30 ┘

    Is it misspelled, or is there an import missing?


┌──────────────────────────────┐
│ INVALID ASSIGNMENT TO ITSELF ├─ The value `t` is assigned to itself, ───────┐
└┬─────────────────────────────┘  which would cause an infinite loop at       │
 │                                runtime.                                    │
 │                                                                            │
 │  t = (123, "World", tag, O, (nd, t), [1, 2, 3])                            │
 │                                  ‾                                         │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:97:34 ┘

    Only functions can reference themselves (for recursion). For non-function
    values, the right-hand side must be fully computable without referring to
    the value being assigned.


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `m` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  m (                                                                       │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:98:2 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `ag1` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  "World",ag1,                                                              │
 │          ‾‾‾                                                               │
 └────────────────────────────────────────────────── fuzz_crash_020.md:100:11 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `ne` in this scope. ──────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  (ne, tuple),                                                              │
 │   ‾‾                                                                       │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:102:4 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `tuple` in this scope. ───────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  (ne, tuple),                                                              │
 │       ‾‾‾‾‾                                                                │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:102:8 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `b` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.n… │
 │  ‾                                                                         │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:105:2 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `e_fn` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  …12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?                                   │
 │           ‾‾‾‾                                                             │
 └────────────────────────────────────────────────── fuzz_crash_020.md:105:55 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `arg1` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  … 3 e_fn(arg1)?.od()?.ned()?.recd?                                        │
 │           ‾‾‾‾                                                             │
 └────────────────────────────────────────────────── fuzz_crash_020.md:105:60 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `r` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  r(nu) # xpr                                                               │
 │  ‾                                                                         │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:108:4 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `nu` in this scope. ──────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  r(nu) # xpr                                                               │
 │    ‾‾                                                                      │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:108:6 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `w` is defined here and then never used. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  w = "d"                                                                   │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:76:2 ┘

    If you don't need this variable, prefix it with an underscore like `_w` to
    suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `i` is defined here and then never used. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  i= "H, ${d}"                                                              │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:87:2 ┘

    If you don't need this variable, prefix it with an underscore like `_i` to
    suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `rd` is defined here and then never used. ──────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }                 │
 │  ‾‾                                                                        │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:96:2 ┘

    If you don't need this variable, prefix it with an underscore like `_rd` to
    suppress this warning.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `t` is defined here and then never used. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  t = (123, "World", tag, O, (nd, t), [1, 2, 3])                            │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:97:2 ┘

    If you don't need this variable, prefix it with an underscore like `_t` to
    suppress this warning.


┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `V` is not declared in this scope. ─────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  t : V((a,c))                                                              │
 │      ‾                                                                     │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:116:5 ┘



┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `foo` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  foo == 1                                                                  │
 │  ‾‾‾                                                                       │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:119:2 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `h` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  h == foo                                                                  │
 │  ‾                                                                         │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:120:1 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `foo` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  h == foo                                                                  │
 │       ‾‾‾                                                                  │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:120:6 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────────────┐
│ EXPOSED BUT NOT DEFINED ├─ The module header says that `main!` is ──────────┐
└┬────────────────────────┘  exposed, but it is not defined anywhere in       │
 │                           this module.                                     │
 │                                                                            │
 │  app [main!] { pf: platform "c" }                                          │
 │       ‾‾‾‾‾                                                                │
 └───────────────────────────────────────────────────── fuzz_crash_020.md:2:6 ┘

    You can fix this by either defining `main!` in this module, or by removing
    it from the list of exposed values.


┌──────────────┐
│ TOO FEW ARGS ├─ The type List expects 1 argument, but got 0 instead. ───────┐
└┬─────────────┘                                                              │
 │                                                                            │
 │  List( #rg                                                                 │
 │  ),                                                                        │
 │                                                                            │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:17:3 ┘



┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  line : ( # Cm                                                             │
 │  ) # Co                                                                    │
 │                                                                            │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:22:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  one : U6                                                                  │
 │  ‾‾‾‾‾‾‾‾                                                                  │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:37:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌────────────────┐
│ MISSING METHOD ├─ This `from_numeral` method is being called on a value ────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  1                                                                         │
 │  ‾                                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:39:2 ┘

    The value's type, which does not have a method named `from_numeral`, is:

        {}


┌────────────────┐
│ MISSING METHOD ├─ This `from_quote` method is being called on a value ──────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  1 "for" => 20[1, ] # t                                                    │
 │    ‾‾‾‾‾                                                                   │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:58:6 ┘

    The value's type, which does not have a method named `from_quote`, is:

        [Blue, ..]


┌───────────────┐
│ TYPE MISMATCH ├─ The fifth branch of this `match` does not match the ───────┐
└┬──────────────┘  previous ones.                                             │
 │                                                                            │
 │  match a {lue  {                                                           │
 │  x                                                                         │
 │      }                                                                     │
 │      Blue=> {x                                                             │
 │          }                                                                 │
 │  er #ent                                                                   │
 │          1 "for" => 20[1, ] # t                                            │
 │      ment                                                                  │
 │      [1, 2, 3,est]123                                                      │
 │      [                                                                     │
 │      ] 23                                                                  │
 │      3.1 314                                                               │
 │      3.14 | 6.28 => 314                                                    │
 │      (1, ) => 123                                                          │
 │      (1, 2, 3)123                                                          │
 │      {  } => 12                                                            │
 │      Ok(123) => 12                                                         │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:52:17 ┘

    This fifth branch is trying to match:

        List(f)
          where [
            f.from_numeral : Numeral -> Try(f, [InvalidNumeral(Str)]),
            f.is_eq : f, f -> Bool,
          ]

    But the expression between the `match` parenthesis has the type:

        [Blue, ..]

    These can never match! Either the pattern or expression has a problem.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  main! : Listlt({}, _)                                                     │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:74:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌────────────────┐
│ MISSING METHOD ├─ This `from_quote` method is being called on a value ──────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  )crash ke"Unr!" #)                                                        │
 │           ‾‾‾‾‾‾                                                           │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:86:11 ┘

    The value's type, which does not have a method named `from_quote`, is:

        {}


┌───────────────┐
│ TYPE MISMATCH ├─ This expression produces a value, but it's not being ──────┐
└┬──────────────┘  used.                                                      │
 │                                                                            │
 │  m (                                                                       │
 │      123,                                                                  │
 │      "World",ag1,                                                          │
 │      O, # nt                                                               │
 │      (ne, tuple),                                                          │
 │      [1, 2, 3],                                                            │
 │  )                                                                         │
 │                                                                            │
 └──────────────────────────────────────────────────── fuzz_crash_020.md:98:4 ┘

    It has the type:

        (f, j, Error, [O, ..], (Error, Error), List(k))
          where [
            f.from_numeral : Numeral -> Try(f, [InvalidNumeral(Str)]),
            j.from_quote : Str -> Try(j, [BadQuotedBytes(Str)]),
            k.from_numeral : Numeral -> Try(k, [InvalidNumeral(Str)]),
          ]

    Since this expression is used as a statement, it must evaluate to `{}`.
    If you don't need the value, you can ignore it with `_ =`.


┌───────────────┐
│ TYPE MISMATCH ├─ This expression produces a value, but it's not being ──────┐
└┬──────────────┘  used.                                                      │
 │                                                                            │
 │  b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.n… │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                      │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:105:2 ┘

    It has the type:

        Bool

    Since this expression is used as a statement, it must evaluate to `{}`.
    If you don't need the value, you can ignore it with `_ =`.


┌───────────────┐
│ TYPE MISMATCH ├─ This expression is used in an unexpected way. ─────────────┐
└┬──────────────┘                                                             │
 │                                                                            │
 │  line!("Ag ${n} to ${er}")                                                 │
 │                      ‾‾                                                    │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:93:22 ┘

    It has the type:

        Dec

    But you are trying to use it as:

        Str


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  y : {}                                                                    │
 │  ‾‾‾‾‾‾                                                                    │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:113:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  t : V((a,c))                                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾                                                              │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:116:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌───────────────┐
│ TYPE MISMATCH ├─ This expression produces a value, but it's not being ──────┐
└┬──────────────┘  used.                                                      │
 │                                                                            │
 │  foo == 1                                                                  │
 │  ‾‾‾‾‾‾‾‾                                                                  │
 └─────────────────────────────────────────────────── fuzz_crash_020.md:119:2 ┘

    It has the type:

        Bool

    Since this expression is used as a statement, it must evaluate to `{}`.
    If you don't need the value, you can ignore it with `_ =`.


┌────────────────┐
│ MISSING METHOD ├─ This is trying to dispatch a method named `od` on an ─────┐
└┬───────────────┘  unresolved type variable, but unresolved type variables   │
 │                  have no methods.                                          │
 │                                                                            │
 │  …12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?                                   │
 │           ‾‾‾‾‾‾‾‾‾‾‾                                                      │
 └────────────────────────────────────────────────── fuzz_crash_020.md:105:55 ┘

    Hint: You can replace this static dispatch call with an ordinary function
    call, or force the type variable to become more concrete—for example, by
    adding a type annotation that narrows its type to something that actually
    has methods.


┌────────────────┐
│ MISSING METHOD ├─ This is trying to dispatch a method named `ned` on an ────┐
└┬───────────────┘  unresolved type variable, but unresolved type variables   │
 │                  have no methods.                                          │
 │                                                                            │
 │  …12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?                                   │
 │           ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                │
 └────────────────────────────────────────────────── fuzz_crash_020.md:105:55 ┘

    Hint: You can replace this static dispatch call with an ordinary function
    call, or force the type variable to become more concrete—for example, by
    adding a type annotation that narrows its type to something that actually
    has methods.

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,LowerIdent,CloseSquare,
KwImport,UpperIdent,
KwExposing,OpenSquare,
UpperIdent,CloseSquare,
KwImport,UpperIdent,KwAs,UpperIdent,
KwImport,
UpperIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,UpperIdent,Comma,OpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,
CloseRound,
OpColon,
UpperIdent,NoSpaceOpenRound,
CloseRound,Comma,
OpenRound,LowerIdent,CloseRound,OpArrow,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,OpenRound,
CloseRound,
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,
CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,
CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenSquare,UpperIdent,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenSquare,
CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,Int,KwElse,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpBar,UpperIdent,OpBar,OpenCurly,
Int,
KwIf,LowerIdent,OpenCurly,
KwDbg,
LowerIdent,LowerIdent,
CloseCurly,KwElse,OpenCurly,
KwDbg,Int,
LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpBar,
LowerIdent,Comma,
OpBar,
KwMatch,LowerIdent,OpenCurly,LowerIdent,OpenCurly,
LowerIdent,
CloseCurly,
UpperIdent,OpFatArrow,OpenCurly,LowerIdent,
CloseCurly,
LowerIdent,
Int,StringStart,StringPart,StringEnd,OpFatArrow,Int,OpenSquare,Int,Comma,CloseSquare,
LowerIdent,
OpenSquare,Int,Comma,Int,Comma,Int,Comma,LowerIdent,CloseSquare,Int,
OpenSquare,
CloseSquare,Int,
Float,Int,
Float,OpBar,Float,OpFatArrow,Int,
OpenRound,Int,Comma,CloseRound,OpFatArrow,Int,
OpenRound,Int,Comma,Int,Comma,Int,CloseRound,Int,
OpenCurly,CloseCurly,OpFatArrow,Int,
UpperIdent,NoSpaceOpenRound,Int,CloseRound,OpFatArrow,Int,
CloseCurly,
KwExpect,
LowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,Underscore,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,LowerIdent,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
KwVar,LowerIdent,OpAssign,Int,
KwExpect,LowerIdent,
KwReturn,
LowerIdent,
TripleDot,
LowerIdent,NoSpaceOpenRound,
TripleDot,Comma,
CloseRound,KwCrash,LowerIdent,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,OpAssign,OpenSquare,
LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,CloseRound,Comma,Int,Comma,
Int,Comma,
CloseSquare,
KwFor,LowerIdent,KwIn,LowerIdent,OpenCurly,
LowerIdent,NoSpaceOpenRound,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,CloseRound,
LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,CloseCurly,
LowerIdent,OpAssign,OpenRound,Int,Comma,StringStart,StringPart,StringEnd,Comma,LowerIdent,Comma,UpperIdent,Comma,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,CloseRound,
LowerIdent,OpenRound,
Int,Comma,
StringStart,StringPart,StringEnd,Comma,LowerIdent,Comma,
UpperIdent,Comma,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,
OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,Comma,
CloseRound,
LowerIdent,OpDoubleQuestion,Int,OpGreaterThan,Int,OpOr,Int,OpPlus,Int,OpLessThan,Int,OpAnd,Int,OpBinaryMinus,Int,OpGreaterThanOrEq,Int,OpOr,Int,OpLessThanOrEq,Int,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,NoSpaceDotLowerIdent,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,
StringStart,StringPart,OpenStringInterpolation,
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseStringInterpolation,StringPart,StringEnd,Comma,
CloseRound,
CloseCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,
LowerIdent,OpAssign,OpenCurly,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,CloseRound,
KwExpect,OpenCurly,
LowerIdent,OpEquals,Int,
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
					(text "line!"))))
		(s-import (raw "Stdot")
			(exposing
				(exposed-upper-ident (text "Cust"))))
		(s-import (raw "Bae") (alias "Gooe"))
		(s-import (raw "Ba"))
		(s-type-decl
			(header (name "Map")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-fn
				(ty (name "Lis"))
				(ty-var (raw "ab"))
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "b")))))
		(s-type-decl
			(header (name "MapML")
				(args))
			(ty-fn
				(ty-apply
					(ty (name "List")))
				(ty-var (raw "ab"))
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "b")))))
		(s-type-anno (name "line")
			(ty-tuple))
		(s-type-decl
			(header (name "Som")
				(args))
			(ty-record
				(anno-record-field (name "foo")
					(ty (name "O")))
				(anno-record-field (name "bar")
					(ty-var (raw "g")))))
		(s-type-decl
			(header (name "Ml")
				(args
					(ty-var (raw "a"))))
			(ty-record))
		(s-type-decl
			(header (name "Soine")
				(args
					(ty-var (raw "a"))))
			(ty-record))
		(s-type-decl
			(header (name "Maybe")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty (name "Somne")))))
		(s-type-decl
			(header (name "Mayine")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags)))
		(s-decl
			(p-ident (raw "ane"))
			(e-lambda
				(args
					(p-ident (raw "num")))
				(e-if-then-else
					(e-ident (raw "num"))
					(e-int (raw "2"))
					(e-int (raw "5")))))
		(s-type-anno (name "one")
			(ty (name "U6")))
		(s-decl
			(p-ident (raw "add"))
			(e-lambda
				(args
					(p-tag (raw "Rum")))
				(e-block
					(statements
						(e-int (raw "1"))
						(e-if-then-else
							(e-ident (raw "num"))
							(e-block
								(statements
									(s-dbg
										(e-ident (raw "s")))
									(e-ident (raw "exp0"))))
							(e-block
								(statements
									(s-dbg
										(e-int (raw "123")))
									(e-ident (raw "r")))))))))
		(s-decl
			(p-ident (raw "me"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-match
					(e-ident (raw "a"))
					(branches
						(branch
							(p-ident (raw "lue"))
							(e-block
								(statements
									(e-ident (raw "x")))))
						(branch
							(p-tag (raw "Blue"))
							(e-block
								(statements
									(e-ident (raw "x")))))
						(branch
							(p-ident (raw "er"))
							(e-int (raw "1")))
						(branch
							(p-string (raw """)
								(p-string-text (raw "for")))
							(e-int (raw "20")))
						(branch
							(p-list
								(p-int (raw "1")))
							(e-ident (raw "ment")))
						(branch
							(p-list
								(p-int (raw "1"))
								(p-int (raw "2"))
								(p-int (raw "3"))
								(p-ident (raw "est")))
							(e-int (raw "123")))
						(branch
							(p-list)
							(e-int (raw "23")))
						(branch
							(p-frac (raw "3.1"))
							(e-int (raw "314")))
						(branch
							(p-alternatives
								(p-frac (raw "3.14"))
								(p-frac (raw "6.28")))
							(e-int (raw "314")))
						(branch
							(p-tuple
								(p-int (raw "1")))
							(e-int (raw "123")))
						(branch
							(p-tuple
								(p-int (raw "1"))
								(p-int (raw "2"))
								(p-int (raw "3")))
							(e-int (raw "123")))
						(branch
							(p-record)
							(e-int (raw "12")))
						(branch
							(p-tag (raw "Ok")
								(p-int (raw "123")))
							(e-int (raw "12")))))))
		(s-expect
			(e-ident (raw "nt")))
		(s-type-anno (name "main!")
			(ty-apply
				(ty (name "Listlt"))
				(ty-record)
				(_)))
		(s-decl
			(p-ident (raw "ma"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(e-ident (raw "e"))
						(s-decl
							(p-ident (raw "w"))
							(e-string
								(e-string-part (raw "d"))))
						(s-var (name "er")
							(e-int (raw "123")))
						(s-expect
							(e-ident (raw "blaue")))
						(s-return
							(e-ident (raw "tag")))
						(e-ellipsis)
						(e-apply
							(e-ident (raw "me"))
							(e-ellipsis))
						(s-crash
							(e-ident (raw "ke")))
						(e-string
							(e-string-part (raw "Unr!")))
						(s-decl
							(p-ident (raw "i"))
							(e-string
								(e-string-part (raw "H, "))
								(e-ident (raw "d"))
								(e-string-part (raw ""))))
						(s-decl
							(p-ident (raw "t"))
							(e-list
								(e-apply
									(e-ident (raw "one"))
									(e-ident (raw "er")))
								(e-int (raw "456"))
								(e-int (raw "9"))))
						(s-for
							(p-ident (raw "n"))
							(e-ident (raw "list"))
							(e-block
								(statements
									(e-apply
										(e-ident (raw "line!"))
										(e-string
											(e-string-part (raw "Ag "))
											(e-ident (raw "n"))
											(e-string-part (raw " to "))
											(e-ident (raw "er"))
											(e-string-part (raw ""))))
									(e-binop (op "+")
										(e-ident (raw "ber"))
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
							(p-ident (raw "t"))
							(e-tuple
								(e-int (raw "123"))
								(e-string
									(e-string-part (raw "World")))
								(e-ident (raw "tag"))
								(e-tag (raw "O"))
								(e-tuple
									(e-ident (raw "nd"))
									(e-ident (raw "t")))
								(e-list
									(e-int (raw "1"))
									(e-int (raw "2"))
									(e-int (raw "3")))))
						(e-ident (raw "m"))
						(e-tuple
							(e-int (raw "123"))
							(e-string
								(e-string-part (raw "World")))
							(e-ident (raw "ag1"))
							(e-tag (raw "O"))
							(e-tuple
								(e-ident (raw "ne"))
								(e-ident (raw "tuple")))
							(e-list
								(e-int (raw "1"))
								(e-int (raw "2"))
								(e-int (raw "3"))))
						(e-binop (op "or")
							(e-binop (op ">")
								(e-binop (op "??")
									(e-ident (raw "b"))
									(e-int (raw "12")))
								(e-int (raw "5")))
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
									(e-int (raw "3")))))
						(e-question-suffix
							(e-field-access
								(e-question-suffix
									(e-method-call (method ".ned")
										(receiver
											(e-question-suffix
												(e-method-call (method ".od")
													(receiver
														(e-question-suffix
															(e-apply
																(e-ident (raw "e_fn"))
																(e-ident (raw "arg1")))))
													(args))))
										(args)))
								(e-ident (raw "recd"))))
						(e-apply
							(e-tag (raw "Stdo!"))
							(e-string
								(e-string-part (raw "Ho"))
								(e-apply
									(e-ident (raw "r"))
									(e-ident (raw "nu")))
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
					(e-binop (op "==")
						(e-ident (raw "foo"))
						(e-int (raw "1")))
					(e-binop (op "==")
						(e-ident (raw "h"))
						(e-ident (raw "foo"))))))))
~~~
# FORMATTED
~~~roc
# Thnt!
app [main!] { pf: platform "c" }

import pf.Stdout exposing [line!]

import Stdot
	exposing [ # tem
		Cust,
	]

import Bae as Gooe
import
	Ba
Map(a, b) : Lis, (ab) -> List(b)

MapML # Ag
	: # Aon
		List(),
		(ab) -> # row
			List(b) # z)

line : () # Co

Som : { foo : O, bar : g }

Ml(a) : {}

Soine(a) : {} #

Maybe(a) : [Somne]

Mayine(a) : [] # )

ane = |num| if num 2 else 5

one : U6

add = |Rum| {
	1
	if num {
		dbg # bug
			s
		exp0
	} else {
		dbg 123
		r
	}
}

me = |
	a, # b,
| # As
	match a {
		lue => {
			x
		}
		Blue => {
			x
		}
		er # ent
			=> # ent
				1
		"for" => 20
		[
			1,
		] # t
			=> # t
				ment
		[1, 2, 3, est] => 123
		[] => 23
		3.1 => 314
		3.14 | 6.28 => 314
		(
			1,
		) => 123
		(1, 2, 3) => 123
		{} => 12
		Ok(123) => 12
	}

expect # Cord
	nt

main! : Listlt({}, _)

ma = |_| {
	e
	w = "d"
	var er = 123
	expect blaue
	return # d
		tag

	#
	...
	me(
		..., # r
	)
	crash ke
	"Unr!" # )
	i = "H, ${d}"
	t = [
		one(
			er,
		),
		456, # two
		9, # ee
	]
	for n in list {
		line!("Ag ${n} to ${er}")
		ber + n
	}
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world), ned }
	t = (123, "World", tag, O, (nd, t), [1, 2, 3])
	m
	(
		123,
		"World",
		ag1,
		O, # nt
		(ne, tuple),
		[1, 2, 3],
	)
	b ?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3
	e_fn(arg1)?.od()?.ned()?.recd?
	Stdo!(
		"Ho${ #
			r(nu) # xpr
		} ",
	)
} # Cocl

y : {}

e = {}

t : V((a, c))

expect {
	foo == 1
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
			(ty-tuple)))
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
		(p-assign (ident "one"))
		(e-anno-only)
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "add"))
		(e-lambda
			(args
				(p-applied-tag))
			(e-block
				(s-expr
					(e-num (value "1")))
				(e-if
					(if-branches
						(if-branch
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-block
								(s-dbg
									(e-runtime-error (tag "ident_not_in_scope")))
								(e-runtime-error (tag "ident_not_in_scope")))))
					(if-else
						(e-block
							(s-dbg
								(e-num (value "123")))
							(e-runtime-error (tag "ident_not_in_scope"))))))))
	(d-let
		(p-assign (ident "me"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "a"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-assign (ident "lue"))))
							(value
								(e-block
									(e-runtime-error (tag "ident_not_in_scope")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-block
									(e-runtime-error (tag "ident_not_in_scope")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-assign (ident "er"))))
							(value
								(e-num (value "1"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-str (text "for"))))
							(value
								(e-num (value "20"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-list
										(patterns
											(p-num (value "1"))))))
							(value
								(e-runtime-error (tag "ident_not_in_scope"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-list
										(patterns
											(p-num (value "1"))
											(p-num (value "2"))
											(p-num (value "3"))
											(p-assign (ident "est"))))))
							(value
								(e-num (value "123"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-list
										(patterns))))
							(value
								(e-num (value "23"))))
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
											(p-num (value "1"))))))
							(value
								(e-num (value "123"))))
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
									(p-record-destructure
										(destructs))))
							(value
								(e-num (value "12"))))
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
			(ty-malformed)))
	(d-let
		(p-assign (ident "ma"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-expr
					(e-lookup-local
						(p-assign (ident "e"))))
				(s-let
					(p-assign (ident "w"))
					(e-string
						(e-literal (string "d"))))
				(s-var
					(p-assign (ident "er"))
					(e-num (value "123")))
				(s-expect
					(e-runtime-error (tag "ident_not_in_scope")))
				(s-return
					(e-runtime-error (tag "ident_not_in_scope")))
				(s-expr
					(e-not-implemented))
				(s-expr
					(e-call (constraint-fn-var 2309)
						(e-lookup-local
							(p-assign (ident "me")))
						(e-not-implemented)))
				(s-runtime-error (tag "crash_expects_string"))
				(s-expr
					(e-string
						(e-literal (string "Unr!"))))
				(s-let
					(p-assign (ident "i"))
					(e-block
						(s-let
							(p-assign (ident "#interp_0"))
							(e-runtime-error (tag "ident_not_in_scope")))
						(e-interpolation
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
									(p-assign (ident "one")))
								(e-lookup-local
									(p-assign (ident "er"))))
							(e-num (value "456"))
							(e-num (value "9")))))
				(s-for
					(p-assign (ident "n"))
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-block
						(s-expr
							(e-call
								(e-runtime-error (tag "ident_not_in_scope"))
								(e-block
									(s-let
										(p-assign (ident "#interp_1"))
										(e-lookup-local
											(p-assign (ident "n"))))
									(s-let
										(p-assign (ident "#interp_2"))
										(e-lookup-local
											(p-assign (ident "er"))))
									(e-interpolation (constraint-fn-var 2593)
										(first
											(e-literal (string "Ag ")))
										(parts
											(e-lookup-local
												(p-assign (ident "#interp_1")))
											(e-literal (string " to "))
											(e-lookup-local
												(p-assign (ident "#interp_2")))
											(e-literal (string "")))))))
						(e-dispatch-call (method "plus") (constraint-fn-var 2596)
							(receiver
								(e-runtime-error (tag "ident_not_in_scope")))
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
								(e-runtime-error (tag "ident_not_in_scope")))
							(field (name "qux")
								(e-tag (name "Ok")
									(args
										(e-runtime-error (tag "ident_not_in_scope")))))
							(field (name "ned")
								(e-runtime-error (tag "ident_not_in_scope"))))))
				(s-let
					(p-assign (ident "t"))
					(e-tuple
						(elems
							(e-num (value "123"))
							(e-string
								(e-literal (string "World")))
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-tag (name "O"))
							(e-tuple
								(elems
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-runtime-error (tag "self_referential_definition"))))
							(e-list
								(elems
									(e-num (value "1"))
									(e-num (value "2"))
									(e-num (value "3")))))))
				(s-expr
					(e-runtime-error (tag "ident_not_in_scope")))
				(s-expr
					(e-tuple
						(elems
							(e-num (value "123"))
							(e-string
								(e-literal (string "World")))
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-tag (name "O"))
							(e-tuple
								(elems
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-runtime-error (tag "ident_not_in_scope"))))
							(e-list
								(elems
									(e-num (value "1"))
									(e-num (value "2"))
									(e-num (value "3")))))))
				(s-expr
					(e-if
						(if-branches
							(if-branch
								(e-dispatch-call (method "is_gt") (constraint-fn-var 3042)
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
										(e-num (value "5"))))
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
													(e-dispatch-call (method "is_lt") (constraint-fn-var 3159)
														(receiver
															(e-dispatch-call (method "plus") (constraint-fn-var 3121)
																(receiver
																	(e-num (value "13")))
																(args
																	(e-num (value "2")))))
														(args
															(e-num (value "5"))))
													(e-dispatch-call (method "is_gte") (constraint-fn-var 3268)
														(receiver
															(e-dispatch-call (method "minus") (constraint-fn-var 3230)
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
									(e-dispatch-call (method "is_lte") (constraint-fn-var 3352)
										(receiver
											(e-num (value "12")))
										(args
											(e-num (value "3")))))))))
				(s-expr
					(e-match
						(match
							(cond
								(e-field-access (field "recd")
									(receiver
										(e-match
											(match
												(cond
													(e-dispatch-call (method "ned") (constraint-fn-var 3419)
														(receiver
															(e-match
																(match
																	(cond
																		(e-dispatch-call (method "od") (constraint-fn-var 3386)
																			(receiver
																				(e-match
																					(match
																						(cond
																							(e-call
																								(e-runtime-error (tag "ident_not_in_scope"))
																								(e-runtime-error (tag "ident_not_in_scope"))))
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
				(e-tag (name "Stdo!")
					(args
						(e-block
							(s-let
								(p-assign (ident "#interp_3"))
								(e-call
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-runtime-error (tag "ident_not_in_scope"))))
							(e-interpolation
								(first
									(e-literal (string "Ho")))
								(parts
									(e-lookup-local
										(p-assign (ident "#interp_3")))
									(e-literal (string " "))))))))))
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
			(exposed (name "line!") (wildcard false))))
	(s-import (module "Stdot")
		(exposes
			(exposed (name "Cust") (wildcard false))))
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
			(ty-malformed)
			(ty-parens
				(ty-malformed))
			(ty-apply (name "List") (builtin)
				(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
	(s-alias-decl
		(ty-header (name "MapML"))
		(ty-fn (effectful false)
			(ty-apply (name "List") (builtin))
			(ty-parens
				(ty-malformed))
			(ty-apply (name "List") (builtin)
				(ty-malformed))))
	(s-alias-decl
		(ty-header (name "Som"))
		(ty-record
			(field (field "foo")
				(ty-malformed))
			(field (field "bar")
				(ty-malformed))))
	(s-alias-decl
		(ty-header (name "Ml")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-record))
	(s-alias-decl
		(ty-header (name "Soine")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-record))
	(s-alias-decl
		(ty-header (name "Maybe")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Somne"))))
	(s-alias-decl
		(ty-header (name "Mayine")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union))
	(s-expect
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-expect
		(e-block
			(s-expr
				(e-method-eq (negated "false")
					(lhs
						(e-runtime-error (tag "ident_not_in_scope")))
					(rhs
						(e-num (value "1")))))
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
		(patt (type "()"))
		(patt (type "Bool -> f where [f.from_numeral : Numeral -> Try(f, [InvalidNumeral(Str)])]"))
		(patt (type "Error"))
		(patt (type "[Rum] -> Error"))
		(patt (type "[Blue, ..] -> Error"))
		(patt (type "Error"))
		(patt (type "_arg -> Error"))
		(patt (type "{}"))
		(patt (type "{}"))
		(patt (type "Error")))
	(type_decls
		(alias (type "Map(a, b)")
			(ty-header (name "Map")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(alias (type "MapML")
			(ty-header (name "MapML")))
		(alias (type "Som")
			(ty-header (name "Som")))
		(alias (type "Ml(a)")
			(ty-header (name "Ml")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Soine(a)")
			(ty-header (name "Soine")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Maybe(a)")
			(ty-header (name "Maybe")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Mayine(a)")
			(ty-header (name "Mayine")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "()"))
		(expr (type "Bool -> f where [f.from_numeral : Numeral -> Try(f, [InvalidNumeral(Str)])]"))
		(expr (type "Error"))
		(expr (type "[Rum] -> Error"))
		(expr (type "[Blue, ..] -> Error"))
		(expr (type "Error"))
		(expr (type "_arg -> Error"))
		(expr (type "{}"))
		(expr (type "{}"))
		(expr (type "Error"))))
~~~
