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
add = |num| {
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
	a, Tb,
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
MISSING MATCH ARROW - fuzz_crash_019.md:52:16:52:16
MISSING MATCH ARROW - fuzz_crash_019.md:58:4:58:4
MISSING MATCH ARROW - fuzz_crash_019.md:59:3:59:3
MISSING MATCH ARROW - fuzz_crash_019.md:60:16:60:16
MISSING MATCH ARROW - fuzz_crash_019.md:62:5:62:5
MISSING MATCH ARROW - fuzz_crash_019.md:63:7:63:7
MISSING MATCH ARROW - fuzz_crash_019.md:66:12:66:12
EXPECTED RECORD ACCESSOR - fuzz_crash_019.md:83:2:83:5
MOD NOT FOUND - fuzz_crash_019.md:6:1:8:6
MOD NOT FOUND - fuzz_crash_019.md:10:1:10:19
MOD NOT FOUND - fuzz_crash_019.md:11:1:12:4
UNDECLARED TYPE - fuzz_crash_019.md:13:13:13:16
UNDECLARED TYPE VARIABLE - fuzz_crash_019.md:13:19:13:21
UNDECLARED TYPE VARIABLE - fuzz_crash_019.md:19:4:19:6
UNDECLARED TYPE VARIABLE - fuzz_crash_019.md:20:12:20:13
UNDECLARED TYPE - fuzz_crash_019.md:24:15:24:16
UNDECLARED TYPE VARIABLE - fuzz_crash_019.md:24:24:24:25
UNDECLARED TYPE - fuzz_crash_019.md:37:7:37:9
NAME NOT IN SCOPE - fuzz_crash_019.md:42:4:42:5
NAME NOT IN SCOPE - fuzz_crash_019.md:42:6:42:10
NAME NOT IN SCOPE - fuzz_crash_019.md:45:3:45:4
NAME NOT IN SCOPE - fuzz_crash_019.md:53:2:53:3
UNUSED VARIABLE - fuzz_crash_019.md:52:11:52:14
NAME NOT IN SCOPE - fuzz_crash_019.md:55:11:55:12
UNUSED VARIABLE - fuzz_crash_019.md:57:2:57:4
NAME NOT IN SCOPE - fuzz_crash_019.md:59:3:59:7
UNUSED VARIABLE - fuzz_crash_019.md:60:12:60:15
NAME NOT IN SCOPE - fuzz_crash_019.md:72:2:72:4
UNDECLARED TYPE - fuzz_crash_019.md:74:9:74:15
NAME NOT IN SCOPE - fuzz_crash_019.md:78:9:78:14
UNRECOGNIZED SYNTAX - fuzz_crash_019.md:83:2:83:5
NAME NOT IN SCOPE - fuzz_crash_019.md:86:9:86:11
NAME NOT IN SCOPE - fuzz_crash_019.md:87:11:87:12
NAME NOT IN SCOPE - fuzz_crash_019.md:92:11:92:15
NAME NOT IN SCOPE - fuzz_crash_019.md:93:2:93:7
NAME NOT IN SCOPE - fuzz_crash_019.md:94:3:94:6
NAME NOT IN SCOPE - fuzz_crash_019.md:96:34:96:37
NAME NOT IN SCOPE - fuzz_crash_019.md:96:47:96:52
NAME NOT IN SCOPE - fuzz_crash_019.md:96:54:96:57
DUPLICATE DEFINITION - fuzz_crash_019.md:97:2:97:3
NAME NOT IN SCOPE - fuzz_crash_019.md:97:21:97:24
NAME NOT IN SCOPE - fuzz_crash_019.md:97:30:97:32
INVALID ASSIGNMENT TO ITSELF - fuzz_crash_019.md:97:34:97:35
NAME NOT IN SCOPE - fuzz_crash_019.md:98:2:98:3
NAME NOT IN SCOPE - fuzz_crash_019.md:100:11:100:14
NAME NOT IN SCOPE - fuzz_crash_019.md:102:4:102:6
NAME NOT IN SCOPE - fuzz_crash_019.md:102:8:102:13
NAME NOT IN SCOPE - fuzz_crash_019.md:105:2:105:3
NAME NOT IN SCOPE - fuzz_crash_019.md:105:55:105:59
NAME NOT IN SCOPE - fuzz_crash_019.md:105:60:105:64
NAME NOT IN SCOPE - fuzz_crash_019.md:108:4:108:5
NAME NOT IN SCOPE - fuzz_crash_019.md:108:6:108:8
UNUSED VARIABLE - fuzz_crash_019.md:76:2:76:3
UNUSED VARIABLE - fuzz_crash_019.md:87:2:87:3
UNUSED VARIABLE - fuzz_crash_019.md:96:2:96:4
UNUSED VARIABLE - fuzz_crash_019.md:97:2:97:3
UNDECLARED TYPE - fuzz_crash_019.md:116:5:116:6
NAME NOT IN SCOPE - fuzz_crash_019.md:119:2:119:5
NAME NOT IN SCOPE - fuzz_crash_019.md:120:1:120:2
NAME NOT IN SCOPE - fuzz_crash_019.md:120:6:120:9
EXPOSED BUT NOT DEFINED - fuzz_crash_019.md:2:6:2:11
TOO FEW ARGS - fuzz_crash_019.md:17:3:18:4
DECLARATION HAS NO VALUE - fuzz_crash_019.md:22:1:23:2
DECLARATION HAS NO VALUE - fuzz_crash_019.md:37:1:37:9
MISSING METHOD - fuzz_crash_019.md:39:2:39:3
MISSING METHOD - fuzz_crash_019.md:58:6:58:11
TYPE MISMATCH - fuzz_crash_019.md:52:2:52:2
DECLARATION HAS NO VALUE - fuzz_crash_019.md:74:1:74:22
DECLARATION HAS NO VALUE - fuzz_crash_019.md:113:1:113:7
TOO FEW ARGS - fuzz_crash_019.md:84:2:86:3
MISSING METHOD - fuzz_crash_019.md:86:11:86:17
REFERENCE HAS NO VALUE - fuzz_crash_019.md:89:3:89:6
TYPE MISMATCH - fuzz_crash_019.md:98:4:104:3
TYPE MISMATCH - fuzz_crash_019.md:105:2:105:54
TYPE MISMATCH - fuzz_crash_019.md:93:22:93:24
DECLARATION HAS NO VALUE - fuzz_crash_019.md:116:1:116:13
MISSING METHOD - fuzz_crash_019.md:105:55:105:66
MISSING METHOD - fuzz_crash_019.md:105:55:105:72
# PROBLEMS
── ✗ missing match arrow ─────────────────────────────── fuzz_crash_019.md:52:16

I was parsing a match branch, and I expected `=>` before the branch body.

match a {lue  {
              ^

Add => after the pattern or guard.

For example:
    Err(msg) => crash msg

I reached the end of the file before this construct was complete.

── ✗ missing match arrow ──────────────────────────────── fuzz_crash_019.md:58:4

I was parsing a match branch, and I expected `=>` before the branch body.

1 "for" => 20[1, ] # t
^

Add => after the pattern or guard.

For example:
    Err(msg) => crash msg

I reached the end of the file before this construct was complete.

── ✗ missing match arrow ──────────────────────────────── fuzz_crash_019.md:59:3

I was parsing a match branch, and I expected `=>` before the branch body.

ment
^

Add => after the pattern or guard.

For example:
    Err(msg) => crash msg

I reached the end of the file before this construct was complete.

── ✗ missing match arrow ─────────────────────────────── fuzz_crash_019.md:60:16

I was parsing a match branch, and I expected `=>` before the branch body.

[1, 2, 3,est]123
             ^

Add => after the pattern or guard.

For example:
    Err(msg) => crash msg

I reached the end of the file before this construct was complete.

── ✗ missing match arrow ──────────────────────────────── fuzz_crash_019.md:62:5

I was parsing a match branch, and I expected `=>` before the branch body.

] 23
  ^

Add => after the pattern or guard.

For example:
    Err(msg) => crash msg

I reached the end of the file before this construct was complete.

── ✗ missing match arrow ──────────────────────────────── fuzz_crash_019.md:63:7

I was parsing a match branch, and I expected `=>` before the branch body.

3.1 314
    ^

Add => after the pattern or guard.

For example:
    Err(msg) => crash msg

I reached the end of the file before this construct was complete.

── ✗ missing match arrow ─────────────────────────────── fuzz_crash_019.md:66:12

I was parsing a match branch, and I expected `=>` before the branch body.

(1, 2, 3)123
         ^

Add => after the pattern or guard.

For example:
    Err(msg) => crash msg

I reached the end of the file before this construct was complete.

── ✗ expected record accessor ─────────────────────────── fuzz_crash_019.md:83:2

I was parsing access after `.`, and I expected a field name or tuple index.

...
^^^

Required record access uses .name, optional record access uses .?name, and
tuple access uses .0. Accessor names must be lowercase and adjacent to their
punctuation.

For example:
    person.name
    maybe_person.?name
    pair.0

I found ... here.

── ✗ mod not found ──────────────────────────────────── fuzz_crash_019.md:6:1

The mod Stdot was not found in this Roc project.

import Stdot
        exposing [ #tem
Cust]

── ✗ mod not found ─────────────────────────────────── fuzz_crash_019.md:10:1

The mod Bae was not found in this Roc project.

import Bae as Gooe
^^^^^^^^^^^^^^^^^^

── ✗ mod not found ─────────────────────────────────── fuzz_crash_019.md:11:1

The mod Ba was not found in this Roc project.

import
    Ba

── ✗ undeclared type ─────────────────────────────────── fuzz_crash_019.md:13:13

The type Lis is not declared in this scope.

Map(a, b) : Lis, (ab) -> List(b)
            ^^^

── ✗ undeclared type variable ────────────────────────── fuzz_crash_019.md:13:19

The type variable ab is not declared in this scope.

Map(a, b) : Lis, (ab) -> List(b)
                  ^^

Type variables must be introduced in a type annotation before they can be used.

── ✗ undeclared type variable ─────────────────────────── fuzz_crash_019.md:19:4

The type variable ab is not declared in this scope.

(ab) -> # row
 ^^

Type variables must be introduced in a type annotation before they can be used.

── ✗ undeclared type variable ────────────────────────── fuzz_crash_019.md:20:12

The type variable b is not declared in this scope.

List(   b ) #z)
        ^

Type variables must be introduced in a type annotation before they can be used.

── ✗ undeclared type ─────────────────────────────────── fuzz_crash_019.md:24:15

The type O is not declared in this scope.

Som : { foo : O, bar : g }
              ^

── ✗ undeclared type variable ────────────────────────── fuzz_crash_019.md:24:24

The type variable g is not declared in this scope.

Som : { foo : O, bar : g }
                       ^

Type variables must be introduced in a type annotation before they can be used.

── ✗ undeclared type ──────────────────────────────────── fuzz_crash_019.md:37:7

The type U6 is not declared in this scope.

one : U6
      ^^

── ✗ name not in scope ────────────────────────────────── fuzz_crash_019.md:42:4

Nothing is named s in this scope.

s exp0
^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ────────────────────────────────── fuzz_crash_019.md:42:6

Nothing is named exp0 in this scope.

s exp0
  ^^^^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ────────────────────────────────── fuzz_crash_019.md:45:3

Nothing is named r in this scope.

r
^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ────────────────────────────────── fuzz_crash_019.md:53:2

Nothing is named x in this scope.

x
^

Is it misspelled, or is there an import missing?

── ● unused variable ─────────────────────────────────── fuzz_crash_019.md:52:11

Variable lue is defined here and then never used:

match a {lue  {
         ^^^

If you don't need this variable, prefix it with an underscore like _lue to
suppress this warning.

── ✗ name not in scope ───────────────────────────────── fuzz_crash_019.md:55:11

Nothing is named x in this scope.

Blue=> {x
        ^

Is it misspelled, or is there an import missing?

── ● unused variable ──────────────────────────────────── fuzz_crash_019.md:57:2

Variable er is defined here and then never used:

er #ent
^^

If you don't need this variable, prefix it with an underscore like _er to
suppress this warning.

── ✗ name not in scope ────────────────────────────────── fuzz_crash_019.md:59:3

Nothing is named ment in this scope.

ment
^^^^

Is it misspelled, or is there an import missing?

── ● unused variable ─────────────────────────────────── fuzz_crash_019.md:60:12

Variable est is defined here and then never used:

[1, 2, 3,est]123
         ^^^

If you don't need this variable, prefix it with an underscore like _est to
suppress this warning.

── ✗ name not in scope ────────────────────────────────── fuzz_crash_019.md:72:2

Nothing is named nt in this scope.

nt
^^

Is it misspelled, or is there an import missing?

── ✗ undeclared type ──────────────────────────────────── fuzz_crash_019.md:74:9

The type Listlt is not declared in this scope.

main! : Listlt({}, _)
        ^^^^^^

── ✗ name not in scope ────────────────────────────────── fuzz_crash_019.md:78:9

Nothing is named blaue in this scope.

expect blaue
       ^^^^^

Is it misspelled, or is there an import missing?

── ✗ unrecognized syntax ──────────────────────────────── fuzz_crash_019.md:83:2

I don't recognize this syntax.

...
^^^

This might be a syntax error, an unsupported language feature, or a typo.

── ✗ name not in scope ────────────────────────────────── fuzz_crash_019.md:86:9

Nothing is named ke in this scope.

)crash ke"Unr!" #)
       ^^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ───────────────────────────────── fuzz_crash_019.md:87:11

Nothing is named d in this scope.

i= "H, ${d}"
         ^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ───────────────────────────────── fuzz_crash_019.md:92:11

Nothing is named list in this scope.

for n in list {
         ^^^^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ────────────────────────────────── fuzz_crash_019.md:93:2

Nothing is named line! in this scope.

line!("Ag ${n} to ${er}")
^^^^^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ────────────────────────────────── fuzz_crash_019.md:94:3

Nothing is named ber in this scope.

ber + n
^^^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ───────────────────────────────── fuzz_crash_019.md:96:34

Nothing is named tag in this scope.

rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
                                ^^^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ───────────────────────────────── fuzz_crash_019.md:96:47

Nothing is named world in this scope.

rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
                                             ^^^^^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ───────────────────────────────── fuzz_crash_019.md:96:54

Nothing is named ned in this scope.

rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
                                                    ^^^

Is it misspelled, or is there an import missing?

── ● duplicate definition ─────────────────────────────── fuzz_crash_019.md:97:2

The name t is being redeclared here:

t = (123, "World", tag, O, (nd, t), [1, 2, 3])
^

In this scope, t was already defined in fuzz_crash_019.md:88:1:

t = [
^

── ✗ name not in scope ───────────────────────────────── fuzz_crash_019.md:97:21

Nothing is named tag in this scope.

t = (123, "World", tag, O, (nd, t), [1, 2, 3])
                   ^^^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ───────────────────────────────── fuzz_crash_019.md:97:30

Nothing is named nd in this scope.

t = (123, "World", tag, O, (nd, t), [1, 2, 3])
                            ^^

Is it misspelled, or is there an import missing?

── ✗ invalid assignment to itself ────────────────────── fuzz_crash_019.md:97:34

The value t is assigned to itself, which would cause an infinite loop at
runtime.

t = (123, "World", tag, O, (nd, t), [1, 2, 3])
                                ^

Only functions can reference themselves (for recursion). For non-function
values, the right-hand side must be fully computable without referring to the
value being assigned.

── ✗ name not in scope ────────────────────────────────── fuzz_crash_019.md:98:2

Nothing is named m in this scope.

m (
^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ──────────────────────────────── fuzz_crash_019.md:100:11

Nothing is named ag1 in this scope.

"World",ag1,
        ^^^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ───────────────────────────────── fuzz_crash_019.md:102:4

Nothing is named ne in this scope.

(ne, tuple),
 ^^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ───────────────────────────────── fuzz_crash_019.md:102:8

Nothing is named tuple in this scope.

(ne, tuple),
     ^^^^^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ───────────────────────────────── fuzz_crash_019.md:105:2

Nothing is named b in this scope.

b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ──────────────────────────────── fuzz_crash_019.md:105:55

Nothing is named e_fn in this scope.

b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
                                                     ^^^^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ──────────────────────────────── fuzz_crash_019.md:105:60

Nothing is named arg1 in this scope.

b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
                                                          ^^^^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ───────────────────────────────── fuzz_crash_019.md:108:4

Nothing is named r in this scope.

r(nu) # xpr
^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ───────────────────────────────── fuzz_crash_019.md:108:6

Nothing is named nu in this scope.

r(nu) # xpr
  ^^

Is it misspelled, or is there an import missing?

── ● unused variable ──────────────────────────────────── fuzz_crash_019.md:76:2

Variable w is defined here and then never used:

w = "d"
^

If you don't need this variable, prefix it with an underscore like _w to
suppress this warning.

── ● unused variable ──────────────────────────────────── fuzz_crash_019.md:87:2

Variable i is defined here and then never used:

i= "H, ${d}"
^

If you don't need this variable, prefix it with an underscore like _i to
suppress this warning.

── ● unused variable ──────────────────────────────────── fuzz_crash_019.md:96:2

Variable rd is defined here and then never used:

rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
^^

If you don't need this variable, prefix it with an underscore like _rd to
suppress this warning.

── ● unused variable ──────────────────────────────────── fuzz_crash_019.md:97:2

Variable t is defined here and then never used:

t = (123, "World", tag, O, (nd, t), [1, 2, 3])
^

If you don't need this variable, prefix it with an underscore like _t to
suppress this warning.

── ✗ undeclared type ─────────────────────────────────── fuzz_crash_019.md:116:5

The type V is not declared in this scope.

t : V((a,c))
    ^

── ✗ name not in scope ───────────────────────────────── fuzz_crash_019.md:119:2

Nothing is named foo in this scope.

foo == 1
^^^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ───────────────────────────────── fuzz_crash_019.md:120:1

Nothing is named h in this scope.

h == foo
^

Is it misspelled, or is there an import missing?

── ✗ name not in scope ───────────────────────────────── fuzz_crash_019.md:120:6

Nothing is named foo in this scope.

h == foo
     ^^^

Is it misspelled, or is there an import missing?

── ✗ exposed but not defined ───────────────────────────── fuzz_crash_019.md:2:6

The mod header says that main! is exposed, but it is not defined anywhere in
this mod.

app [main!] { pf: platform "c" }
     ^^^^^

You can fix this by either defining main! in this mod, or by removing it
from the list of exposed values.

── ✗ too few args ─────────────────────────────────────── fuzz_crash_019.md:17:3

The type List expects 1 argument, but got 0 instead.

List( #rg
),

── ● declaration has no value ─────────────────────────── fuzz_crash_019.md:22:1

This declaration has a type annotation but no implementation.

line : ( # Cm
) # Co

Add a value body here, or put hosted functions in a platform type mod so
they are published through the host boundary.

── ● declaration has no value ─────────────────────────── fuzz_crash_019.md:37:1

This declaration has a type annotation but no implementation.

one : U6
^^^^^^^^

Add a value body here, or put hosted functions in a platform type mod so
they are published through the host boundary.

── ✗ missing method ───────────────────────────────────── fuzz_crash_019.md:39:2

This from_numeral method is being called on a value whose type doesn't have
that method.

1
^

The value's type, which does not have a method named from_numeral, is:

    {}

── ✗ missing method ───────────────────────────────────── fuzz_crash_019.md:58:6

This from_quote method is being called on a value whose type doesn't have that
method.

1 "for" => 20[1, ] # t
  ^^^^^

The value's type, which does not have a method named from_quote, is:

    [Blue, ..]

── ✗ type mismatch ───────────────────────────────────── fuzz_crash_019.md:52:17

The fifth branch of this match does not match the previous ones.

match a {lue  {
x
    }
    Blue=> {x
        }
er #ent
        1 "for" => 20[1, ] # t
    ment
    [1, 2, 3,est]123
    [
    ] 23
    3.1 314
    3.14 | 6.28 => 314
    (1, ) => 123
    (1, 2, 3)123
    {  } => 12
    Ok(123) => 12
}

This fifth branch is trying to match:

    List(f)
      where [
        f.from_numeral : Numeral -> Try(f, [InvalidNumeral(Str)]),
        f.is_eq : f, f -> Bool,
      ]

But the expression between the match parenthesis has the type:

    [Blue, ..]

These can never match! Either the pattern or expression has a problem.

── ● declaration has no value ─────────────────────────── fuzz_crash_019.md:74:1

This declaration has a type annotation but no implementation.

main! : Listlt({}, _)
^^^^^^^^^^^^^^^^^^^^^

Add a value body here, or put hosted functions in a platform type mod so
they are published through the host boundary.

── ● declaration has no value ────────────────────────── fuzz_crash_019.md:113:1

This declaration has a type annotation but no implementation.

y : {}
^^^^^^

Add a value body here, or put hosted functions in a platform type mod so
they are published through the host boundary.

── ✗ too few args ─────────────────────────────────────── fuzz_crash_019.md:84:2

The me function expects 2 arguments, but it got 1 instead.

me(
    ..., # r
)crash ke"Unr!" #)

The me function has the type:

    [Blue, ..], [Tb] -> Error

Are there any missing commas?

── ✗ missing method ──────────────────────────────────── fuzz_crash_019.md:86:11

This from_quote method is being called on a value whose type doesn't have that
method.

)crash ke"Unr!" #)
         ^^^^^^

The value's type, which does not have a method named from_quote, is:

    {}

── ✗ reference has no value ───────────────────────────── fuzz_crash_019.md:89:3

This refers to a declaration that has a type annotation but no implementation,
so there is no value here to use.

one(er,   ), 456, # two
^^^

Give that declaration a value body, or stop referring to it here.

── ✗ type mismatch ────────────────────────────────────── fuzz_crash_019.md:98:4

This expression produces a value, but it's not being used.

m (
    123,
    "World",ag1,
    O, # nt
    (ne, tuple),
    [1, 2, 3],
)

It has the type:

    (f, j, Error, [O, ..], (Error, Error), List(l))
      where [
        f.from_numeral : Numeral -> Try(f, [InvalidNumeral(Str)]),
        j.from_quote : Str -> Try(j, [BadQuotedBytes(Str)]),
        l.from_numeral : Numeral -> Try(l, [InvalidNumeral(Str)]),
      ]

Since this expression is used as a statement, it must evaluate to {}.
If you don't need the value, you can ignore it with _ =.

── ✗ type mismatch ───────────────────────────────────── fuzz_crash_019.md:105:2

This expression produces a value, but it's not being used.

b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It has the type:

    Bool

Since this expression is used as a statement, it must evaluate to {}.
If you don't need the value, you can ignore it with _ =.

── ✗ type mismatch ───────────────────────────────────── fuzz_crash_019.md:93:22

This expression is used in an unexpected way.

line!("Ag ${n} to ${er}")
                    ^^

It has the type:

    Dec

But you are trying to use it as:

    Str

── ● declaration has no value ────────────────────────── fuzz_crash_019.md:116:1

This declaration has a type annotation but no implementation.

t : V((a,c))
^^^^^^^^^^^^

Add a value body here, or put hosted functions in a platform type mod so
they are published through the host boundary.

── ✗ missing method ─────────────────────────────────── fuzz_crash_019.md:105:55

This is trying to dispatch a method named od on an unresolved type variable,
but unresolved type variables have no methods.

b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
                                                     ^^^^^^^^^^^

Hint: You can replace this static dispatch call with an ordinary function call,
or force the type variable to become more concrete—for example, by adding a
type annotation that narrows its type to something that actually has methods.

── ✗ missing method ─────────────────────────────────── fuzz_crash_019.md:105:55

This is trying to dispatch a method named ned on an unresolved type variable,
but unresolved type variables have no methods.

b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
                                                     ^^^^^^^^^^^^^^^^^

Hint: You can replace this static dispatch call with an ordinary function call,
or force the type variable to become more concrete—for example, by adding a
type annotation that narrows its type to something that actually has methods.

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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
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
LowerIdent,Comma,UpperIdent,Comma,
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
					(p-ident (raw "num")))
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
					(p-ident (raw "a"))
					(p-tag (raw "Tb")))
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
							(e-malformed (reason "expr_dot_suffix_not_allowed")))
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
								(receiver
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
											(args))))
								(segment (mode "required") (field "recd"))))
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
		List( # rg
		),
		(ab) -> # row
			List(b) # z)

line : ( # Cm
) # Co

Som : { foo : O, bar : g }

Ml(a) : { # ld
}

Soine(a) : { #
} #

Maybe(a) : [Somne]

Mayine(a) : [] # )

ane = |num| if num 2 else 5

one : U6

add = |num| {
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
	a,
	Tb,
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
	return

	#
		
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
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "add"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "me"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "main!"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "ma"))
		(e-runtime-error (tag "erroneous_value_expr")))
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
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed)))
	(s-import (mod "pf.Stdout")
		(exposes
			(exposed (name "line!") (wildcard false))))
	(s-import (mod "Stdot")
		(exposes
			(exposed (name "Cust") (wildcard false))))
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
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "()"))
		(patt (type "Bool -> f where [f.from_numeral : Numeral -> Try(f, [InvalidNumeral(Str)])]"))
		(patt (type "Error"))
		(patt (type "Bool -> Error"))
		(patt (type "[Blue, ..], [Tb] -> Error"))
		(patt (type "Error"))
		(patt (type "_arg -> Error"))
		(patt (type "{}"))
		(patt (type "{}"))
		(patt (type "Error")))
	(type_decls
		(alias (type "Error")
			(ty-header (name "Map")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(alias (type "Error")
			(ty-header (name "MapML")))
		(alias (type "Error")
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
		(expr (type "Bool -> Error"))
		(expr (type "[Blue, ..], [Tb] -> Error"))
		(expr (type "Error"))
		(expr (type "_arg -> Error"))
		(expr (type "{}"))
		(expr (type "{}"))
		(expr (type "Error"))))
~~~
