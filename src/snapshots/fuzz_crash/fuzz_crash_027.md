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
# EXPECTED
PARSE ERROR - fuzz_crash_027.md:34:12:35:2
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_027.md:35:1:35:1
PARSE ERROR - fuzz_crash_027.md:1:1:39:2
# PROBLEMS
**OVER CLOSED BRACE**
There are too many closing braces here.

**LEADING ZERO**
Numbers cannot have leading zeros.

**UNCLOSED STRING**
This string is missing a closing quote.

**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**PARSE ERROR**
A parsing error occurred: `expected_ty_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_027.md:34:12:35:2:**
```roc
	bar : Som# Afld
}
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_027.md:35:1:35:1:**
```roc
}
```



**PARSE ERROR**
A parsing error occurred: `expected_ty_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_027.md:1:1:39:2:**
```roc
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
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} #** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_027.md:39:1:39:4:**
```roc
} #
```
^^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_apply_close_round`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_027.md:122:3:122:11:**
```roc
		add_one(
```
  ^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,	456** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_027.md:125:4:125:9:**
```roc
		),	456, # ee
```
   ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, # ee** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_027.md:125:9:125:15:**
```roc
		),	456, # ee
```
        ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_027.md:126:2:126:2:**
```roc
	]
```
 


**UNEXPECTED TOKEN IN EXPRESSION**
The token **} # Commenl decl** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_027.md:148:1:148:17:**
```roc
} # Commenl decl
```
^^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type ``Bar`` is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:26:8:26:11:**
```roc
Foo : (Bar, Baz)
```
       ^^^


**UNDECLARED TYPE**
The type ``Baz`` is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:26:13:26:16:**
```roc
Foo : (Bar, Baz)
```
            ^^^


**UNDECLARED TYPE**
The type ``Ok`` is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:32:19:32:21:**
```roc
Some(a) : { foo : Ok(a), bar : g }
```
                  ^^


**UNDECLARED TYPE VARIABLE**
The type variable ``g`` is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**fuzz_crash_027.md:32:32:32:33:**
```roc
Some(a) : { foo : Ok(a), bar : g }
```
                               ^


**UNDECLARED TYPE**
The type ``Ok`` is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:32:19:32:24:**
```roc
Some(a) : { foo : Ok(a), bar : g }
```
                  ^^^^^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**UNDECLARED TYPE**
The type ``Maybe`` is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:43:11:43:16:**
```roc
Func(a) : Maybe(a), a -> Maybe(a)
```
          ^^^^^


**UNDECLARED TYPE**
The type ``Maybe`` is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:43:26:43:31:**
```roc
Func(a) : Maybe(a), a -> Maybe(a)
```
                         ^^^^^


**UNDECLARED TYPE**
The type ``Maybe`` is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:43:11:43:19:**
```roc
Func(a) : Maybe(a), a -> Maybe(a)
```
          ^^^^^^^^


**UNDECLARED TYPE**
The type ``Maybe`` is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:43:26:43:34:**
```roc
Func(a) : Maybe(a), a -> Maybe(a)
```
                         ^^^^^^^^


**UNDECLARED TYPE**
The type ``Bar`` is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:29:2:29:5:**
```roc
	Bar, #
```
 ^^^


**UNDECLARED TYPE**
The type ``Baz`` is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:30:2:30:5:**
```roc
	Baz, #m
```
 ^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_027.md:1:1:1:1:**
```roc

```



**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_027.md:39:1:39:4:**
```roc
} #
```
^^^


**EMPTY TUPLE NOT ALLOWED**
I am part way through parsing this tuple, but it is empty:
**fuzz_crash_027.md:52:1:52:3:**
```roc
() #r
```
^^

If you want to represent nothing, try using an empty record: `{}`.

**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_027.md:65:4:65:5:**
```roc
			x x
```
   ^


**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_027.md:65:6:65:7:**
```roc
			x x
```
     ^


**UNUSED VARIABLE**
Variable ``lue`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_lue` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:64:11:64:14:**
```roc
	match a {lue | Red => {
```
          ^^^


**UNDEFINED VARIABLE**
Nothing is named `ment` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_027.md:71:7:71:11:**
```roc
			=> ment
```
      ^^^^


**UNUSED VARIABLE**
Variable ``rest`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:70:38:70:42:**
```roc
		"foo" | "bar" => 20[1, 2, 3, .. as rest] # Aftet
```
                                     ^^^^


**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: alternatives pattern outside match expression
Let us know if you want to help!

**UNUSED VARIABLE**
Variable ``rest`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:74:23:74:27:**
```roc
		[1, 2 | 5, 3, .. as rest] => 123
```
                      ^^^^


**UNUSED VARIABLE**
Variable ``ist`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_ist` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:76:1:76:4:**
```roc
ist
```
^^^


**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: alternatives pattern outside match expression
Let us know if you want to help!

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize local_dispatch expression
Let us know if you want to help!

**UNUSED VARIABLE**
Variable ``rest`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:82:21:82:29:**
```roc
		{ foo: 1, bar: 2, ..rest } => 12->add(34)
```
                    ^^^^^^^^


**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: report an error when unable to resolve field identifier
Let us know if you want to help!

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: alternatives pattern outside match expression
Let us know if you want to help!

**UNUSED VARIABLE**
Variable ``b`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:62:2:62:3:**
```roc
	b,
```
 ^


**UNDEFINED VARIABLE**
Nothing is named `blah` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_027.md:97:2:97:6:**
```roc
	blah == 1 # Commnt
```
 ^^^^


**UNDECLARED TYPE**
The type ``String`` is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:99:14:99:20:**
```roc
main! : List(String) -> Result({}, _)
```
             ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `blah` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_027.md:103:9:103:13:**
```roc
	expect blah == 1
```
        ^^^^


**UNDEFINED VARIABLE**
Nothing is named `some_func` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_027.md:114:2:114:11:**
```roc
	some_func(
```
 ^^^^^^^^^


**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: statement type in block
Let us know if you want to help!

**UNDEFINED VARIABLE**
Nothing is named `punned` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_027.md:131:63:131:71:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```
                                                              ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `nested` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_027.md:132:42:132:48:**
```roc
	tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
```
                                         ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `tag1` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_027.md:136:3:136:7:**
```roc
		tag1,
```
  ^^^^


**UNDEFINED VARIABLE**
Nothing is named `nested` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_027.md:138:4:138:10:**
```roc
		(nested, tuple),
```
   ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_027.md:141:14:141:17:**
```roc
	bsult = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
```
             ^^^


**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize suffix_single_question expression
Let us know if you want to help!

**UNDEFINED VARIABLE**
Nothing is named `toStr` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_027.md:145:4:145:13:**
```roc
			Num.toStr(number) # on expr
```
   ^^^^^^^^^


**UNDECLARED TYPE**
The type ``Value`` is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:153:9:153:14:**
```roc
tuple : Value((a, b, c))
```
        ^^^^^


**UNUSED VARIABLE**
Variable ``record`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_record` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:131:2:131:8:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```
 ^^^^^^


**UNUSED VARIABLE**
Variable ``list`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_list` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:121:2:121:6:**
```roc
	list = [
```
 ^^^^


**UNUSED VARIABLE**
Variable ``m_tuple`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_m_tuple` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:133:2:133:9:**
```roc
	m_tuple = (
```
 ^^^^^^^


**UNUSED VARIABLE**
Variable ``stale`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_stale` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:142:2:142:7:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
```
 ^^^^^


**UNUSED VARIABLE**
Variable ``empty`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_empty` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:151:1:151:6:**
```roc
empty = {}
```
^^^^^


**UNUSED VARIABLE**
Variable ``tag_with`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_tag_with` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:119:2:119:10:**
```roc
	tag_with = Ok(number)
```
 ^^^^^^^^


**UNUSED VARIABLE**
Variable ``bsult`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_bsult` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:141:2:141:7:**
```roc
	bsult = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
```
 ^^^^^


**UNUSED VARIABLE**
Variable ``ited`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_ited` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:120:2:120:6:**
```roc
	ited = "Hello, ${world}"
```
 ^^^^


**TYPE MISMATCH**
This expression is used in an unexpected way:
**fuzz_crash_027.md:47:11:47:14:**
```roc
add_one : U64 -> U64
```
          ^^^

It is of type:
    _U64_

But you are trying to use it as:
    _Bool_

**INCOMPATIBLE MATCH PATTERNS**
The pattern in the third branch of this `match` differs from previous ones:
**fuzz_crash_027.md:64:2:**
```roc
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
```
  ^

The third pattern has this type:
    _Str_

But all the previous patterns have this type: 
    _[Red, Blue]*_

All patterns in an `match` must have compatible types.



**TYPE MISMATCH**
This expression is used in an unexpected way:
**fuzz_crash_027.md:111:2:111:12:**
```roc
	match_time(
```
 ^^^^^^^^^^

It is of type:
    _Error, * -> Error_

But you are trying to use it as:
    _* -> *_

# TOKENS
~~~zig
Newline(1:2-1:8),
KwApp(2:1-2:4),OpenSquare(2:5-2:6),LowerIdent(2:6-2:11),CloseSquare(2:11-2:12),OpenCurly(2:13-2:14),LowerIdent(2:15-2:17),OpColon(2:17-2:18),KwPlatform(2:19-2:27),StringStart(2:28-2:29),StringPart(2:29-2:30),StringEnd(2:30-2:31),CloseCurly(2:32-2:33),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(4:1-4:7),LowerIdent(4:8-4:10),NoSpaceDotUpperIdent(4:10-4:17),KwExposing(4:18-4:26),OpenSquare(4:27-4:28),LowerIdent(4:28-4:33),Comma(4:33-4:34),LowerIdent(4:35-4:37),CloseSquare(4:37-4:38),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(6:1-6:7),UpperIdent(6:8-6:13),Newline(1:1-1:1),
KwExposing(7:3-7:11),OpenSquare(7:12-7:13),Newline(7:15-7:18),
CloseSquare(8:3-8:4),Newline(8:6-8:11),
Newline(1:1-1:1),
KwImport(10:1-10:7),LowerIdent(10:8-10:11),NoSpaceDotUpperIdent(10:11-10:13),KwExposing(10:14-10:22),OpenSquare(10:23-10:24),LowerIdent(10:24-10:28),KwAs(10:29-10:31),LowerIdent(10:32-10:35),Comma(10:35-10:36),UpperIdent(10:37-10:43),DotStar(10:43-10:45),CloseSquare(10:45-10:46),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(12:1-12:7),UpperIdent(12:8-12:11),KwAs(12:12-12:14),UpperIdent(12:15-12:19),Newline(1:1-1:1),
KwImport(13:1-13:7),Newline(1:1-1:1),
UpperIdent(14:2-14:4),Newline(1:1-1:1),
UpperIdent(15:1-15:4),NoSpaceOpenRound(15:4-15:5),LowerIdent(15:5-15:6),Comma(15:6-15:7),LowerIdent(15:8-15:9),CloseRound(15:9-15:10),OpColon(15:11-15:12),UpperIdent(15:13-15:17),NoSpaceOpenRound(15:17-15:18),LowerIdent(15:18-15:19),CloseRound(15:19-15:20),Comma(15:20-15:21),OpenRound(15:22-15:23),LowerIdent(15:23-15:24),OpArrow(15:25-15:27),LowerIdent(15:28-15:29),CloseRound(15:29-15:30),OpArrow(15:31-15:33),UpperIdent(15:34-15:38),NoSpaceOpenRound(15:38-15:39),LowerIdent(15:39-15:40),CloseRound(15:40-15:41),Newline(1:1-1:1),
UpperIdent(16:1-16:6),NoSpaceOpenRound(16:6-16:7),Newline(16:9-16:14),
LowerIdent(17:2-17:3),Comma(17:3-17:4),Newline(17:6-17:11),
LowerIdent(18:2-18:3),Comma(18:3-18:4),Newline(1:1-1:1),
CloseRound(19:1-19:2),Newline(19:4-19:7),
OpColon(20:2-20:3),Newline(20:5-20:9),
UpperIdent(21:3-21:7),NoSpaceOpenRound(21:7-21:8),Newline(21:10-21:12),
CloseRound(22:3-22:4),Comma(22:4-22:5),Newline(1:1-1:1),
OpenRound(23:3-23:4),LowerIdent(23:4-23:5),OpArrow(23:6-23:8),LowerIdent(23:9-23:10),CloseRound(23:10-23:11),OpArrow(23:12-23:14),Newline(23:16-23:20),
UpperIdent(24:4-24:8),NoSpaceOpenRound(24:8-24:9),LowerIdent(24:12-24:13),CloseRound(24:14-24:15),Newline(24:17-24:17),
Newline(1:1-1:1),
UpperIdent(26:1-26:4),OpColon(26:5-26:6),OpenRound(26:7-26:8),UpperIdent(26:8-26:11),Comma(26:11-26:12),UpperIdent(26:13-26:16),CloseRound(26:16-26:17),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(28:1-28:5),OpColon(28:6-28:7),OpenRound(28:8-28:9),Newline(28:11-28:16),
UpperIdent(29:2-29:5),Comma(29:5-29:6),Newline(29:8-29:8),
UpperIdent(30:2-30:5),Comma(30:5-30:6),Newline(30:8-30:9),
CloseRound(31:1-31:2),Newline(31:4-31:7),
UpperIdent(32:1-32:5),NoSpaceOpenRound(32:5-32:6),LowerIdent(32:6-32:7),CloseRound(32:7-32:8),OpColon(32:9-32:10),OpenCurly(32:11-32:12),LowerIdent(32:13-32:16),OpColon(32:17-32:18),UpperIdent(32:19-32:21),NoSpaceOpenRound(32:21-32:22),LowerIdent(32:22-32:23),CloseRound(32:23-32:24),Comma(32:24-32:25),LowerIdent(32:26-32:29),OpColon(32:30-32:31),LowerIdent(32:32-32:33),CloseCurly(32:34-32:35),Newline(1:1-1:1),
UpperIdent(33:1-33:3),NoSpaceOpenRound(33:3-33:4),LowerIdent(33:4-33:5),CloseRound(33:5-33:6),OpColon(33:7-33:8),OpenCurly(33:9-33:10),Newline(33:12-33:14),
LowerIdent(34:2-34:5),OpColon(34:6-34:7),UpperIdent(34:8-34:11),Newline(34:12-34:17),
CloseCurly(35:1-35:2),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(37:1-37:6),NoSpaceOpenRound(37:6-37:7),LowerIdent(37:7-37:8),CloseRound(37:8-37:9),OpColon(37:10-37:11),OpenCurly(37:12-37:13),Newline(37:15-37:16),
LowerIdent(38:2-38:5),OpColon(38:6-38:7),UpperIdent(38:8-38:11),Newline(1:1-1:1),
CloseCurly(39:1-39:2),Newline(39:4-39:4),
UpperIdent(40:1-40:5),OpColon(40:7-40:8),OpenSquare(40:9-40:10),Newline(40:12-40:12),
CloseSquare(41:1-41:2),Newline(41:4-41:6),
Newline(1:1-1:1),
UpperIdent(43:1-43:5),NoSpaceOpenRound(43:5-43:6),LowerIdent(43:6-43:7),CloseRound(43:7-43:8),OpColon(43:9-43:10),UpperIdent(43:11-43:16),NoSpaceOpenRound(43:16-43:17),LowerIdent(43:17-43:18),CloseRound(43:18-43:19),Comma(43:19-43:20),LowerIdent(43:21-43:22),OpArrow(43:23-43:25),UpperIdent(43:26-43:31),NoSpaceOpenRound(43:31-43:32),LowerIdent(43:32-43:33),CloseRound(43:33-43:34),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(45:1-45:4),OpAssign(45:5-45:6),OpBar(45:7-45:8),LowerIdent(45:8-45:11),OpBar(45:11-45:12),KwIf(45:13-45:15),LowerIdent(45:16-45:19),Int(45:20-45:21),KwElse(45:22-45:26),Int(45:27-45:28),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(47:1-47:8),OpColon(47:9-47:10),UpperIdent(47:11-47:14),OpArrow(47:15-47:17),UpperIdent(47:18-47:21),Newline(1:1-1:1),
LowerIdent(48:1-48:8),OpAssign(48:9-48:10),OpBar(48:11-48:12),LowerIdent(48:12-48:15),OpBar(48:15-48:16),OpenCurly(48:17-48:18),Newline(1:1-1:1),
LowerIdent(49:2-49:7),OpAssign(49:8-49:9),Int(49:10-49:11),Newline(1:1-1:1),
KwIf(50:2-50:4),LowerIdent(50:5-50:8),OpenCurly(50:9-50:10),Newline(1:1-1:1),
KwDbg(51:3-51:6),Newline(51:8-51:12),
OpenRound(52:1-52:2),CloseRound(52:2-52:3),Newline(52:5-52:6),
Int(53:3-53:4),Newline(1:1-1:1),
CloseCurly(54:2-54:3),KwElse(54:4-54:8),OpenCurly(54:9-54:10),Newline(1:1-1:1),
KwDbg(55:3-55:6),Int(55:7-55:10),Newline(1:1-1:1),
LowerIdent(56:3-56:8),Newline(1:1-1:1),
CloseCurly(57:2-57:3),Newline(1:1-1:1),
CloseCurly(58:1-58:2),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(60:1-60:11),OpAssign(60:12-60:13),OpBar(60:14-60:15),Newline(1:1-1:1),
LowerIdent(61:2-61:3),Comma(61:3-61:4),Newline(61:6-61:8),
LowerIdent(62:2-62:3),Comma(62:3-62:4),Newline(1:1-1:1),
OpBar(63:1-63:2),Newline(63:4-63:7),
KwMatch(64:2-64:7),LowerIdent(64:8-64:9),OpenCurly(64:10-64:11),LowerIdent(64:11-64:14),OpBar(64:15-64:16),UpperIdent(64:17-64:20),OpFatArrow(64:21-64:23),OpenCurly(64:24-64:25),Newline(1:1-1:1),
LowerIdent(65:4-65:5),LowerIdent(65:6-65:7),Newline(1:1-1:1),
CloseCurly(66:3-66:4),Newline(1:1-1:1),
UpperIdent(67:3-67:7),OpFatArrow(67:9-67:11),Int(67:12-67:13),Newline(1:1-1:1),
StringStart(68:3-68:4),StringPart(68:4-68:7),StringEnd(68:7-68:8),OpFatArrow(68:9-68:11),Newline(68:13-68:17),
Int(69:1-69:3),Newline(1:1-1:1),
StringStart(70:3-70:4),StringPart(70:4-70:7),StringEnd(70:7-70:8),OpBar(70:9-70:10),StringStart(70:11-70:12),StringPart(70:12-70:15),StringEnd(70:15-70:16),OpFatArrow(70:17-70:19),Int(70:20-70:22),OpenSquare(70:22-70:23),Int(70:23-70:24),Comma(70:24-70:25),Int(70:26-70:27),Comma(70:27-70:28),Int(70:29-70:30),Comma(70:30-70:31),DoubleDot(70:32-70:34),KwAs(70:35-70:37),LowerIdent(70:38-70:42),CloseSquare(70:42-70:43),Newline(70:45-70:51),
OpFatArrow(71:4-71:6),LowerIdent(71:7-71:11),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(1:1-1:1),
OpenSquare(74:3-74:4),Int(74:4-74:5),Comma(74:5-74:6),Int(74:7-74:8),OpBar(74:9-74:10),Int(74:11-74:12),Comma(74:12-74:13),Int(74:14-74:15),Comma(74:15-74:16),DoubleDot(74:17-74:19),KwAs(74:20-74:22),LowerIdent(74:23-74:27),CloseSquare(74:27-74:28),OpFatArrow(74:29-74:31),Int(74:32-74:35),Newline(1:1-1:1),
OpenSquare(75:3-75:4),Newline(1:1-1:1),
LowerIdent(76:1-76:4),Newline(1:1-1:1),
CloseSquare(77:3-77:4),OpFatArrow(77:5-77:7),Int(77:8-77:11),Newline(1:1-1:1),
Float(78:3-78:7),OpFatArrow(78:8-78:10),Int(78:11-78:14),Newline(1:1-1:1),
Float(79:3-79:7),OpBar(79:8-79:9),Float(79:10-79:14),OpFatArrow(79:15-79:17),Int(79:18-79:21),Newline(1:1-1:1),
OpenRound(80:3-80:4),Int(80:4-80:5),Comma(80:5-80:6),Int(80:7-80:8),Comma(80:8-80:9),Int(80:10-80:11),CloseRound(80:11-80:12),OpFatArrow(80:13-80:15),Int(80:16-80:19),Newline(1:1-1:1),
OpenRound(81:3-81:4),Int(81:4-81:5),Comma(81:5-81:6),Int(81:7-81:8),OpBar(81:9-81:10),Int(81:11-81:12),Comma(81:12-81:13),Int(81:14-81:15),CloseRound(81:15-81:16),OpFatArrow(81:17-81:19),Int(81:20-81:23),Newline(1:1-1:1),
OpenCurly(82:3-82:4),LowerIdent(82:5-82:8),OpColon(82:8-82:9),Int(82:10-82:11),Comma(82:11-82:12),LowerIdent(82:13-82:16),OpColon(82:16-82:17),Int(82:18-82:19),Comma(82:19-82:20),DoubleDot(82:21-82:23),LowerIdent(82:23-82:27),CloseCurly(82:28-82:29),OpFatArrow(82:30-82:32),Int(82:33-82:35),OpArrow(82:35-82:37),LowerIdent(82:37-82:40),NoSpaceOpenRound(82:40-82:41),Int(82:41-82:43),CloseRound(82:43-82:44),Newline(1:1-1:1),
OpenCurly(83:3-83:4),Newline(83:6-83:16),
LowerIdent(84:4-84:7),Newline(84:9-84:9),
OpColon(85:5-85:6),Newline(85:8-85:10),
Int(86:6-86:7),Comma(86:7-86:8),Newline(86:10-86:21),
LowerIdent(87:4-87:7),OpColon(87:7-87:8),Int(87:9-87:10),Comma(87:10-87:11),Newline(1:1-1:1),
DoubleDot(88:4-88:6),CloseCurly(88:6-88:7),OpFatArrow(88:8-88:10),Int(88:11-88:13),Newline(1:1-1:1),
OpenCurly(89:3-89:4),LowerIdent(89:5-89:8),OpColon(89:8-89:9),Int(89:10-89:11),Comma(89:11-89:12),LowerIdent(89:13-89:16),OpColon(89:16-89:17),Int(89:18-89:19),OpBar(89:20-89:21),Int(89:22-89:23),CloseCurly(89:24-89:25),OpFatArrow(89:26-89:28),Int(89:29-89:31),Newline(1:1-1:1),
OpenCurly(90:3-90:4),Newline(1:1-1:1),
LowerIdent(91:4-91:7),OpColon(91:7-91:8),Int(91:9-91:10),Comma(91:10-91:11),Newline(1:1-1:1),
CloseCurly(92:4-92:5),OpFatArrow(92:6-92:8),Int(92:9-92:11),Newline(1:1-1:1),
UpperIdent(93:3-93:5),NoSpaceOpenRound(93:5-93:6),Int(93:6-93:9),CloseRound(93:9-93:10),OpFatArrow(93:11-93:13),Int(93:14-93:20),Newline(1:1-1:1),
CloseCurly(94:2-94:3),Newline(1:1-1:1),
Newline(1:1-1:1),
KwExpect(96:1-96:7),Newline(96:9-96:22),
LowerIdent(97:2-97:6),OpEquals(97:7-97:9),Int(97:10-97:11),Newline(97:13-97:20),
Newline(1:1-1:1),
LowerIdent(99:1-99:6),OpColon(99:7-99:8),UpperIdent(99:9-99:13),NoSpaceOpenRound(99:13-99:14),UpperIdent(99:14-99:20),CloseRound(99:20-99:21),OpArrow(99:22-99:24),UpperIdent(99:25-99:31),NoSpaceOpenRound(99:31-99:32),OpenCurly(99:32-99:33),CloseCurly(99:33-99:34),Comma(99:34-99:35),Underscore(99:36-99:37),CloseRound(99:37-99:38),Newline(1:1-1:1),
LowerIdent(100:1-100:6),OpAssign(100:7-100:8),OpBar(100:9-100:10),Underscore(100:10-100:11),OpBar(100:11-100:12),OpenCurly(100:13-100:14),Newline(100:16-100:24),
LowerIdent(101:2-101:7),OpAssign(101:8-101:9),StringStart(101:10-101:11),StringPart(101:11-101:16),StringEnd(101:16-101:17),Newline(1:1-1:1),
KwVar(102:2-102:5),LowerIdent(102:6-102:12),OpAssign(102:13-102:14),Int(102:15-102:18),Newline(1:1-1:1),
KwExpect(103:2-103:8),LowerIdent(103:9-103:13),OpEquals(103:14-103:16),Int(103:17-103:18),Newline(1:1-1:1),
LowerIdent(104:2-104:5),OpAssign(104:6-104:7),UpperIdent(104:8-104:12),Newline(1:1-1:1),
KwReturn(105:2-105:8),Newline(105:10-105:15),
LowerIdent(106:3-106:6),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(108:3-108:10),
Newline(1:1-1:1),
TripleDot(110:2-110:5),Newline(1:1-1:1),
LowerIdent(111:2-111:12),NoSpaceOpenRound(111:12-111:13),Newline(1:1-1:1),
TripleDot(112:3-112:6),Comma(112:6-112:7),Newline(112:9-112:9),
CloseRound(113:2-113:3),Newline(1:1-1:1),
LowerIdent(114:2-114:11),NoSpaceOpenRound(114:11-114:12),Newline(1:1-1:1),
KwDbg(115:3-115:6),Newline(115:8-115:12),
Int(116:4-116:6),Comma(116:6-116:7),Newline(116:9-116:18),
CloseRound(117:2-117:3),Newline(1:1-1:1),
KwCrash(118:2-118:7),StringStart(118:8-118:9),StringPart(118:9-118:22),StringEnd(118:22-118:22),Newline(1:1-1:1),
LowerIdent(119:2-119:10),OpAssign(119:11-119:12),UpperIdent(119:13-119:15),NoSpaceOpenRound(119:15-119:16),LowerIdent(119:16-119:22),CloseRound(119:22-119:23),Newline(1:1-1:1),
LowerIdent(120:2-120:6),OpAssign(120:7-120:8),StringStart(120:9-120:10),StringPart(120:10-120:17),OpenStringInterpolation(120:17-120:19),LowerIdent(120:19-120:24),CloseStringInterpolation(120:24-120:25),StringPart(120:25-120:25),StringEnd(120:25-120:26),Newline(1:1-1:1),
LowerIdent(121:2-121:6),OpAssign(121:7-121:8),OpenSquare(121:9-121:10),Newline(1:1-1:1),
LowerIdent(122:3-122:10),NoSpaceOpenRound(122:10-122:11),Newline(1:1-1:1),
KwDbg(123:4-123:7),Newline(123:9-123:19),
LowerIdent(124:1-124:2),OpenSquare(124:2-124:3),Comma(124:3-124:4),Newline(124:6-124:12),
CloseSquare(125:3-125:4),Comma(125:4-125:5),Int(125:6-125:9),Comma(125:9-125:10),Newline(125:12-125:15),
CloseRound(126:2-126:3),Newline(1:1-1:1),
KwFor(127:2-127:5),LowerIdent(127:6-127:7),KwIn(127:8-127:10),LowerIdent(127:11-127:15),OpenCurly(127:16-127:17),Newline(1:1-1:1),
LowerIdent(128:2-128:7),NoSpaceOpenRound(128:7-128:8),StringStart(128:8-128:9),StringPart(128:9-128:16),OpenStringInterpolation(128:16-128:18),LowerIdent(128:18-128:19),CloseStringInterpolation(128:19-128:20),StringPart(128:20-128:24),OpenStringInterpolation(128:24-128:26),LowerIdent(128:26-128:32),CloseStringInterpolation(128:32-128:33),StringPart(128:33-128:33),StringEnd(128:33-128:34),CloseRound(128:34-128:35),Newline(1:1-1:1),
LowerIdent(129:3-129:9),OpAssign(129:10-129:11),LowerIdent(129:12-129:18),OpPlus(129:19-129:20),LowerIdent(129:21-129:22),Newline(1:1-1:1),
CloseCurly(130:2-130:3),Newline(1:1-1:1),
LowerIdent(131:2-131:8),OpAssign(131:9-131:10),OpenCurly(131:11-131:12),LowerIdent(131:13-131:16),OpColon(131:16-131:17),Int(131:18-131:21),Comma(131:21-131:22),LowerIdent(131:23-131:26),OpColon(131:26-131:27),StringStart(131:28-131:29),StringPart(131:29-131:34),StringEnd(131:34-131:35),Comma(131:35-131:36),LowerIdent(131:37-131:40),OpColon(131:40-131:41),LowerIdent(131:42-131:45),Comma(131:45-131:46),LowerIdent(131:47-131:50),OpColon(131:50-131:51),UpperIdent(131:52-131:54),NoSpaceOpenRound(131:54-131:55),LowerIdent(131:55-131:60),CloseRound(131:60-131:61),Comma(131:61-131:62),LowerIdent(131:63-131:69),CloseCurly(131:70-131:71),Newline(1:1-1:1),
LowerIdent(132:2-132:7),OpAssign(132:8-132:9),OpenRound(132:10-132:11),Int(132:11-132:14),Comma(132:14-132:15),StringStart(132:16-132:17),StringPart(132:17-132:22),StringEnd(132:22-132:23),Comma(132:23-132:24),LowerIdent(132:25-132:28),Comma(132:28-132:29),UpperIdent(132:30-132:32),NoSpaceOpenRound(132:32-132:33),LowerIdent(132:33-132:38),CloseRound(132:38-132:39),Comma(132:39-132:40),OpenRound(132:41-132:42),LowerIdent(132:42-132:48),Comma(132:48-132:49),LowerIdent(132:50-132:55),CloseRound(132:55-132:56),Comma(132:56-132:57),OpenSquare(132:58-132:59),Int(132:59-132:60),Comma(132:60-132:61),Int(132:62-132:63),Comma(132:63-132:64),Int(132:65-132:66),CloseSquare(132:66-132:67),CloseRound(132:67-132:68),Newline(1:1-1:1),
LowerIdent(133:2-133:9),OpAssign(133:10-133:11),OpenRound(133:12-133:13),Newline(1:1-1:1),
Int(134:3-134:6),Comma(134:6-134:7),Newline(1:1-1:1),
StringStart(135:3-135:4),StringPart(135:4-135:9),StringEnd(135:9-135:10),Comma(135:10-135:11),Newline(1:1-1:1),
LowerIdent(136:3-136:7),Comma(136:7-136:8),Newline(1:1-1:1),
UpperIdent(137:3-137:5),NoSpaceOpenRound(137:5-137:6),LowerIdent(137:6-137:11),CloseRound(137:11-137:12),Comma(137:12-137:13),Newline(137:15-137:22),
OpenRound(138:3-138:4),LowerIdent(138:4-138:10),Comma(138:10-138:11),LowerIdent(138:12-138:17),CloseRound(138:17-138:18),Comma(138:18-138:19),Newline(1:1-1:1),
OpenSquare(139:3-139:4),Int(139:4-139:5),Comma(139:5-139:6),Int(139:7-139:8),Comma(139:8-139:9),Int(139:10-139:11),CloseSquare(139:11-139:12),Comma(139:12-139:13),Newline(1:1-1:1),
CloseRound(140:2-140:3),Newline(1:1-1:1),
LowerIdent(141:2-141:7),OpAssign(141:8-141:9),UpperIdent(141:10-141:13),NoSpaceOpenRound(141:13-141:14),LowerIdent(141:14-141:17),CloseRound(141:17-141:18),OpDoubleQuestion(141:19-141:21),Int(141:22-141:24),OpGreaterThan(141:25-141:26),Int(141:27-141:28),OpStar(141:29-141:30),Int(141:31-141:32),OpOr(141:33-141:35),Int(141:36-141:38),OpPlus(141:39-141:40),Int(141:41-141:42),OpLessThan(141:43-141:44),Int(141:45-141:46),OpAnd(141:47-141:50),Int(141:51-141:53),OpBinaryMinus(141:54-141:55),Int(141:56-141:57),OpGreaterThanOrEq(141:58-141:60),Int(141:61-141:63),OpOr(141:64-141:66),Int(141:67-141:69),OpLessThanOrEq(141:70-141:72),Int(141:73-141:74),OpSlash(141:75-141:76),Int(141:77-141:78),Newline(1:1-1:1),
LowerIdent(142:2-142:7),OpAssign(142:8-142:9),LowerIdent(142:10-142:17),NoSpaceOpenRound(142:17-142:18),LowerIdent(142:18-142:22),CloseRound(142:22-142:23),NoSpaceOpQuestion(142:23-142:24),NoSpaceDotLowerIdent(142:24-142:31),NoSpaceOpenRound(142:31-142:32),CloseRound(142:32-142:33),NoSpaceOpQuestion(142:33-142:34),NoSpaceDotLowerIdent(142:34-142:38),NoSpaceOpenRound(142:38-142:39),CloseRound(142:39-142:40),NoSpaceOpQuestion(142:40-142:41),NoSpaceDotLowerIdent(142:41-142:46),NoSpaceOpQuestion(142:46-142:47),Newline(1:1-1:1),
UpperIdent(143:2-143:11),NoSpaceOpenRound(143:11-143:12),Newline(1:1-1:1),
StringStart(144:3-144:4),StringPart(144:4-144:14),OpenStringInterpolation(144:14-144:16),Newline(144:18-144:18),
UpperIdent(145:4-145:7),NoSpaceDotLowerIdent(145:7-145:13),NoSpaceOpenRound(145:13-145:14),LowerIdent(145:14-145:20),CloseRound(145:20-145:21),Newline(145:23-145:31),
CloseStringInterpolation(146:3-146:4),StringPart(146:4-146:9),StringEnd(146:9-146:10),Comma(146:10-146:11),Newline(1:1-1:1),
CloseRound(147:2-147:3),Newline(1:1-1:1),
CloseSquare(148:1-148:2),Newline(148:4-148:17),
Newline(1:1-1:1),
LowerIdent(150:1-150:6),OpColon(150:7-150:8),OpenCurly(150:9-150:10),CloseCurly(150:10-150:11),Newline(1:1-1:1),
LowerIdent(151:1-151:6),OpAssign(151:7-151:8),OpenCurly(151:9-151:10),CloseCurly(151:10-151:11),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(153:1-153:6),OpColon(153:7-153:8),UpperIdent(153:9-153:14),NoSpaceOpenRound(153:14-153:15),NoSpaceOpenRound(153:15-153:16),LowerIdent(153:16-153:17),Comma(153:17-153:18),LowerIdent(153:19-153:20),Comma(153:20-153:21),LowerIdent(153:22-153:23),CloseRound(153:23-153:24),CloseRound(153:24-153:25),Newline(1:1-1:1),
Newline(1:1-1:1),
KwExpect(155:1-155:7),OpenCurly(155:8-155:9),Newline(1:1-1:1),
LowerIdent(156:2-156:5),OpAssign(156:6-156:7),Int(156:8-156:9),Newline(156:11-156:16),
LowerIdent(157:2-157:6),OpAssign(157:7-157:8),Int(157:9-157:10),Newline(1:1-1:1),
LowerIdent(158:2-158:6),OpEquals(158:7-158:9),LowerIdent(158:10-158:13),Newline(1:1-1:1),
CloseCurly(159:1-159:2),EndOfFile(159:2-159:2),
~~~
# PARSE
~~~clojure
(file @1.2-159.2
	(app @2.1-2.33
		(provides @2.6-2.12
			(exposed-lower-ident (text "main!")))
		(record-field @2.15-2.33 (name "pf")
			(e-string @2.28-2.31
				(e-string-part @2.29-2.30 (raw "c"))))
		(packages @2.13-2.33
			(record-field @2.15-2.33 (name "pf")
				(e-string @2.28-2.31
					(e-string-part @2.29-2.30 (raw "c"))))))
	(statements
		(s-import @4.1-4.38 (raw "pf.Stdout")
			(exposing
				(exposed-lower-ident (text "line!"))
				(exposed-lower-ident (text "e!"))))
		(s-import @6.1-8.4 (raw "Stdot"))
		(s-import @10.1-10.46 (raw "pkg.S")
			(exposing
				(exposed-lower-ident (text "func") (as "fry"))
				(exposed-upper-ident-star (text "Custom"))))
		(s-import @12.1-12.19 (raw "Bae") (alias "Gooe"))
		(s-import @13.1-14.4 (raw "Ba"))
		(s-type-decl @15.1-15.41
			(header @15.1-15.10 (name "Map")
				(args
					(ty-var @15.5-15.6 (raw "a"))
					(ty-var @15.8-15.9 (raw "b"))))
			(ty-fn @15.13-15.41
				(ty-apply @15.13-15.20
					(ty @15.13-15.17 (name "List"))
					(ty-var @15.18-15.19 (raw "a")))
				(ty-fn @15.23-15.29
					(ty-var @15.23-15.24 (raw "a"))
					(ty-var @15.28-15.29 (raw "b")))
				(ty-apply @15.34-15.41
					(ty @15.34-15.38 (name "List"))
					(ty-var @15.39-15.40 (raw "b")))))
		(s-type-decl @16.1-24.15
			(header @16.1-19.2 (name "MapML")
				(args
					(ty-var @17.2-17.3 (raw "a"))
					(ty-var @18.2-18.3 (raw "b"))))
			(ty-fn @21.3-24.15
				(ty-apply @21.3-22.4
					(ty @21.3-21.7 (name "List")))
				(ty-fn @23.4-23.10
					(ty-var @23.4-23.5 (raw "a"))
					(ty-var @23.9-23.10 (raw "b")))
				(ty-apply @24.4-24.15
					(ty @24.4-24.8 (name "List"))
					(ty-var @24.12-24.13 (raw "b")))))
		(s-type-decl @26.1-26.17
			(header @26.1-26.4 (name "Foo")
				(args))
			(ty-tuple @26.7-26.17
				(ty @26.8-26.11 (name "Bar"))
				(ty @26.13-26.16 (name "Baz"))))
		(s-type-anno @28.1-32.5 (name "line")
			(ty-tuple @28.8-31.2
				(ty @29.2-29.5 (name "Bar"))
				(ty @30.2-30.5 (name "Baz"))))
		(s-type-decl @32.1-32.35
			(header @32.1-32.8 (name "Some")
				(args
					(ty-var @32.6-32.7 (raw "a"))))
			(ty-record @32.11-32.35
				(anno-record-field @32.13-32.25 (name "foo")
					(ty-apply @32.19-32.24
						(ty @32.19-32.21 (name "Ok"))
						(ty-var @32.22-32.23 (raw "a"))))
				(anno-record-field @32.26-32.35 (name "bar")
					(ty-var @32.32-32.33 (raw "g")))))
		(s-type-decl @33.1-35.2
			(header @33.1-33.6 (name "Ml")
				(args
					(ty-var @33.4-33.5 (raw "a"))))
			(ty-malformed @34.12-35.2 (tag "expected_ty_close_curly_or_comma")))
		(e-malformed @1.1-1.1 (reason "expr_unexpected_token"))
		(s-type-decl @37.1-39.2
			(header @37.1-37.9 (name "Soine")
				(args
					(ty-var @37.7-37.8 (raw "a"))))
			(ty-malformed @1.1-39.2 (tag "expected_ty_close_curly_or_comma")))
		(e-malformed @39.1-39.4 (reason "expr_unexpected_token"))
		(s-type-decl @40.1-41.2
			(header @40.1-40.5 (name "Maya")
				(args))
			(ty-tag-union @40.9-41.2
				(tags)))
		(s-type-decl @43.1-43.34
			(header @43.1-43.8 (name "Func")
				(args
					(ty-var @43.6-43.7 (raw "a"))))
			(ty-fn @43.11-43.34
				(ty-apply @43.11-43.19
					(ty @43.11-43.16 (name "Maybe"))
					(ty-var @43.17-43.18 (raw "a")))
				(ty-var @43.21-43.22 (raw "a"))
				(ty-apply @43.26-43.34
					(ty @43.26-43.31 (name "Maybe"))
					(ty-var @43.32-43.33 (raw "a")))))
		(s-decl @45.1-47.8
			(p-ident @45.1-45.4 (raw "ane"))
			(e-lambda @45.7-47.8
				(args
					(p-ident @45.8-45.11 (raw "num")))
				(e-if-then-else @45.13-47.8
					(e-ident @45.16-45.19 (raw "num"))
					(e-int @45.20-45.21 (raw "2"))
					(e-int @45.27-45.28 (raw "5")))))
		(s-type-anno @1.1-1.1 (name "add_one")
			(ty-fn @47.11-47.21
				(ty @47.11-47.14 (name "U64"))
				(ty @47.18-47.21 (name "U64"))))
		(s-decl @48.1-58.2
			(p-ident @48.1-48.8 (raw "add_one"))
			(e-lambda @48.11-58.2
				(args
					(p-ident @48.12-48.15 (raw "num")))
				(e-block @48.17-58.2
					(statements
						(s-decl @49.2-49.11
							(p-ident @49.2-49.7 (raw "other"))
							(e-int @49.10-49.11 (raw "1")))
						(e-if-then-else @50.2-58.2
							(e-ident @50.5-50.8 (raw "num"))
							(e-block @50.9-54.3
								(statements
									(s-dbg @51.3-53.4
										(e-tuple @52.1-52.3))
									(e-int @53.3-53.4 (raw "0"))))
							(e-block @54.9-57.3
								(statements
									(s-dbg @55.3-56.8
										(e-int @55.7-55.10 (raw "123")))
									(e-ident @56.3-56.8 (raw "other")))))))))
		(s-decl @60.1-94.3
			(p-ident @60.1-60.11 (raw "match_time"))
			(e-lambda @60.14-94.3
				(args
					(p-ident @61.2-61.3 (raw "a"))
					(p-ident @62.2-62.3 (raw "b")))
				(e-match
					(e-ident @64.8-64.9 (raw "a"))
					(branches
						(branch @64.11-67.7
							(p-alternatives
								(p-ident @64.11-64.14 (raw "lue"))
								(p-tag @64.17-64.20 (raw "Red")))
							(e-block @64.24-66.4
								(statements
									(e-ident @65.4-65.5 (raw "x"))
									(e-ident @65.6-65.7 (raw "x")))))
						(branch @67.3-68.4
							(p-tag @67.3-67.7 (raw "Blue"))
							(e-int @67.12-67.13 (raw "1")))
						(branch @68.3-70.4
							(p-string @68.3-68.4 (raw """))
							(e-int @69.1-69.3 (raw "00")))
						(branch @70.3-70.23
							(p-alternatives
								(p-string @70.3-70.4 (raw """))
								(p-string @70.11-70.12 (raw """)))
							(e-int @70.20-70.22 (raw "20")))
						(branch @70.22-74.4
							(p-list @70.22-70.43
								(p-int @70.23-70.24 (raw "1"))
								(p-int @70.26-70.27 (raw "2"))
								(p-int @70.29-70.30 (raw "3"))
								(p-list-rest @70.32-70.43 (name "rest")))
							(e-ident @71.7-71.11 (raw "ment")))
						(branch @74.3-75.4
							(p-list @74.3-74.28
								(p-int @74.4-74.5 (raw "1"))
								(p-alternatives
									(p-int @74.7-74.8 (raw "2"))
									(p-int @74.11-74.12 (raw "5")))
								(p-int @74.14-74.15 (raw "3"))
								(p-list-rest @74.17-74.28 (name "rest")))
							(e-int @74.32-74.35 (raw "123")))
						(branch @75.3-78.7
							(p-list @75.3-77.4
								(p-ident @76.1-76.4 (raw "ist")))
							(e-int @77.8-77.11 (raw "123")))
						(branch @78.3-79.7
							(p-frac @78.3-78.7 (raw "3.14"))
							(e-int @78.11-78.14 (raw "314")))
						(branch @79.3-80.4
							(p-alternatives
								(p-frac @79.3-79.7 (raw "3.14"))
								(p-frac @79.10-79.14 (raw "6.28")))
							(e-int @79.18-79.21 (raw "314")))
						(branch @80.3-81.4
							(p-tuple @80.3-80.12
								(p-int @80.4-80.5 (raw "1"))
								(p-int @80.7-80.8 (raw "2"))
								(p-int @80.10-80.11 (raw "3")))
							(e-int @80.16-80.19 (raw "123")))
						(branch @81.3-82.4
							(p-tuple @81.3-81.16
								(p-int @81.4-81.5 (raw "1"))
								(p-alternatives
									(p-int @81.7-81.8 (raw "2"))
									(p-int @81.11-81.12 (raw "5")))
								(p-int @81.14-81.15 (raw "3")))
							(e-int @81.20-81.23 (raw "123")))
						(branch @82.3-83.4
							(p-record @82.3-82.29
								(field @82.5-82.12 (name "foo") (rest false)
									(p-int @82.10-82.11 (raw "1")))
								(field @82.13-82.20 (name "bar") (rest false)
									(p-int @82.18-82.19 (raw "2")))
								(field @82.21-82.29 (name "rest") (rest true)))
							(e-local-dispatch @82.33-83.4
								(e-int @82.33-82.35 (raw "12"))
								(e-apply @82.35-82.44
									(e-ident @82.37-82.40 (raw "add"))
									(e-int @82.41-82.43 (raw "34")))))
						(branch @83.3-89.4
							(p-record @83.3-88.7
								(field @84.4-86.8 (name "foo") (rest false)
									(p-int @86.6-86.7 (raw "1")))
								(field @87.4-87.11 (name "bar") (rest false)
									(p-int @87.9-87.10 (raw "2")))
								(field @88.4-88.7 (name " Thnt!") (rest true)))
							(e-int @88.11-88.13 (raw "12")))
						(branch @89.3-90.4
							(p-record @89.3-89.25
								(field @89.5-89.12 (name "foo") (rest false)
									(p-int @89.10-89.11 (raw "1")))
								(field @89.13-89.25 (name "bar") (rest false)
									(p-alternatives
										(p-int @89.18-89.19 (raw "2"))
										(p-int @89.22-89.23 (raw "7")))))
							(e-int @89.29-89.31 (raw "12")))
						(branch @90.3-93.5
							(p-record @90.3-92.5
								(field @91.4-91.11 (name "foo") (rest false)
									(p-int @91.9-91.10 (raw "1"))))
							(e-int @92.9-92.11 (raw "12")))
						(branch @93.3-94.3
							(p-tag @93.3-93.10 (raw "Ok")
								(p-int @93.6-93.9 (raw "123")))
							(e-int @93.14-93.20 (raw "121000")))))))
		(s-expect @96.1-99.6
			(e-binop @97.2-99.6 (op "==")
				(e-ident @97.2-97.6 (raw "blah"))
				(e-int @97.10-97.11 (raw "1"))))
		(s-type-anno @99.1-100.6 (name "main!")
			(ty-fn @99.9-99.38
				(ty-apply @99.9-99.21
					(ty @99.9-99.13 (name "List"))
					(ty @99.14-99.20 (name "String")))
				(ty-apply @99.25-99.38
					(ty @99.25-99.31 (name "Result"))
					(ty-record @99.32-99.34)
					(_))))
		(s-decl @100.1-159.2
			(p-ident @100.1-100.6 (raw "main!"))
			(e-lambda @100.9-159.2
				(args
					(p-underscore))
				(e-block @100.13-159.2
					(statements
						(s-decl @101.2-101.17
							(p-ident @101.2-101.7 (raw "world"))
							(e-string @101.10-101.17
								(e-string-part @101.11-101.16 (raw "World"))))
						(s-var @102.2-103.8 (name "number")
							(e-int @102.15-102.18 (raw "123")))
						(s-expect @103.2-104.5
							(e-binop @103.9-104.5 (op "==")
								(e-ident @103.9-103.13 (raw "blah"))
								(e-int @103.17-103.18 (raw "1"))))
						(s-decl @104.2-104.12
							(p-ident @104.2-104.5 (raw "tag"))
							(e-tag @104.8-104.12 (raw "Blue")))
						(s-return @105.2-110.5
							(e-ident @106.3-106.6 (raw "tag")))
						(e-ellipsis)
						(e-apply @111.2-113.3
							(e-ident @111.2-111.12 (raw "match_time"))
							(e-ellipsis))
						(e-apply @114.2-117.3
							(e-ident @114.2-114.11 (raw "some_func"))
							(e-dbg
								(e-int @116.4-116.6 (raw "42"))))
						(s-crash @1.1-1.1
							(e-string @118.8-118.22
								(e-string-part @118.9-118.22 (raw "Unreachtement"))))
						(s-decl @119.2-119.23
							(p-ident @119.2-119.10 (raw "tag_with"))
							(e-apply @119.13-119.23
								(e-tag @119.13-119.15 (raw "Ok"))
								(e-ident @119.16-119.22 (raw "number"))))
						(s-decl @120.2-120.26
							(p-ident @120.2-120.6 (raw "ited"))
							(e-string @120.9-120.26
								(e-string-part @120.10-120.17 (raw "Hello, "))
								(e-ident @120.19-120.24 (raw "world"))
								(e-string-part @120.25-120.25 (raw ""))))
						(s-decl @121.2-125.4
							(p-ident @121.2-121.6 (raw "list"))
							(e-list @121.9-125.4
								(e-malformed @122.3-124.4 (reason "expected_expr_apply_close_round"))))
						(e-malformed @125.4-125.9 (reason "expr_unexpected_token"))
						(e-int @125.6-125.9 (raw "456"))
						(e-malformed @125.9-125.15 (reason "expr_unexpected_token"))
						(e-malformed @1.1-1.1 (reason "expr_unexpected_token"))
						(s-for
							(p-ident @127.6-127.7 (raw "n"))
							(e-ident @127.11-127.15 (raw "list"))
							(e-block @127.16-130.3
								(statements
									(e-apply @128.2-128.35
										(e-ident @128.2-128.7 (raw "line!"))
										(e-string @128.8-128.34
											(e-string-part @128.9-128.16 (raw "Adding "))
											(e-ident @128.18-128.19 (raw "n"))
											(e-string-part @128.20-128.24 (raw " to "))
											(e-ident @128.26-128.32 (raw "number"))
											(e-string-part @128.33-128.33 (raw ""))))
									(s-decl @129.3-130.3
										(p-ident @129.3-129.9 (raw "number"))
										(e-binop @129.12-130.3 (op "+")
											(e-ident @129.12-129.18 (raw "number"))
											(e-ident @129.21-129.22 (raw "n")))))))
						(s-decl @131.2-131.71
							(p-ident @131.2-131.8 (raw "record"))
							(e-record @131.11-131.71
								(field (field "foo") (optional false)
									(e-int @131.18-131.21 (raw "123")))
								(field (field "bar") (optional false)
									(e-string @131.28-131.35
										(e-string-part @131.29-131.34 (raw "Hello"))))
								(field (field "baz") (optional false)
									(e-ident @131.42-131.45 (raw "tag")))
								(field (field "qux") (optional false)
									(e-apply @131.52-131.61
										(e-tag @131.52-131.54 (raw "Ok"))
										(e-ident @131.55-131.60 (raw "world"))))
								(field (field "punned") (optional false))))
						(s-decl @132.2-132.68
							(p-ident @132.2-132.7 (raw "tuple"))
							(e-tuple @132.10-132.68
								(e-int @132.11-132.14 (raw "123"))
								(e-string @132.16-132.23
									(e-string-part @132.17-132.22 (raw "World")))
								(e-ident @132.25-132.28 (raw "tag"))
								(e-apply @132.30-132.39
									(e-tag @132.30-132.32 (raw "Ok"))
									(e-ident @132.33-132.38 (raw "world")))
								(e-tuple @132.41-132.56
									(e-ident @132.42-132.48 (raw "nested"))
									(e-ident @132.50-132.55 (raw "tuple")))
								(e-list @132.58-132.67
									(e-int @132.59-132.60 (raw "1"))
									(e-int @132.62-132.63 (raw "2"))
									(e-int @132.65-132.66 (raw "3")))))
						(s-decl @133.2-140.3
							(p-ident @133.2-133.9 (raw "m_tuple"))
							(e-tuple @133.12-140.3
								(e-int @134.3-134.6 (raw "123"))
								(e-string @135.3-135.10
									(e-string-part @135.4-135.9 (raw "World")))
								(e-ident @136.3-136.7 (raw "tag1"))
								(e-apply @137.3-137.12
									(e-tag @137.3-137.5 (raw "Ok"))
									(e-ident @137.6-137.11 (raw "world")))
								(e-tuple @138.3-138.18
									(e-ident @138.4-138.10 (raw "nested"))
									(e-ident @138.12-138.17 (raw "tuple")))
								(e-list @139.3-139.12
									(e-int @139.4-139.5 (raw "1"))
									(e-int @139.7-139.8 (raw "2"))
									(e-int @139.10-139.11 (raw "3")))))
						(s-decl @141.2-142.7
							(p-ident @141.2-141.7 (raw "bsult"))
							(e-binop @141.10-142.7 (op "or")
								(e-binop @141.10-141.66 (op "or")
									(e-binop @141.10-141.35 (op ">")
										(e-binop @141.10-141.26 (op "??")
											(e-apply @141.10-141.18
												(e-tag @141.10-141.13 (raw "Err"))
												(e-ident @141.14-141.17 (raw "foo")))
											(e-int @141.22-141.24 (raw "12")))
										(e-binop @141.27-141.35 (op "*")
											(e-int @141.27-141.28 (raw "5"))
											(e-int @141.31-141.32 (raw "5"))))
									(e-binop @141.36-141.66 (op "and")
										(e-binop @141.36-141.50 (op "<")
											(e-binop @141.36-141.44 (op "+")
												(e-int @141.36-141.38 (raw "13"))
												(e-int @141.41-141.42 (raw "2")))
											(e-int @141.45-141.46 (raw "5")))
										(e-binop @141.51-141.66 (op ">=")
											(e-binop @141.51-141.60 (op "-")
												(e-int @141.51-141.53 (raw "10"))
												(e-int @141.56-141.57 (raw "1")))
											(e-int @141.61-141.63 (raw "16")))))
								(e-binop @141.67-142.7 (op "<=")
									(e-int @141.67-141.69 (raw "12"))
									(e-binop @141.73-142.7 (op "/")
										(e-int @141.73-141.74 (raw "3"))
										(e-int @141.77-141.78 (raw "5"))))))
						(s-decl @142.2-143.11
							(p-ident @142.2-142.7 (raw "stale"))
							(e-field-access @142.10-143.11
								(e-field-access @142.10-142.46
									(e-field-access @142.10-142.38
										(e-question-suffix @142.10-142.24
											(e-apply @142.10-142.23
												(e-ident @142.10-142.17 (raw "some_fn"))
												(e-ident @142.18-142.22 (raw "arg1"))))
										(e-question-suffix @142.24-142.34
											(e-apply @142.24-142.33
												(e-ident @142.24-142.31 (raw "statod")))))
									(e-question-suffix @142.34-142.41
										(e-apply @142.34-142.40
											(e-ident @142.34-142.38 (raw "ned")))))
								(e-question-suffix @142.41-142.47
									(e-ident @142.41-142.46 (raw "recd")))))
						(e-apply @143.2-147.3
							(e-tag @143.2-143.11 (raw "Stdoline!"))
							(e-string @144.3-146.10
								(e-string-part @144.4-144.14 (raw "How about "))
								(e-apply @145.4-145.21
									(e-ident @145.4-145.13 (raw "Num.toStr"))
									(e-ident @145.14-145.20 (raw "number")))
								(e-string-part @146.4-146.9 (raw " as a"))))
						(e-malformed @148.1-148.17 (reason "expr_unexpected_token"))
						(s-type-anno @150.1-151.6 (name "empty")
							(ty-record @150.9-150.11))
						(s-decl @151.1-151.11
							(p-ident @151.1-151.6 (raw "empty"))
							(e-record @151.9-151.11))
						(s-type-anno @153.1-155.7 (name "tuple")
							(ty-apply @153.9-153.25
								(ty @153.9-153.14 (name "Value"))
								(ty-tuple @153.15-153.24
									(ty-var @153.16-153.17 (raw "a"))
									(ty-var @153.19-153.20 (raw "b"))
									(ty-var @153.22-153.23 (raw "c")))))
						(s-expect @155.1-159.2
							(e-block @155.8-159.2
								(statements
									(s-decl @156.2-156.9
										(p-ident @156.2-156.5 (raw "foo"))
										(e-int @156.8-156.9 (raw "1")))
									(s-decl @157.2-157.10
										(p-ident @157.2-157.6 (raw "blah"))
										(e-int @157.9-157.10 (raw "1")))
									(e-binop @158.2-159.2 (op "==")
										(e-ident @158.2-158.6 (raw "blah"))
										(e-ident @158.10-158.13 (raw "foo"))))))))))))
~~~
# FORMATTED
~~~roc
# Thnt!
app [main!] { pf: platform "c" }

import pf.Stdout exposing [line!, e!]

import Stdot
	 # Cose

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
			List(b)

Foo : (Bar, Baz)

line : ( # Cpen
	Bar,
	Baz, # m
) # Co
Some(a) : { foo : Ok(a), bar : g }
Ml(a) :  # Afld


Soine(a) : 

Maya : [] # se

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
	match a {		lue | Red => {
			x
			x
		}
		Blue => 1
		"foo" => # ent
			00
		"foo" | "bar" => 20		[1, 2, 3, .. as rest] # Aftet
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
			foo
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
		...,
	)
	some_func(
		dbg # bug
			42, # Aft expr
	)
	crash "Unreachtement"
	tag_with = Ok(number)
	ited = "Hello, ${world}"
	list = [
		, # afarg
	]
	
	456
	 # ee
	
	for n in list {
		line!("Adding ${n} to ${number}")
		number = number + n
	}
	record = {foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned}
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
		"How about ${
			Num.toStr(number) # on expr
		} as a",
	)
	 # Commenl decl

	empty : {}
	empty = {}

	tuple : Value((a, b, c))

	expect {
		foo = 1 # Thio
		blah = 1
		blah == foo
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @45.1-45.4 (ident "ane"))
		(e-lambda @45.7-47.8
			(args
				(p-assign @45.8-45.11 (ident "num")))
			(e-if @45.13-47.8
				(if-branches
					(if-branch
						(e-lookup-local @45.16-45.19
							(pattern @45.8-45.11))
						(e-int @45.20-45.21 (value "2"))))
				(if-else
					(e-int @45.27-45.28 (value "5"))))))
	(d-let
		(p-assign @48.1-48.8 (ident "add_one"))
		(e-lambda @48.11-58.2
			(args
				(p-assign @48.12-48.15 (ident "num")))
			(e-block @48.17-58.2
				(s-let @49.2-49.11
					(p-assign @49.2-49.7 (ident "other"))
					(e-int @49.10-49.11 (value "1")))
				(e-if @50.2-58.2
					(if-branches
						(if-branch
							(e-lookup-local @50.5-50.8
								(pattern @48.12-48.15))
							(e-block @50.9-54.3
								(s-dbg @51.3-53.4
									(e-runtime-error (tag "empty_tuple")))
								(e-int @53.3-53.4 (value "0")))))
					(if-else
						(e-block @54.9-57.3
							(s-dbg @55.3-56.8
								(e-int @55.7-55.10 (value "123")))
							(e-lookup-local @56.3-56.8
								(pattern @49.2-49.7)))))))
		(annotation @48.1-48.8
			(declared-type
				(ty-fn @47.11-47.21 (effectful false)
					(ty @47.11-47.14 (name "U64"))
					(ty @47.18-47.21 (name "U64"))))))
	(d-let
		(p-assign @60.1-60.11 (ident "match_time"))
		(e-lambda @60.14-94.3
			(args
				(p-assign @61.2-61.3 (ident "a"))
				(p-assign @62.2-62.3 (ident "b")))
			(e-match @64.2-94.3
				(match @64.2-94.3
					(cond
						(e-lookup-local @64.8-64.9
							(pattern @61.2-61.3)))
					(branches
						(branch
							(patterns
								(p-assign @64.11-64.14 (ident "lue") (degenerate false))
								(p-applied-tag @64.17-64.20 (degenerate false)))
							(value
								(e-block @64.24-66.4
									(s-expr @65.4-65.7
										(e-runtime-error (tag "ident_not_in_scope")))
									(e-runtime-error (tag "ident_not_in_scope")))))
						(branch
							(patterns
								(p-applied-tag @67.3-67.7 (degenerate false)))
							(value
								(e-int @67.12-67.13 (value "1"))))
						(branch
							(patterns
								(p-str @68.3-68.4 (text """) (degenerate false)))
							(value
								(e-int @69.1-69.3 (value "0"))))
						(branch
							(patterns
								(p-str @70.3-70.4 (text """) (degenerate false))
								(p-str @70.11-70.12 (text """) (degenerate false)))
							(value
								(e-int @70.20-70.22 (value "20"))))
						(branch
							(patterns
								(p-list @70.22-70.43 (degenerate false)
									(patterns
										(p-int @70.23-70.24 (value "1"))
										(p-int @70.26-70.27 (value "2"))
										(p-int @70.29-70.30 (value "3")))
									(rest-at (index 3)
										(p-assign @70.38-70.42 (ident "rest")))))
							(value
								(e-runtime-error (tag "ident_not_in_scope"))))
						(branch
							(patterns
								(p-list @74.3-74.28 (degenerate false)
									(patterns
										(p-int @74.4-74.5 (value "1"))
										(p-runtime-error @1.1-1.1 (tag "not_implemented"))
										(p-int @74.14-74.15 (value "3")))
									(rest-at (index 3)
										(p-assign @74.23-74.27 (ident "rest")))))
							(value
								(e-int @74.32-74.35 (value "123"))))
						(branch
							(patterns
								(p-list @75.3-77.4 (degenerate false)
									(patterns
										(p-assign @76.1-76.4 (ident "ist")))))
							(value
								(e-int @77.8-77.11 (value "123"))))
						(branch
							(patterns
								(p-small-dec @78.3-78.7 (degenerate false)))
							(value
								(e-int @78.11-78.14 (value "314"))))
						(branch
							(patterns
								(p-small-dec @79.3-79.7 (degenerate false))
								(p-small-dec @79.10-79.14 (degenerate false)))
							(value
								(e-int @79.18-79.21 (value "314"))))
						(branch
							(patterns
								(p-tuple @80.3-80.12 (degenerate false)
									(patterns
										(p-int @80.4-80.5 (value "1"))
										(p-int @80.7-80.8 (value "2"))
										(p-int @80.10-80.11 (value "3")))))
							(value
								(e-int @80.16-80.19 (value "123"))))
						(branch
							(patterns
								(p-tuple @81.3-81.16 (degenerate false)
									(patterns
										(p-int @81.4-81.5 (value "1"))
										(p-runtime-error @1.1-1.1 (tag "not_implemented"))
										(p-int @81.14-81.15 (value "3")))))
							(value
								(e-int @81.20-81.23 (value "123"))))
						(branch
							(patterns
								(p-record-destructure @82.3-82.29 (degenerate false)
									(destructs
										(record-destruct @82.5-82.12 (label "foo") (ident "foo")
											(sub-pattern
												(p-int @82.10-82.11 (value "1"))))
										(record-destruct @82.13-82.20 (label "bar") (ident "bar")
											(sub-pattern
												(p-int @82.18-82.19 (value "2"))))
										(record-destruct @82.21-82.29 (label "rest") (ident "rest")
											(required)))))
							(value
								(e-runtime-error (tag "not_implemented"))))
						(branch
							(patterns
								(p-runtime-error @88.4-88.7 (tag "not_implemented") (degenerate false)))
							(value
								(e-int @88.11-88.13 (value "12"))))
						(branch
							(patterns
								(p-record-destructure @89.3-89.25 (degenerate false)
									(destructs
										(record-destruct @89.5-89.12 (label "foo") (ident "foo")
											(sub-pattern
												(p-int @89.10-89.11 (value "1"))))
										(record-destruct @89.13-89.25 (label "bar") (ident "bar")
											(sub-pattern
												(p-runtime-error @1.1-1.1 (tag "not_implemented")))))))
							(value
								(e-int @89.29-89.31 (value "12"))))
						(branch
							(patterns
								(p-record-destructure @90.3-92.5 (degenerate false)
									(destructs
										(record-destruct @91.4-91.11 (label "foo") (ident "foo")
											(sub-pattern
												(p-int @91.9-91.10 (value "1")))))))
							(value
								(e-int @92.9-92.11 (value "12"))))
						(branch
							(patterns
								(p-applied-tag @93.3-93.10 (degenerate false)))
							(value
								(e-int @93.14-93.20 (value "121000")))))))))
	(d-let
		(p-assign @100.1-100.6 (ident "main!"))
		(e-lambda @100.9-159.2
			(args
				(p-underscore @100.10-100.11))
			(e-block @100.13-159.2
				(s-let @101.2-101.17
					(p-assign @101.2-101.7 (ident "world"))
					(e-string @101.10-101.17
						(e-literal @101.11-101.16 (string "World"))))
				(s-var @102.2-103.8
					(p-assign @102.2-103.8 (ident "number"))
					(e-int @102.15-102.18 (value "123")))
				(s-expect @103.2-104.5
					(e-binop @103.9-104.5 (op "eq")
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-int @103.17-103.18 (value "1"))))
				(s-let @104.2-104.12
					(p-assign @104.2-104.5 (ident "tag"))
					(e-tag @104.8-104.12 (name "Blue")))
				(s-return @105.2-110.5
					(e-lookup-local @106.3-106.6
						(pattern @104.2-104.5)))
				(s-expr @110.2-111.12
					(e-not-implemented))
				(s-expr @111.2-114.11
					(e-call @111.2-113.3
						(e-lookup-local @111.2-111.12
							(pattern @60.1-60.11))
						(e-not-implemented)))
				(s-expr @114.2-118.7
					(e-call @114.2-117.3
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-dbg @115.3-116.7
							(e-int @116.4-116.6 (value "42")))))
				(s-crash @1.1-1.1 (msg "Unreachtement"))
				(s-let @119.2-119.23
					(p-assign @119.2-119.10 (ident "tag_with"))
					(e-tag @119.13-119.23 (name "Ok")
						(args
							(e-lookup-local @119.16-119.22
								(pattern @102.2-103.8)))))
				(s-let @120.2-120.26
					(p-assign @120.2-120.6 (ident "ited"))
					(e-string @120.9-120.26
						(e-literal @120.10-120.17 (string "Hello, "))
						(e-lookup-local @120.19-120.24
							(pattern @101.2-101.7))
						(e-literal @120.25-120.25 (string ""))))
				(s-let @121.2-125.4
					(p-assign @121.2-121.6 (ident "list"))
					(e-empty_list @121.9-125.4))
				(s-expr @125.6-125.10
					(e-int @125.6-125.9 (value "456")))
				(s-let @131.2-131.71
					(p-assign @131.2-131.8 (ident "record"))
					(e-record @131.11-131.71
						(fields
							(field (name "foo")
								(e-int @131.18-131.21 (value "123")))
							(field (name "bar")
								(e-string @131.28-131.35
									(e-literal @131.29-131.34 (string "Hello"))))
							(field (name "baz")
								(e-lookup-local @131.42-131.45
									(pattern @104.2-104.5)))
							(field (name "qux")
								(e-tag @131.52-131.61 (name "Ok")
									(args
										(e-lookup-local @131.55-131.60
											(pattern @101.2-101.7)))))
							(field (name "punned")
								(e-runtime-error (tag "ident_not_in_scope"))))))
				(s-let @132.2-132.68
					(p-assign @132.2-132.7 (ident "tuple"))
					(e-tuple @132.10-132.68
						(elems
							(e-int @132.11-132.14 (value "123"))
							(e-string @132.16-132.23
								(e-literal @132.17-132.22 (string "World")))
							(e-lookup-local @132.25-132.28
								(pattern @104.2-104.5))
							(e-tag @132.30-132.39 (name "Ok")
								(args
									(e-lookup-local @132.33-132.38
										(pattern @101.2-101.7))))
							(e-tuple @132.41-132.56
								(elems
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-lookup-local @132.50-132.55
										(pattern @132.2-132.7))))
							(e-list @132.58-132.67
								(elems
									(e-int @132.59-132.60 (value "1"))
									(e-int @132.62-132.63 (value "2"))
									(e-int @132.65-132.66 (value "3")))))))
				(s-let @133.2-140.3
					(p-assign @133.2-133.9 (ident "m_tuple"))
					(e-tuple @133.12-140.3
						(elems
							(e-int @134.3-134.6 (value "123"))
							(e-string @135.3-135.10
								(e-literal @135.4-135.9 (string "World")))
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-tag @137.3-137.12 (name "Ok")
								(args
									(e-lookup-local @137.6-137.11
										(pattern @101.2-101.7))))
							(e-tuple @138.3-138.18
								(elems
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-lookup-local @138.12-138.17
										(pattern @132.2-132.7))))
							(e-list @139.3-139.12
								(elems
									(e-int @139.4-139.5 (value "1"))
									(e-int @139.7-139.8 (value "2"))
									(e-int @139.10-139.11 (value "3")))))))
				(s-let @141.2-142.7
					(p-assign @141.2-141.7 (ident "bsult"))
					(e-binop @141.10-142.7 (op "or")
						(e-binop @141.10-141.66 (op "or")
							(e-binop @141.10-141.35 (op "gt")
								(e-binop @141.10-141.26 (op "null_coalesce")
									(e-tag @141.10-141.18 (name "Err")
										(args
											(e-runtime-error (tag "ident_not_in_scope"))))
									(e-int @141.22-141.24 (value "12")))
								(e-binop @141.27-141.35 (op "mul")
									(e-int @141.27-141.28 (value "5"))
									(e-int @141.31-141.32 (value "5"))))
							(e-binop @141.36-141.66 (op "and")
								(e-binop @141.36-141.50 (op "lt")
									(e-binop @141.36-141.44 (op "add")
										(e-int @141.36-141.38 (value "13"))
										(e-int @141.41-141.42 (value "2")))
									(e-int @141.45-141.46 (value "5")))
								(e-binop @141.51-141.66 (op "ge")
									(e-binop @141.51-141.60 (op "sub")
										(e-int @141.51-141.53 (value "10"))
										(e-int @141.56-141.57 (value "1")))
									(e-int @141.61-141.63 (value "16")))))
						(e-binop @141.67-142.7 (op "le")
							(e-int @141.67-141.69 (value "12"))
							(e-binop @141.73-142.7 (op "div")
								(e-int @141.73-141.74 (value "3"))
								(e-int @141.77-141.78 (value "5"))))))
				(s-let @142.2-143.11
					(p-assign @142.2-142.7 (ident "stale"))
					(e-dot-access @142.10-143.11 (field "unknown")
						(receiver
							(e-dot-access @142.10-142.46 (field "unknown")
								(receiver
									(e-dot-access @142.10-142.38 (field "unknown")
										(receiver
											(e-runtime-error (tag "not_implemented")))))))))
				(s-expr @143.2-148.2
					(e-tag @143.2-147.3 (name "Stdoline!")
						(args
							(e-string @144.3-146.10
								(e-literal @144.4-144.14 (string "How about "))
								(e-call @145.4-145.21
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-lookup-local @145.14-145.20
										(pattern @102.2-103.8)))
								(e-literal @146.4-146.9 (string " as a"))))))
				(s-type-anno @150.1-151.6 (name "empty")
					(ty-record @150.9-150.11))
				(s-let @151.1-151.11
					(p-assign @151.1-151.6 (ident "empty"))
					(e-empty_record @151.9-151.11))
				(s-type-anno @153.1-155.7 (name "tuple")
					(ty-apply @153.9-153.25 (symbol "Value")
						(ty-tuple @153.15-153.24
							(ty-var @153.16-153.17 (name "a"))
							(ty-var @153.19-153.20 (name "b"))
							(ty-var @153.22-153.23 (name "c")))))
				(s-expect @155.1-159.2
					(e-block @155.8-159.2
						(s-let @156.2-156.9
							(p-assign @156.2-156.5 (ident "foo"))
							(e-int @156.8-156.9 (value "1")))
						(s-let @157.2-157.10
							(p-assign @157.2-157.6 (ident "blah"))
							(e-int @157.9-157.10 (value "1")))
						(e-binop @158.2-159.2 (op "eq")
							(e-lookup-local @158.2-158.6
								(pattern @157.2-157.6))
							(e-lookup-local @158.10-158.13
								(pattern @156.2-156.5)))))
				(e-expect @155.1-159.2
					(e-block @155.8-159.2
						(s-let @156.2-156.9
							(p-assign @156.2-156.5 (ident "foo"))
							(e-int @156.8-156.9 (value "1")))
						(s-let @157.2-157.10
							(p-assign @157.2-157.6 (ident "blah"))
							(e-int @157.9-157.10 (value "1")))
						(e-binop @158.2-159.2 (op "eq")
							(e-lookup-local @158.2-158.6
								(pattern @157.2-157.6))
							(e-lookup-local @158.10-158.13
								(pattern @156.2-156.5)))))))
		(annotation @100.1-100.6
			(declared-type
				(ty-fn @99.9-99.38 (effectful false)
					(ty-apply @99.9-99.21 (symbol "List")
						(ty @99.14-99.20 (name "String")))
					(ty-apply @99.25-99.38 (symbol "Result")
						(ty-record @99.32-99.34)
						(ty-underscore @99.36-99.37))))))
	(s-alias-decl @15.1-15.41
		(ty-header @15.1-15.10 (name "Map")
			(ty-args
				(ty-var @15.5-15.6 (name "a"))
				(ty-var @15.8-15.9 (name "b"))))
		(ty-fn @15.13-15.41 (effectful false)
			(ty-apply @15.13-15.20 (symbol "List")
				(ty-var @15.18-15.19 (name "a")))
			(ty-parens @15.22-15.30
				(ty-fn @15.23-15.29 (effectful false)
					(ty-var @15.23-15.24 (name "a"))
					(ty-var @15.28-15.29 (name "b"))))
			(ty-apply @15.34-15.41 (symbol "List")
				(ty-var @15.39-15.40 (name "b")))))
	(s-alias-decl @16.1-24.15
		(ty-header @16.1-19.2 (name "MapML")
			(ty-args
				(ty-var @17.2-17.3 (name "a"))
				(ty-var @18.2-18.3 (name "b"))))
		(ty-fn @21.3-24.15 (effectful false)
			(ty-apply @21.3-22.4 (symbol "List"))
			(ty-parens @23.3-23.11
				(ty-fn @23.4-23.10 (effectful false)
					(ty-var @23.4-23.5 (name "a"))
					(ty-var @23.9-23.10 (name "b"))))
			(ty-apply @24.4-24.15 (symbol "List")
				(ty-var @24.12-24.13 (name "b")))))
	(s-alias-decl @26.1-26.17
		(ty-header @26.1-26.4 (name "Foo"))
		(ty-tuple @26.7-26.17
			(ty @26.8-26.11 (name "Bar"))
			(ty @26.13-26.16 (name "Baz"))))
	(s-alias-decl @32.1-32.35
		(ty-header @32.1-32.8 (name "Some")
			(ty-args
				(ty-var @32.6-32.7 (name "a"))))
		(ty-record @32.11-32.35
			(field (field "foo")
				(ty-apply @32.19-32.24 (symbol "Ok")
					(ty-var @32.22-32.23 (name "a"))))
			(field (field "bar")
				(ty-var @32.32-32.33 (name "g")))))
	(s-alias-decl @33.1-35.2
		(ty-header @33.1-33.6 (name "Ml")
			(ty-args
				(ty-var @33.4-33.5 (name "a"))))
		(ty-malformed @34.12-35.2))
	(s-alias-decl @37.1-39.2
		(ty-header @37.1-37.9 (name "Soine")
			(ty-args
				(ty-var @37.7-37.8 (name "a"))))
		(ty-malformed @1.1-39.2))
	(s-alias-decl @40.1-41.2
		(ty-header @40.1-40.5 (name "Maya"))
		(ty-tag-union @40.9-41.2))
	(s-alias-decl @43.1-43.34
		(ty-header @43.1-43.8 (name "Func")
			(ty-args
				(ty-var @43.6-43.7 (name "a"))))
		(ty-fn @43.11-43.34 (effectful false)
			(ty-apply @43.11-43.19 (symbol "Maybe")
				(ty-var @43.17-43.18 (name "a")))
			(ty-var @43.21-43.22 (name "a"))
			(ty-apply @43.26-43.34 (symbol "Maybe")
				(ty-var @43.32-43.33 (name "a")))))
	(s-import @4.1-4.38 (module "pf.Stdout") (qualifier "pf")
		(exposes
			(exposed (name "line!") (wildcard false))
			(exposed (name "e!") (wildcard false))))
	(s-import @6.1-8.4 (module "Stdot")
		(exposes))
	(s-import @10.1-10.46 (module "pkg.S") (qualifier "pkg")
		(exposes
			(exposed (name "func") (alias "fry") (wildcard false))
			(exposed (name "Custom") (wildcard true))))
	(s-import @12.1-12.19 (module "Bae") (alias "Gooe")
		(exposes))
	(s-import @13.1-14.4 (module "Ba")
		(exposes))
	(s-expect @96.1-99.6
		(e-binop @97.2-99.6 (op "eq")
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-int @97.10-97.11 (value "1")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @45.1-45.4 (type "Bool -> Num(*)"))
		(patt @48.1-48.8 (type "Error -> U64"))
		(patt @60.1-60.11 (type "Error"))
		(patt @100.1-100.6 (type "Error -> Error")))
	(type_decls
		(alias @15.1-15.41 (type "Map(a, b)")
			(ty-header @15.1-15.10 (name "Map")
				(ty-args
					(ty-var @15.5-15.6 (name "a"))
					(ty-var @15.8-15.9 (name "b")))))
		(alias @16.1-24.15 (type "MapML(a, b)")
			(ty-header @16.1-19.2 (name "MapML")
				(ty-args
					(ty-var @17.2-17.3 (name "a"))
					(ty-var @18.2-18.3 (name "b")))))
		(alias @26.1-26.17 (type "Foo")
			(ty-header @26.1-26.4 (name "Foo")))
		(alias @32.1-32.35 (type "Some(a)")
			(ty-header @32.1-32.8 (name "Some")
				(ty-args
					(ty-var @32.6-32.7 (name "a")))))
		(alias @33.1-35.2 (type "Ml(a)")
			(ty-header @33.1-33.6 (name "Ml")
				(ty-args
					(ty-var @33.4-33.5 (name "a")))))
		(alias @37.1-39.2 (type "Soine(a)")
			(ty-header @37.1-37.9 (name "Soine")
				(ty-args
					(ty-var @37.7-37.8 (name "a")))))
		(alias @40.1-41.2 (type "Maya")
			(ty-header @40.1-40.5 (name "Maya")))
		(alias @43.1-43.34 (type "Func(a)")
			(ty-header @43.1-43.8 (name "Func")
				(ty-args
					(ty-var @43.6-43.7 (name "a"))))))
	(expressions
		(expr @45.7-47.8 (type "Bool -> Num(*)"))
		(expr @48.11-58.2 (type "Error -> U64"))
		(expr @60.14-94.3 (type "Error"))
		(expr @100.9-159.2 (type "Error -> Error"))))
~~~
