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
LEADING ZERO - :0:0:0:0
UNCLOSED STRING - :0:0:0:0
PARSE ERROR - fuzz_crash_027.md:40:5:40:6
PARSE ERROR - fuzz_crash_027.md:40:7:40:8
PARSE ERROR - fuzz_crash_027.md:40:9:40:10
PARSE ERROR - fuzz_crash_027.md:41:1:41:2
PARSE ERROR - fuzz_crash_027.md:122:3:122:10
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_027.md:125:3:125:4
UNDECLARED TYPE - fuzz_crash_027.md:26:8:26:11
UNDECLARED TYPE - fuzz_crash_027.md:26:13:26:16
UNDECLARED TYPE - fuzz_crash_027.md:32:19:32:21
UNDECLARED TYPE VARIABLE - fuzz_crash_027.md:32:32:32:33
UNDECLARED TYPE - fuzz_crash_027.md:34:8:34:11
UNDECLARED TYPE - fuzz_crash_027.md:38:8:38:11
UNDECLARED TYPE - fuzz_crash_027.md:43:11:43:16
UNDECLARED TYPE - fuzz_crash_027.md:43:26:43:31
MODULE NOT FOUND - fuzz_crash_027.md:4:1:4:38
MODULE NOT FOUND - fuzz_crash_027.md:6:1:8:4
MODULE NOT FOUND - fuzz_crash_027.md:10:1:10:46
MODULE NOT FOUND - fuzz_crash_027.md:12:1:12:19
MODULE NOT FOUND - fuzz_crash_027.md:13:1:14:4
UNDECLARED TYPE - fuzz_crash_027.md:29:2:29:5
UNDECLARED TYPE - fuzz_crash_027.md:30:2:30:5
EMPTY TUPLE NOT ALLOWED - fuzz_crash_027.md:52:1:52:3
UNDEFINED VARIABLE - fuzz_crash_027.md:65:4:65:5
UNDEFINED VARIABLE - fuzz_crash_027.md:65:6:65:7
UNUSED VARIABLE - fuzz_crash_027.md:64:11:64:14
UNDEFINED VARIABLE - fuzz_crash_027.md:71:7:71:11
UNUSED VARIABLE - fuzz_crash_027.md:1:1:1:1
NOT IMPLEMENTED - :0:0:0:0
UNUSED VARIABLE - fuzz_crash_027.md:1:1:1:1
UNUSED VARIABLE - fuzz_crash_027.md:76:1:76:4
NOT IMPLEMENTED - :0:0:0:0
NOT IMPLEMENTED - :0:0:0:0
UNUSED VARIABLE - fuzz_crash_027.md:82:21:82:27
NOT IMPLEMENTED - :0:0:0:0
NOT IMPLEMENTED - :0:0:0:0
UNUSED VARIABLE - fuzz_crash_027.md:62:2:62:3
UNDEFINED VARIABLE - fuzz_crash_027.md:97:2:97:6
UNDECLARED TYPE - fuzz_crash_027.md:99:14:99:20
UNDEFINED VARIABLE - fuzz_crash_027.md:103:9:103:13
UNDEFINED VARIABLE - fuzz_crash_027.md:114:2:114:11
NOT IMPLEMENTED - :0:0:0:0
UNDEFINED VARIABLE - fuzz_crash_027.md:131:63:131:69
UNDEFINED VARIABLE - fuzz_crash_027.md:132:42:132:48
UNDEFINED VARIABLE - fuzz_crash_027.md:136:3:136:7
UNDEFINED VARIABLE - fuzz_crash_027.md:138:4:138:10
UNDEFINED VARIABLE - fuzz_crash_027.md:141:14:141:17
NOT IMPLEMENTED - :0:0:0:0
UNDEFINED VARIABLE - fuzz_crash_027.md:145:4:145:13
UNUSED VARIABLE - fuzz_crash_027.md:119:2:119:10
UNUSED VARIABLE - fuzz_crash_027.md:120:2:120:6
UNUSED VARIABLE - fuzz_crash_027.md:121:2:121:6
UNUSED VARIABLE - fuzz_crash_027.md:131:2:131:8
UNUSED VARIABLE - fuzz_crash_027.md:133:2:133:9
UNUSED VARIABLE - fuzz_crash_027.md:141:2:141:7
UNUSED VARIABLE - fuzz_crash_027.md:142:2:142:7
UNDECLARED TYPE - fuzz_crash_027.md:153:9:153:14
TOO FEW ARGS - fuzz_crash_027.md:21:3:22:4
INVALID IF CONDITION - fuzz_crash_027.md:50:5:50:5
INCOMPATIBLE MATCH PATTERNS - fuzz_crash_027.md:64:2:64:2
TYPE MISMATCH - fuzz_crash_027.md:111:2:113:3
TYPE MISMATCH - fuzz_crash_027.md:143:2:147:3
# PROBLEMS
**LEADING ZERO**
Numbers cannot have leading zeros.



**UNCLOSED STRING**
This string is missing a closing quote.

```roc
	crash "Unreachtement
```
	      ^^^^^^^^^^^^^^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**fuzz_crash_027.md:40:5:40:6:**
```roc
Maya) : [ #
```
    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:40:7:40:8:**
```roc
Maya) : [ #
```
      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:40:9:40:10:**
```roc
Maya) : [ #
```
        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:41:1:41:2:**
```roc
] #se
```
^


**PARSE ERROR**
A parsing error occurred: `expected_expr_apply_close_round`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_027.md:122:3:122:10:**
```roc
		add_one(
```
		^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_027.md:125:3:125:4:**
```roc
		),	456, # ee
```
		^


**UNDECLARED TYPE**
The type _Bar_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:26:8:26:11:**
```roc
Foo : (Bar, Baz)
```
       ^^^


**UNDECLARED TYPE**
The type _Baz_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:26:13:26:16:**
```roc
Foo : (Bar, Baz)
```
            ^^^


**UNDECLARED TYPE**
The type _Ok_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:32:19:32:21:**
```roc
Some(a) : { foo : Ok(a), bar : g }
```
                  ^^


**UNDECLARED TYPE VARIABLE**
The type variable _g_ is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**fuzz_crash_027.md:32:32:32:33:**
```roc
Some(a) : { foo : Ok(a), bar : g }
```
                               ^


**UNDECLARED TYPE**
The type _Som_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:34:8:34:11:**
```roc
	bar : Som# Afld
```
	      ^^^


**UNDECLARED TYPE**
The type _Som_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:38:8:38:11:**
```roc
	bar : Som
```
	      ^^^


**UNDECLARED TYPE**
The type _Maybe_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:43:11:43:16:**
```roc
Func(a) : Maybe(a), a -> Maybe(a)
```
          ^^^^^


**UNDECLARED TYPE**
The type _Maybe_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:43:26:43:31:**
```roc
Func(a) : Maybe(a), a -> Maybe(a)
```
                         ^^^^^


**MODULE NOT FOUND**
The module `pf.Stdout` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_027.md:4:1:4:38:**
```roc
import pf.Stdout exposing [line!, e!]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `Stdot` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_027.md:6:1:8:4:**
```roc
import Stdot
		exposing [ #tem
		] # Cose
```


**MODULE NOT FOUND**
The module `pkg.S` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_027.md:10:1:10:46:**
```roc
import pkg.S exposing [func as fry, Custom.*]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `Bae` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_027.md:12:1:12:19:**
```roc
import Bae as Gooe
```
^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `Ba` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_027.md:13:1:14:4:**
```roc
import
	Ba
```


**UNDECLARED TYPE**
The type _Bar_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:29:2:29:5:**
```roc
	Bar, #
```
	^^^


**UNDECLARED TYPE**
The type _Baz_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:30:2:30:5:**
```roc
	Baz, #m
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
Variable `lue` is not used anywhere in your code.

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
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:1:1:1:1:**
```roc
# Thnt!
```
^


**NOT IMPLEMENTED**
This feature is not yet implemented: alternatives pattern outside match expression

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:1:1:1:1:**
```roc
# Thnt!
```
^


**UNUSED VARIABLE**
Variable `ist` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_ist` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:76:1:76:4:**
```roc
ist
```
^^^


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
**fuzz_crash_027.md:82:21:82:27:**
```roc
		{ foo: 1, bar: 2, ..rest } => 12->add(34)
```
		                  ^^^^^^


**NOT IMPLEMENTED**
This feature is not yet implemented: report an error when unable to resolve field identifier

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**NOT IMPLEMENTED**
This feature is not yet implemented: alternatives pattern outside match expression

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

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
The type _String_ is not declared in this scope.

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
This feature is not yet implemented: statement type in block

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**UNDEFINED VARIABLE**
Nothing is named `punned` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_027.md:131:63:131:69:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```
	                                                             ^^^^^^


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
This feature is not yet implemented: canonicalize suffix_single_question expression

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**UNDEFINED VARIABLE**
Nothing is named `toStr` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_027.md:145:4:145:13:**
```roc
			Num.toStr(number) # on expr
```
			^^^^^^^^^


**UNUSED VARIABLE**
Variable `tag_with` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_tag_with` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:119:2:119:10:**
```roc
	tag_with = Ok(number)
```
	^^^^^^^^


**UNUSED VARIABLE**
Variable `ited` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_ited` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:120:2:120:6:**
```roc
	ited = "Hello, ${world}"
```
	^^^^


**UNUSED VARIABLE**
Variable `list` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_list` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:121:2:121:6:**
```roc
	list = [
```
	^^^^


**UNUSED VARIABLE**
Variable `record` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_record` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:131:2:131:8:**
```roc
	record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
```
	^^^^^^


**UNUSED VARIABLE**
Variable `m_tuple` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_m_tuple` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:133:2:133:9:**
```roc
	m_tuple = (
```
	^^^^^^^


**UNUSED VARIABLE**
Variable `bsult` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_bsult` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:141:2:141:7:**
```roc
	bsult = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
```
	^^^^^


**UNUSED VARIABLE**
Variable `stale` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_stale` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_027.md:142:2:142:7:**
```roc
	stale = some_fn(arg1)?.statod()?.ned()?.recd?
```
	^^^^^


**UNDECLARED TYPE**
The type _Value_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_027.md:153:9:153:14:**
```roc
tuple : Value((a, b, c))
```
        ^^^^^


**TOO FEW ARGS**
The type _List_ expects  argument, but got  instead.
**fuzz_crash_027.md:21:3:22:4:**
```roc
		List( #rg
		),
```



**INVALID IF CONDITION**
This `if` condition needs to be a _Bool_:
**fuzz_crash_027.md:50:5:**
```roc
	if num {
```
    ^^^

Right now, it has the type:
    _Num(Int(Unsigned64))_

Every `if` condition must evaluate to a _Bool_–either `True` or `False`.

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
  ^^^^^

The third pattern has this type:
    _Str_

But all the previous patterns have this type: 
    _[Red, Blue]_others_

All patterns in an `match` must have compatible types.



**TYPE MISMATCH**
This expression is used in an unexpected way:
**fuzz_crash_027.md:111:2:113:3:**
```roc
	match_time(
		..., #
	)
```

It has the type:
    __arg -> _ret_

But I expected it to be:
    _[Red, Blue]_others, _arg -> Error_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**fuzz_crash_027.md:143:2:147:3:**
```roc
	Stdoline!(
		"How about ${ #
			Num.toStr(number) # on expr
		} as a",
	)
```

It has the type:
    _[Stdoline!(Error)][Err(_d), Ok({  })]_

But the type annotation says it should have the type:
    _Result({  }, _d)_

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
							(p-string (raw """))
							(e-int (raw "00")))
						(branch
							(p-alternatives
								(p-string (raw """))
								(p-string (raw """)))
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
								(field (name "app") (rest true)))
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
							(e-list
								(e-malformed (reason "expected_expr_apply_close_round"))
								(e-malformed (reason "expr_unexpected_token"))
								(e-int (raw "456"))))
						(s-for
							(p-ident (raw "n"))
							(e-ident (raw "list"))
							(e-block
								(statements
									(e-apply
										(e-ident (raw "line!"))
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
							(e-record
								(field (field "foo")
									(e-int (raw "123")))
								(field (field "bar")
									(e-string
										(e-string-part (raw "Hello"))))
								(field (field "baz")
									(e-ident (raw "tag")))
								(field (field "qux")
									(e-apply
										(e-tag (raw "Ok"))
										(e-ident (raw "world"))))
								(field (field "punned"))))
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
							(p-ident (raw "m_tuple"))
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
							(p-ident (raw "bsult"))
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
							(p-ident (raw "stale"))
							(e-field-access
								(e-field-access
									(e-field-access
										(e-question-suffix
											(e-apply
												(e-ident (raw "some_fn"))
												(e-ident (raw "arg1"))))
										(e-question-suffix
											(e-apply
												(e-ident (raw "statod")))))
									(e-question-suffix
										(e-apply
											(e-ident (raw "ned")))))
								(e-question-suffix
									(e-ident (raw "recd")))))
						(e-apply
							(e-tag (raw "Stdoline!"))
							(e-string
								(e-string-part (raw "How about "))
								(e-apply
									(e-ident (raw "Num.toStr"))
									(e-ident (raw "number")))
								(e-string-part (raw " as a"))))))))
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
	crash "Unreachtement"
	tag_with = Ok(number)
	ited = "Hello, ${world}"
	list = [
		, # afarg
		,
		456, # ee
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
# CANONICALIZE
~~~clojure
(can-ir
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
								(pattern (degenerate false)
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
									(p-str (text """))))
							(value
								(e-num (value "0"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-str (text """)))
								(pattern (degenerate false)
									(p-str (text """))))
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
												(required
													(p-assign (ident "rest"))))))))
							(value
								(e-runtime-error (tag "not_implemented"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-runtime-error (tag "not_implemented"))))
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
		(e-closure
			(captures
				(capture (ident "match_time")))
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
					(s-crash (msg "Unreachtement"))
					(s-let
						(p-assign (ident "tag_with"))
						(e-tag (name "Ok")
							(args
								(e-lookup-local
									(p-assign (ident "number"))))))
					(s-let
						(p-assign (ident "ited"))
						(e-string
							(e-literal (string "Hello, "))
							(e-lookup-local
								(p-assign (ident "world")))
							(e-literal (string ""))))
					(s-let
						(p-assign (ident "list"))
						(e-list
							(elems
								(e-num (value "456")))))
					(s-runtime-error (tag "not_implemented"))
					(s-let
						(p-assign (ident "record"))
						(e-record
							(fields
								(field (name "foo")
									(e-num (value "123")))
								(field (name "bar")
									(e-string
										(e-literal (string "Hello"))))
								(field (name "baz")
									(e-lookup-local
										(p-assign (ident "tag"))))
								(field (name "qux")
									(e-tag (name "Ok")
										(args
											(e-lookup-local
												(p-assign (ident "world"))))))
								(field (name "punned")
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
										(e-lookup-local
											(p-assign (ident "tuple")))))
								(e-list
									(elems
										(e-num (value "1"))
										(e-num (value "2"))
										(e-num (value "3")))))))
					(s-let
						(p-assign (ident "m_tuple"))
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
						(p-assign (ident "bsult"))
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
						(p-assign (ident "stale"))
						(e-dot-access (field "unknown")
							(receiver
								(e-dot-access (field "unknown")
									(receiver
										(e-dot-access (field "unknown")
											(receiver
												(e-runtime-error (tag "not_implemented")))))))))
					(e-tag (name "Stdoline!")
						(args
							(e-string
								(e-literal (string "How about "))
								(e-call
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-lookup-local
										(p-assign (ident "number"))))
								(e-literal (string " as a"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-malformed))
				(ty-apply (name "Result") (external-module "Result")
					(ty-record)
					(ty-underscore)))))
	(d-let
		(p-assign (ident "empty"))
		(e-empty_record)
		(annotation
			(ty-record)))
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
	(s-type-anno (name "line")
		(ty-tuple
			(ty-malformed)
			(ty-malformed)))
	(s-expect
		(e-binop (op "eq")
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-num (value "1"))))
	(s-type-anno (name "tuple")
		(ty-malformed))
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
		(patt (type "[Red, Blue]_others, _arg -> Error"))
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
		(expr (type "Bool -> Num(_size)"))
		(expr (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(expr (type "[Red, Blue]_others, _arg -> Error"))
		(expr (type "List(Error) -> Error"))
		(expr (type "{}"))))
~~~
