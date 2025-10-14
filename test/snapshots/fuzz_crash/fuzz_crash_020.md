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
UNDECLARED TYPE - fuzz_crash_020.md:13:13:13:16
UNDECLARED TYPE VARIABLE - fuzz_crash_020.md:13:19:13:21
UNDECLARED TYPE VARIABLE - fuzz_crash_020.md:19:4:19:6
UNDECLARED TYPE VARIABLE - fuzz_crash_020.md:20:12:20:13
UNDECLARED TYPE - fuzz_crash_020.md:24:15:24:16
UNDECLARED TYPE VARIABLE - fuzz_crash_020.md:24:24:24:25
MODULE NOT FOUND - fuzz_crash_020.md:4:1:4:34
MODULE NOT FOUND - fuzz_crash_020.md:6:1:8:6
MODULE NOT FOUND - fuzz_crash_020.md:10:1:10:19
MODULE NOT FOUND - fuzz_crash_020.md:11:1:12:4
UNDECLARED TYPE - fuzz_crash_020.md:37:7:37:9
UNDEFINED VARIABLE - fuzz_crash_020.md:40:5:40:8
UNDEFINED VARIABLE - fuzz_crash_020.md:42:4:42:5
UNDEFINED VARIABLE - fuzz_crash_020.md:42:6:42:10
UNDEFINED VARIABLE - fuzz_crash_020.md:45:3:45:4
UNDEFINED VARIABLE - fuzz_crash_020.md:53:2:53:3
UNUSED VARIABLE - fuzz_crash_020.md:52:11:52:14
UNDEFINED VARIABLE - fuzz_crash_020.md:55:11:55:12
UNUSED VARIABLE - fuzz_crash_020.md:57:2:57:4
UNDEFINED VARIABLE - fuzz_crash_020.md:59:3:59:7
UNUSED VARIABLE - fuzz_crash_020.md:60:12:60:15
UNDEFINED VARIABLE - fuzz_crash_020.md:72:2:72:4
UNDECLARED TYPE - fuzz_crash_020.md:74:9:74:15
UNDEFINED VARIABLE - fuzz_crash_020.md:75:11:75:12
UNDEFINED VARIABLE - fuzz_crash_020.md:78:9:78:14
UNDEFINED VARIABLE - fuzz_crash_020.md:80:3:80:6
CRASH EXPECTS STRING - fuzz_crash_020.md:86:3:86:11
UNDEFINED VARIABLE - fuzz_crash_020.md:87:11:87:12
UNDEFINED VARIABLE - fuzz_crash_020.md:89:3:89:6
NOT IMPLEMENTED - :0:0:0:0
UNDEFINED VARIABLE - fuzz_crash_020.md:96:34:96:37
UNDEFINED VARIABLE - fuzz_crash_020.md:96:47:96:52
UNDEFINED VARIABLE - fuzz_crash_020.md:96:54:96:57
DUPLICATE DEFINITION - fuzz_crash_020.md:97:2:97:3
UNDEFINED VARIABLE - fuzz_crash_020.md:97:21:97:24
UNDEFINED VARIABLE - fuzz_crash_020.md:97:30:97:32
UNDEFINED VARIABLE - fuzz_crash_020.md:98:2:98:3
UNDEFINED VARIABLE - fuzz_crash_020.md:100:11:100:14
UNDEFINED VARIABLE - fuzz_crash_020.md:102:4:102:6
UNDEFINED VARIABLE - fuzz_crash_020.md:102:8:102:13
UNDEFINED VARIABLE - fuzz_crash_020.md:105:2:105:3
NOT IMPLEMENTED - :0:0:0:0
UNDEFINED VARIABLE - fuzz_crash_020.md:108:4:108:5
UNDEFINED VARIABLE - fuzz_crash_020.md:108:6:108:8
UNUSED VARIABLE - fuzz_crash_020.md:76:2:76:3
UNUSED VARIABLE - fuzz_crash_020.md:87:2:87:3
UNUSED VARIABLE - fuzz_crash_020.md:96:2:96:4
UNDECLARED TYPE - fuzz_crash_020.md:116:5:116:6
UNDEFINED VARIABLE - fuzz_crash_020.md:119:2:119:5
UNDEFINED VARIABLE - fuzz_crash_020.md:120:1:120:2
UNDEFINED VARIABLE - fuzz_crash_020.md:120:6:120:9
EXPOSED BUT NOT DEFINED - fuzz_crash_020.md:2:6:2:11
TOO FEW ARGS - fuzz_crash_020.md:17:3:18:4
INCOMPATIBLE MATCH PATTERNS - fuzz_crash_020.md:52:2:52:2
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_020.md:52:16:52:16:**
```roc
	match a {lue  {
```
	              ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_020.md:58:4:58:4:**
```roc
			1	"for" => 20[1, ] # t
```
			^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_020.md:59:3:59:3:**
```roc
		ment
```
		^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_020.md:60:16:60:16:**
```roc
		[1, 2, 3,est]123
```
		             ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_020.md:62:5:62:5:**
```roc
		] 23
```
		  ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_020.md:63:7:63:7:**
```roc
		3.1 314
```
		    ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_020.md:66:12:66:12:**
```roc
		(1, 2, 3)123
```
		         ^


**UNDECLARED TYPE**
The type _Lis_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_020.md:13:13:13:16:**
```roc
Map(a, b) : Lis, (ab) -> List(b)
```
            ^^^


**UNDECLARED TYPE VARIABLE**
The type variable _ab_ is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**fuzz_crash_020.md:13:19:13:21:**
```roc
Map(a, b) : Lis, (ab) -> List(b)
```
                  ^^


**UNDECLARED TYPE VARIABLE**
The type variable _ab_ is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**fuzz_crash_020.md:19:4:19:6:**
```roc
		(ab) -> # row
```
		 ^^


**UNDECLARED TYPE VARIABLE**
The type variable _b_ is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**fuzz_crash_020.md:20:12:20:13:**
```roc
			List(			b	) #z)
```
			     			^


**UNDECLARED TYPE**
The type _O_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_020.md:24:15:24:16:**
```roc
Som : { foo : O, bar : g }
```
              ^


**UNDECLARED TYPE VARIABLE**
The type variable _g_ is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**fuzz_crash_020.md:24:24:24:25:**
```roc
Som : { foo : O, bar : g }
```
                       ^


**MODULE NOT FOUND**
The module `pf.Stdout` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_020.md:4:1:4:34:**
```roc
import pf.Stdout exposing [line!]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `Stdot` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_020.md:6:1:8:6:**
```roc
import Stdot
		exposing [ #tem
Cust]
```


**MODULE NOT FOUND**
The module `Bae` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_020.md:10:1:10:19:**
```roc
import Bae as Gooe
```
^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `Ba` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_020.md:11:1:12:4:**
```roc
import
	Ba
```


**UNDECLARED TYPE**
The type _U6_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_020.md:37:7:37:9:**
```roc
one : U6
```
      ^^


**UNDEFINED VARIABLE**
Nothing is named `num` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:40:5:40:8:**
```roc
	if num {
```
	   ^^^


**UNDEFINED VARIABLE**
Nothing is named `s` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:42:4:42:5:**
```roc
			s exp0
```
			^


**UNDEFINED VARIABLE**
Nothing is named `exp0` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:42:6:42:10:**
```roc
			s exp0
```
			  ^^^^


**UNDEFINED VARIABLE**
Nothing is named `r` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:45:3:45:4:**
```roc
		r
```
		^


**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:53:2:53:3:**
```roc
	x
```
	^


**UNUSED VARIABLE**
Variable `lue` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_lue` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_020.md:52:11:52:14:**
```roc
	match a {lue  {
```
	         ^^^


**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:55:11:55:12:**
```roc
		Blue=> {x
```
		        ^


**UNUSED VARIABLE**
Variable `er` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_er` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_020.md:57:2:57:4:**
```roc
	er #ent
```
	^^


**UNDEFINED VARIABLE**
Nothing is named `ment` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:59:3:59:7:**
```roc
		ment
```
		^^^^


**UNUSED VARIABLE**
Variable `est` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_est` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_020.md:60:12:60:15:**
```roc
		[1, 2, 3,est]123
```
		         ^^^


**UNDEFINED VARIABLE**
Nothing is named `nt` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:72:2:72:4:**
```roc
	nt
```
	^^


**UNDECLARED TYPE**
The type _Listlt_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_020.md:74:9:74:15:**
```roc
main! : Listlt({}, _)
```
        ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `e` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:75:11:75:12:**
```roc
ma= |_| { e
```
          ^


**UNDEFINED VARIABLE**
Nothing is named `blaue` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:78:9:78:14:**
```roc
	expect blaue
```
	       ^^^^^


**UNDEFINED VARIABLE**
Nothing is named `tag` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:80:3:80:6:**
```roc
		tag
```
		^^^


**CRASH EXPECTS STRING**
The `crash` keyword expects a string literal as its argument.
For example: `crash "Something went wrong"`
**fuzz_crash_020.md:86:3:86:11:**
```roc
	)crash ke"Unr!" #)
```
	 ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `d` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:87:11:87:12:**
```roc
	i= "H, ${d}"
```
	         ^


**UNDEFINED VARIABLE**
Nothing is named `one` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:89:3:89:6:**
```roc
		one(er, 		),	456, # two
```
		^^^


**NOT IMPLEMENTED**
This feature is not yet implemented: statement type in block

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**UNDEFINED VARIABLE**
Nothing is named `tag` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:96:34:96:37:**
```roc
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
```
	                                ^^^


**UNDEFINED VARIABLE**
Nothing is named `world` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:96:47:96:52:**
```roc
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
```
	                                             ^^^^^


**UNDEFINED VARIABLE**
Nothing is named `ned` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:96:54:96:57:**
```roc
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
```
	                                                    ^^^


**DUPLICATE DEFINITION**
The name `t` is being redeclared in this scope.

The redeclaration is here:
**fuzz_crash_020.md:97:2:97:3:**
```roc
	t = (123, "World", tag, O, (nd, t), [1, 2, 3])
```
	^

But `t` was already defined here:
**fuzz_crash_020.md:88:1:88:2:**
```roc
t = [
```
^


**UNDEFINED VARIABLE**
Nothing is named `tag` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:97:21:97:24:**
```roc
	t = (123, "World", tag, O, (nd, t), [1, 2, 3])
```
	                   ^^^


**UNDEFINED VARIABLE**
Nothing is named `nd` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:97:30:97:32:**
```roc
	t = (123, "World", tag, O, (nd, t), [1, 2, 3])
```
	                            ^^


**UNDEFINED VARIABLE**
Nothing is named `m` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:98:2:98:3:**
```roc
	m (
```
	^


**UNDEFINED VARIABLE**
Nothing is named `ag1` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:100:11:100:14:**
```roc
		"World",ag1,
```
		        ^^^


**UNDEFINED VARIABLE**
Nothing is named `ne` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:102:4:102:6:**
```roc
		(ne, tuple),
```
		 ^^


**UNDEFINED VARIABLE**
Nothing is named `tuple` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:102:8:102:13:**
```roc
		(ne, tuple),
```
		     ^^^^^


**UNDEFINED VARIABLE**
Nothing is named `b` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:105:2:105:3:**
```roc
	b?? 12 > 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 e_fn(arg1)?.od()?.ned()?.recd?
```
	^


**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize suffix_single_question expression

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**UNDEFINED VARIABLE**
Nothing is named `r` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:108:4:108:5:**
```roc
			r(nu) # xpr
```
			^


**UNDEFINED VARIABLE**
Nothing is named `nu` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:108:6:108:8:**
```roc
			r(nu) # xpr
```
			  ^^


**UNUSED VARIABLE**
Variable `w` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_w` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_020.md:76:2:76:3:**
```roc
	w = "d"
```
	^


**UNUSED VARIABLE**
Variable `i` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_i` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_020.md:87:2:87:3:**
```roc
	i= "H, ${d}"
```
	^


**UNUSED VARIABLE**
Variable `rd` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rd` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_020.md:96:2:96:4:**
```roc
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
```
	^^


**UNDECLARED TYPE**
The type _V_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_020.md:116:5:116:6:**
```roc
t : V((a,c))
```
    ^


**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:119:2:119:5:**
```roc
	foo == 1
```
	^^^


**UNDEFINED VARIABLE**
Nothing is named `h` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:120:1:120:2:**
```roc
h == foo
```
^


**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_020.md:120:6:120:9:**
```roc
h == foo
```
     ^^^


**EXPOSED BUT NOT DEFINED**
The module header says that `main!` is exposed, but it is not defined anywhere in this module.

**fuzz_crash_020.md:2:6:2:11:**
```roc
app [main!] { pf: platform "c" }
```
     ^^^^^
You can fix this by either defining `main!` in this module, or by removing it from the list of exposed values.

**TOO FEW ARGS**
The type _List_ expects  argument, but got  instead.
**fuzz_crash_020.md:17:3:18:4:**
```roc
		List( #rg
		),
```



**INCOMPATIBLE MATCH PATTERNS**
The pattern in the fourth branch of this `match` differs from previous ones:
**fuzz_crash_020.md:52:2:**
```roc
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
```
     ^^^^^

The fourth pattern has this type:
    _Str_

But all the previous patterns have this type: 
    _[Blue]_others_

All patterns in an `match` must have compatible types.



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
				(ty-tuple
					(ty-var (raw "ab")))
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "b")))))
		(s-type-decl
			(header (name "MapML")
				(args))
			(ty-fn
				(ty-apply
					(ty (name "List")))
				(ty-tuple
					(ty-var (raw "ab")))
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
							(p-string (raw """))
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
						(e-field-access
							(e-field-access
								(e-field-access
									(e-question-suffix
										(e-apply
											(e-ident (raw "e_fn"))
											(e-ident (raw "arg1"))))
									(e-question-suffix
										(e-apply
											(e-ident (raw "od")))))
								(e-question-suffix
									(e-apply
										(e-ident (raw "ned")))))
							(e-question-suffix
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
									(p-str (text """))))
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
		(p-assign (ident "ma"))
		(e-closure
			(captures
				(capture (ident "me")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-expr
						(e-runtime-error (tag "ident_not_in_scope")))
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
						(e-call
							(e-lookup-local
								(p-assign (ident "me")))
							(e-not-implemented)))
					(s-runtime-error (tag "crash_expects_string"))
					(s-expr
						(e-string
							(e-literal (string "Unr!"))))
					(s-let
						(p-assign (ident "i"))
						(e-string
							(e-literal (string "H, "))
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-literal (string ""))))
					(s-let
						(p-assign (ident "t"))
						(e-list
							(elems
								(e-call
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-lookup-local
										(p-assign (ident "er"))))
								(e-num (value "456"))
								(e-num (value "9")))))
					(s-runtime-error (tag "not_implemented"))
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
										(e-lookup-local
											(p-assign (ident "t")))))
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
						(e-binop (op "or")
							(e-binop (op "gt")
								(e-binop (op "null_coalesce")
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-num (value "12")))
								(e-num (value "5")))
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
									(e-num (value "3"))))))
					(s-expr
						(e-dot-access (field "unknown")
							(receiver
								(e-dot-access (field "unknown")
									(receiver
										(e-dot-access (field "unknown")
											(receiver
												(e-runtime-error (tag "not_implemented")))))))))
					(e-tag (name "Stdo!")
						(args
							(e-string
								(e-literal (string "Ho"))
								(e-call
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-runtime-error (tag "ident_not_in_scope")))
								(e-literal (string " ")))))))))
	(d-let
		(p-assign (ident "e"))
		(e-empty_record))
	(s-alias-decl
		(ty-header (name "Map")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-fn (effectful false)
			(ty-malformed)
			(ty-malformed)
			(ty-apply (name "List") (builtin)
				(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
	(s-alias-decl
		(ty-header (name "MapML"))
		(ty-fn (effectful false)
			(ty-apply (name "List") (builtin))
			(ty-malformed)
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
	(s-type-anno (name "line")
		(ty-tuple))
	(s-expect
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-type-anno (name "t")
		(ty-malformed))
	(s-expect
		(e-block
			(s-expr
				(e-binop (op "eq")
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-num (value "1"))))
			(e-binop (op "eq")
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-runtime-error (tag "ident_not_in_scope"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Bool -> Num(_size)"))
		(patt (type "[Rum]_others -> Error"))
		(patt (type "[Blue]_others -> Error"))
		(patt (type "_arg -> [Stdo!(Str)]_others"))
		(patt (type "{}")))
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
		(expr (type "Bool -> Num(_size)"))
		(expr (type "[Rum]_others -> Error"))
		(expr (type "[Blue]_others -> Error"))
		(expr (type "_arg -> [Stdo!(Str)]_others"))
		(expr (type "{}"))))
~~~
