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
# PROBLEMS
**UNDECLARED TYPE**
The type ``Lis`` is not declared in this scope.

This type is referenced here:
**fuzz_crash_019.md:13:13:13:16:**
```roc
Map(a, b) : Lis, (ab) -> List(b)
```
            ^^^


**UNDECLARED TYPE VARIABLE**
The type variable ``ab`` is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**fuzz_crash_019.md:13:19:13:21:**
```roc
Map(a, b) : Lis, (ab) -> List(b)
```
                  ^^


**UNDECLARED TYPE VARIABLE**
The type variable ``ab`` is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**fuzz_crash_019.md:19:4:19:6:**
```roc
		(ab) -> # row
```
   ^^


**UNDECLARED TYPE VARIABLE**
The type variable ``b`` is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**fuzz_crash_019.md:20:12:20:13:**
```roc
			List(			b	) #z)
```
           ^


**UNDECLARED TYPE**
The type ``O`` is not declared in this scope.

This type is referenced here:
**fuzz_crash_019.md:24:15:24:16:**
```roc
Som : { foo : O, bar : g }
```
              ^


**UNDECLARED TYPE VARIABLE**
The type variable ``g`` is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**fuzz_crash_019.md:24:24:24:25:**
```roc
Som : { foo : O, bar : g }
```
                       ^


**UNDECLARED TYPE**
The type ``U6`` is not declared in this scope.

This type is referenced here:
**fuzz_crash_019.md:37:7:37:9:**
```roc
one : U6
```
      ^^


**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize dbg expression
Let us know if you want to help!

**UNDEFINED VARIABLE**
Nothing is named `exp0` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize dbg expression
Let us know if you want to help!

**UNDEFINED VARIABLE**
Nothing is named `r` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize list pattern
Let us know if you want to help!

**UNDEFINED VARIABLE**
Nothing is named `ment` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize list pattern
Let us know if you want to help!

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize list pattern
Let us know if you want to help!

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize alternatives pattern
Let us know if you want to help!

**UNUSED VARIABLE**
Variable ``lue`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_lue` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_019.md:52:11:52:14:**
```roc
	match a {lue  {
```
          ^^^


**UNUSED VARIABLE**
Variable ``er`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_er` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_019.md:57:2:57:4:**
```roc
	er #ent
```
 ^^


**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: top-level expect
Let us know if you want to help!

**UNDEFINED VARIABLE**
Nothing is named `e` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: statement type in block
Let us know if you want to help!

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: statement type in block
Let us know if you want to help!

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: ...
Let us know if you want to help!

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: ...
Let us know if you want to help!

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: crash statement
Let us know if you want to help!

**UNDEFINED VARIABLE**
Nothing is named `d` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `one` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: statement type in block
Let us know if you want to help!

**UNDEFINED VARIABLE**
Nothing is named `tag` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `world` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `ned` in this scope.
Is there an `import` or `exposing` missing up-top?

**DUPLICATE DEFINITION**
The name `t` is being redeclared in this scope.

The redeclaration is here:
**fuzz_crash_019.md:97:2:97:3:**
```roc
	t = (123, "World", tag, O, (nd, t), [1, 2, 3])
```
 ^

But `t` was already defined here:
**fuzz_crash_019.md:88:1:88:2:**
```roc
t = [
```
^


**UNDEFINED VARIABLE**
Nothing is named `tag` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `nd` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `m` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `ag1` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `ne` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `tuple` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `b` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize suffix_single_question expression
Let us know if you want to help!

**UNDEFINED VARIABLE**
Nothing is named `r` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `nu` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNUSED VARIABLE**
Variable ``i`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_i` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_019.md:87:2:87:3:**
```roc
	i= "H, ${d}"
```
 ^


**UNUSED VARIABLE**
Variable ``w`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_w` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_019.md:76:2:76:3:**
```roc
	w = "d"
```
 ^


**UNUSED VARIABLE**
Variable ``t`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_t` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_019.md:88:1:88:2:**
```roc
t = [
```
^


**UNUSED VARIABLE**
Variable ``rd`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rd` to suppress this warning.
The unused variable is declared here:
**fuzz_crash_019.md:96:2:96:4:**
```roc
	rd = { foo: 123, bar: "H", baz: tag, qux: Ok(world),ned }
```
 ^^


**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: top-level expect
Let us know if you want to help!

# TOKENS
~~~zig
Newline(1:2-1:8),
KwApp(2:1-2:4),OpenSquare(2:5-2:6),LowerIdent(2:6-2:11),CloseSquare(2:11-2:12),OpenCurly(2:13-2:14),LowerIdent(2:15-2:17),OpColon(2:17-2:18),KwPlatform(2:19-2:27),StringStart(2:28-2:29),StringPart(2:29-2:30),StringEnd(2:30-2:31),CloseCurly(2:32-2:33),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(4:1-4:7),LowerIdent(4:8-4:10),NoSpaceDotUpperIdent(4:10-4:17),KwExposing(4:18-4:26),OpenSquare(4:27-4:28),LowerIdent(4:28-4:33),CloseSquare(4:33-4:34),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(6:1-6:7),UpperIdent(6:8-6:13),Newline(1:1-1:1),
KwExposing(7:3-7:11),OpenSquare(7:12-7:13),Newline(7:15-7:18),
UpperIdent(8:1-8:5),CloseSquare(8:5-8:6),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(10:1-10:7),UpperIdent(10:8-10:11),KwAs(10:12-10:14),UpperIdent(10:15-10:19),Newline(1:1-1:1),
KwImport(11:1-11:7),Newline(1:1-1:1),
UpperIdent(12:2-12:4),Newline(1:1-1:1),
UpperIdent(13:1-13:4),NoSpaceOpenRound(13:4-13:5),LowerIdent(13:5-13:6),Comma(13:6-13:7),LowerIdent(13:8-13:9),CloseRound(13:9-13:10),OpColon(13:11-13:12),UpperIdent(13:13-13:16),Comma(13:16-13:17),OpenRound(13:18-13:19),LowerIdent(13:19-13:21),CloseRound(13:21-13:22),OpArrow(13:23-13:25),UpperIdent(13:26-13:30),NoSpaceOpenRound(13:30-13:31),LowerIdent(13:31-13:32),CloseRound(13:32-13:33),Newline(1:1-1:1),
UpperIdent(14:1-14:6),NoSpaceOpenRound(14:6-14:7),Newline(14:9-14:13),
CloseRound(15:1-15:2),Newline(15:4-15:7),
OpColon(16:2-16:3),Newline(16:5-16:9),
UpperIdent(17:3-17:7),NoSpaceOpenRound(17:7-17:8),Newline(17:10-17:12),
CloseRound(18:3-18:4),Comma(18:4-18:5),Newline(1:1-1:1),
OpenRound(19:3-19:4),LowerIdent(19:4-19:6),CloseRound(19:6-19:7),OpArrow(19:8-19:10),Newline(19:12-19:16),
UpperIdent(20:4-20:8),NoSpaceOpenRound(20:8-20:9),LowerIdent(20:12-20:13),CloseRound(20:14-20:15),Newline(20:17-20:19),
Newline(1:1-1:1),
LowerIdent(22:1-22:5),OpColon(22:6-22:7),OpenRound(22:8-22:9),Newline(22:11-22:14),
CloseRound(23:1-23:2),Newline(23:4-23:7),
UpperIdent(24:1-24:4),OpColon(24:5-24:6),OpenCurly(24:7-24:8),LowerIdent(24:9-24:12),OpColon(24:13-24:14),UpperIdent(24:15-24:16),Comma(24:16-24:17),LowerIdent(24:18-24:21),OpColon(24:22-24:23),LowerIdent(24:24-24:25),CloseCurly(24:26-24:27),Newline(1:1-1:1),
UpperIdent(25:1-25:3),NoSpaceOpenRound(25:3-25:4),LowerIdent(25:4-25:5),CloseRound(25:5-25:6),OpColon(25:7-25:8),OpenCurly(25:9-25:10),Newline(25:12-25:15),
CloseCurly(26:1-26:2),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(28:1-28:6),NoSpaceOpenRound(28:6-28:7),LowerIdent(28:7-28:8),CloseRound(28:8-28:9),OpColon(28:10-28:11),OpenCurly(28:12-28:13),Newline(28:15-28:15),
CloseCurly(29:1-29:2),Newline(29:4-29:4),
UpperIdent(30:1-30:6),NoSpaceOpenRound(30:6-30:7),LowerIdent(30:7-30:8),CloseRound(30:8-30:9),OpColon(30:10-30:11),OpenSquare(30:12-30:13),UpperIdent(30:13-30:18),CloseSquare(30:18-30:19),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(32:1-32:7),NoSpaceOpenRound(32:7-32:8),LowerIdent(32:8-32:9),CloseRound(32:9-32:10),OpColon(32:11-32:12),OpenSquare(32:13-32:14),Newline(32:16-32:16),
CloseSquare(33:1-33:2),Newline(33:4-33:5),
Newline(1:1-1:1),
LowerIdent(35:1-35:4),OpAssign(35:5-35:6),OpBar(35:7-35:8),LowerIdent(35:8-35:11),OpBar(35:11-35:12),KwIf(35:13-35:15),LowerIdent(35:16-35:19),Int(35:20-35:21),KwElse(35:22-35:26),Int(35:27-35:28),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(37:1-37:4),OpColon(37:5-37:6),UpperIdent(37:7-37:9),Newline(1:1-1:1),
LowerIdent(38:1-38:4),OpAssign(38:5-38:6),OpBar(38:7-38:8),LowerIdent(38:8-38:11),OpBar(38:11-38:12),OpenCurly(38:13-38:14),Newline(1:1-1:1),
Int(39:2-39:3),Newline(1:1-1:1),
KwIf(40:2-40:4),LowerIdent(40:5-40:8),OpenCurly(40:9-40:10),Newline(1:1-1:1),
KwDbg(41:3-41:6),Newline(41:8-41:12),
LowerIdent(42:4-42:5),LowerIdent(42:6-42:10),Newline(1:1-1:1),
CloseCurly(43:2-43:3),KwElse(43:4-43:8),OpenCurly(43:9-43:10),Newline(1:1-1:1),
KwDbg(44:3-44:6),Int(44:7-44:10),Newline(1:1-1:1),
LowerIdent(45:3-45:4),Newline(1:1-1:1),
CloseCurly(46:2-46:3),Newline(1:1-1:1),
CloseCurly(47:1-47:2),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(49:1-49:3),OpAssign(49:4-49:5),OpBar(49:6-49:7),Newline(1:1-1:1),
LowerIdent(50:2-50:3),Comma(50:3-50:4),UpperIdent(50:5-50:7),Comma(50:7-50:8),Newline(1:1-1:1),
OpBar(51:1-51:2),Newline(51:4-51:7),
KwMatch(52:2-52:7),LowerIdent(52:8-52:9),OpenCurly(52:10-52:11),LowerIdent(52:11-52:14),OpenCurly(52:16-52:17),Newline(1:1-1:1),
LowerIdent(53:2-53:3),Newline(1:1-1:1),
CloseCurly(54:3-54:4),Newline(1:1-1:1),
UpperIdent(55:3-55:7),OpFatArrow(55:7-55:9),OpenCurly(55:10-55:11),LowerIdent(55:11-55:12),Newline(1:1-1:1),
CloseCurly(56:4-56:5),Newline(1:1-1:1),
LowerIdent(57:2-57:4),Newline(57:6-57:9),
Int(58:4-58:5),StringStart(58:6-58:7),StringPart(58:7-58:10),StringEnd(58:10-58:11),OpFatArrow(58:12-58:14),Int(58:15-58:17),OpenSquare(58:17-58:18),Int(58:18-58:19),Comma(58:19-58:20),CloseSquare(58:21-58:22),Newline(58:24-58:26),
LowerIdent(59:3-59:7),Newline(1:1-1:1),
OpenSquare(60:3-60:4),Int(60:4-60:5),Comma(60:5-60:6),Int(60:7-60:8),Comma(60:8-60:9),Int(60:10-60:11),Comma(60:11-60:12),LowerIdent(60:12-60:15),CloseSquare(60:15-60:16),Int(60:16-60:19),Newline(1:1-1:1),
OpenSquare(61:3-61:4),Newline(1:1-1:1),
CloseSquare(62:3-62:4),Int(62:5-62:7),Newline(1:1-1:1),
Float(63:3-63:6),Int(63:7-63:10),Newline(1:1-1:1),
Float(64:3-64:7),OpBar(64:8-64:9),Float(64:10-64:14),OpFatArrow(64:15-64:17),Int(64:18-64:21),Newline(1:1-1:1),
OpenRound(65:3-65:4),Int(65:4-65:5),Comma(65:5-65:6),CloseRound(65:7-65:8),OpFatArrow(65:9-65:11),Int(65:12-65:15),Newline(1:1-1:1),
OpenRound(66:3-66:4),Int(66:4-66:5),Comma(66:5-66:6),Int(66:7-66:8),Comma(66:8-66:9),Int(66:10-66:11),CloseRound(66:11-66:12),Int(66:12-66:15),Newline(1:1-1:1),
OpenCurly(67:3-67:4),CloseCurly(67:6-67:7),OpFatArrow(67:8-67:10),Int(67:11-67:13),Newline(1:1-1:1),
UpperIdent(68:3-68:5),NoSpaceOpenRound(68:5-68:6),Int(68:6-68:9),CloseRound(68:9-68:10),OpFatArrow(68:11-68:13),Int(68:14-68:16),Newline(1:1-1:1),
CloseCurly(69:2-69:3),Newline(1:1-1:1),
Newline(1:1-1:1),
KwExpect(71:1-71:7),Newline(71:9-71:14),
LowerIdent(72:2-72:4),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(74:1-74:6),OpColon(74:7-74:8),UpperIdent(74:9-74:15),NoSpaceOpenRound(74:15-74:16),OpenCurly(74:16-74:17),CloseCurly(74:17-74:18),Comma(74:18-74:19),Underscore(74:20-74:21),CloseRound(74:21-74:22),Newline(1:1-1:1),
LowerIdent(75:1-75:3),OpAssign(75:3-75:4),OpBar(75:5-75:6),Underscore(75:6-75:7),OpBar(75:7-75:8),OpenCurly(75:9-75:10),LowerIdent(75:11-75:12),Newline(1:1-1:1),
LowerIdent(76:2-76:3),OpAssign(76:4-76:5),StringStart(76:6-76:7),StringPart(76:7-76:8),StringEnd(76:8-76:9),Newline(1:1-1:1),
KwVar(77:2-77:5),LowerIdent(77:6-77:8),OpAssign(77:9-77:10),Int(77:11-77:14),Newline(1:1-1:1),
KwExpect(78:2-78:8),LowerIdent(78:9-78:14),Newline(1:1-1:1),
KwReturn(79:2-79:8),Newline(79:10-79:11),
LowerIdent(80:3-80:6),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(82:3-82:3),
TripleDot(83:2-83:5),Newline(1:1-1:1),
LowerIdent(84:2-84:4),NoSpaceOpenRound(84:4-84:5),Newline(1:1-1:1),
TripleDot(85:3-85:6),Comma(85:6-85:7),Newline(85:9-85:11),
CloseRound(86:2-86:3),KwCrash(86:3-86:8),LowerIdent(86:9-86:11),StringStart(86:11-86:12),StringPart(86:12-86:16),StringEnd(86:16-86:17),Newline(86:19-86:20),
LowerIdent(87:2-87:3),OpAssign(87:3-87:4),StringStart(87:5-87:6),StringPart(87:6-87:9),OpenStringInterpolation(87:9-87:11),LowerIdent(87:11-87:12),CloseStringInterpolation(87:12-87:13),StringPart(87:13-87:13),StringEnd(87:13-87:14),Newline(1:1-1:1),
LowerIdent(88:1-88:2),OpAssign(88:3-88:4),OpenSquare(88:5-88:6),Newline(1:1-1:1),
LowerIdent(89:3-89:6),NoSpaceOpenRound(89:6-89:7),LowerIdent(89:7-89:9),Comma(89:9-89:10),CloseRound(89:13-89:14),Comma(89:14-89:15),Int(89:16-89:19),Comma(89:19-89:20),Newline(89:22-89:26),
Int(90:1-90:2),Comma(90:2-90:3),Newline(90:5-90:7),
CloseSquare(91:2-91:3),Newline(1:1-1:1),
KwFor(92:2-92:5),LowerIdent(92:6-92:7),KwIn(92:8-92:10),LowerIdent(92:11-92:15),OpenCurly(92:16-92:17),Newline(1:1-1:1),
LowerIdent(93:2-93:7),NoSpaceOpenRound(93:7-93:8),StringStart(93:8-93:9),StringPart(93:9-93:12),OpenStringInterpolation(93:12-93:14),LowerIdent(93:14-93:15),CloseStringInterpolation(93:15-93:16),StringPart(93:16-93:20),OpenStringInterpolation(93:20-93:22),LowerIdent(93:22-93:24),CloseStringInterpolation(93:24-93:25),StringPart(93:25-93:25),StringEnd(93:25-93:26),CloseRound(93:26-93:27),Newline(1:1-1:1),
LowerIdent(94:3-94:6),OpPlus(94:7-94:8),LowerIdent(94:9-94:10),Newline(1:1-1:1),
CloseCurly(95:2-95:3),Newline(1:1-1:1),
LowerIdent(96:2-96:4),OpAssign(96:5-96:6),OpenCurly(96:7-96:8),LowerIdent(96:9-96:12),OpColon(96:12-96:13),Int(96:14-96:17),Comma(96:17-96:18),LowerIdent(96:19-96:22),OpColon(96:22-96:23),StringStart(96:24-96:25),StringPart(96:25-96:26),StringEnd(96:26-96:27),Comma(96:27-96:28),LowerIdent(96:29-96:32),OpColon(96:32-96:33),LowerIdent(96:34-96:37),Comma(96:37-96:38),LowerIdent(96:39-96:42),OpColon(96:42-96:43),UpperIdent(96:44-96:46),NoSpaceOpenRound(96:46-96:47),LowerIdent(96:47-96:52),CloseRound(96:52-96:53),Comma(96:53-96:54),LowerIdent(96:54-96:57),CloseCurly(96:58-96:59),Newline(1:1-1:1),
LowerIdent(97:2-97:3),OpAssign(97:4-97:5),OpenRound(97:6-97:7),Int(97:7-97:10),Comma(97:10-97:11),StringStart(97:12-97:13),StringPart(97:13-97:18),StringEnd(97:18-97:19),Comma(97:19-97:20),LowerIdent(97:21-97:24),Comma(97:24-97:25),UpperIdent(97:26-97:27),Comma(97:27-97:28),OpenRound(97:29-97:30),LowerIdent(97:30-97:32),Comma(97:32-97:33),LowerIdent(97:34-97:35),CloseRound(97:35-97:36),Comma(97:36-97:37),OpenSquare(97:38-97:39),Int(97:39-97:40),Comma(97:40-97:41),Int(97:42-97:43),Comma(97:43-97:44),Int(97:45-97:46),CloseSquare(97:46-97:47),CloseRound(97:47-97:48),Newline(1:1-1:1),
LowerIdent(98:2-98:3),OpenRound(98:4-98:5),Newline(1:1-1:1),
Int(99:3-99:6),Comma(99:6-99:7),Newline(1:1-1:1),
StringStart(100:3-100:4),StringPart(100:4-100:9),StringEnd(100:9-100:10),Comma(100:10-100:11),LowerIdent(100:11-100:14),Comma(100:14-100:15),Newline(1:1-1:1),
UpperIdent(101:3-101:4),Comma(101:4-101:5),Newline(101:7-101:10),
OpenRound(102:3-102:4),LowerIdent(102:4-102:6),Comma(102:6-102:7),LowerIdent(102:8-102:13),CloseRound(102:13-102:14),Comma(102:14-102:15),Newline(1:1-1:1),
OpenSquare(103:3-103:4),Int(103:4-103:5),Comma(103:5-103:6),Int(103:7-103:8),Comma(103:8-103:9),Int(103:10-103:11),CloseSquare(103:11-103:12),Comma(103:12-103:13),Newline(1:1-1:1),
CloseRound(104:2-104:3),Newline(1:1-1:1),
LowerIdent(105:2-105:3),OpDoubleQuestion(105:3-105:5),Int(105:6-105:8),OpGreaterThan(105:9-105:10),Int(105:11-105:12),OpOr(105:13-105:15),Int(105:16-105:18),OpPlus(105:19-105:20),Int(105:21-105:22),OpLessThan(105:23-105:24),Int(105:25-105:26),OpAnd(105:27-105:30),Int(105:31-105:33),OpBinaryMinus(105:34-105:35),Int(105:36-105:37),OpGreaterThanOrEq(105:38-105:40),Int(105:41-105:43),OpOr(105:44-105:46),Int(105:47-105:49),OpLessThanOrEq(105:50-105:52),Int(105:53-105:54),LowerIdent(105:55-105:59),NoSpaceOpenRound(105:59-105:60),LowerIdent(105:60-105:64),CloseRound(105:64-105:65),NoSpaceOpQuestion(105:65-105:66),NoSpaceDotLowerIdent(105:66-105:69),NoSpaceOpenRound(105:69-105:70),CloseRound(105:70-105:71),NoSpaceOpQuestion(105:71-105:72),NoSpaceDotLowerIdent(105:72-105:76),NoSpaceOpenRound(105:76-105:77),CloseRound(105:77-105:78),NoSpaceOpQuestion(105:78-105:79),NoSpaceDotLowerIdent(105:79-105:84),NoSpaceOpQuestion(105:84-105:85),Newline(1:1-1:1),
UpperIdent(106:2-106:7),NoSpaceOpenRound(106:7-106:8),Newline(1:1-1:1),
StringStart(107:3-107:4),StringPart(107:4-107:6),OpenStringInterpolation(107:6-107:8),Newline(107:10-107:10),
LowerIdent(108:4-108:5),NoSpaceOpenRound(108:5-108:6),LowerIdent(108:6-108:8),CloseRound(108:8-108:9),Newline(108:11-108:15),
CloseStringInterpolation(109:3-109:4),StringPart(109:4-109:5),StringEnd(109:5-109:6),Comma(109:6-109:7),Newline(1:1-1:1),
CloseRound(110:2-110:3),Newline(1:1-1:1),
CloseCurly(111:1-111:2),Newline(111:4-111:9),
Newline(1:1-1:1),
LowerIdent(113:1-113:2),OpColon(113:3-113:4),OpenCurly(113:5-113:6),CloseCurly(113:6-113:7),Newline(1:1-1:1),
LowerIdent(114:1-114:2),OpAssign(114:3-114:4),OpenCurly(114:5-114:6),CloseCurly(114:6-114:7),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(116:1-116:2),OpColon(116:3-116:4),UpperIdent(116:5-116:6),NoSpaceOpenRound(116:6-116:7),NoSpaceOpenRound(116:7-116:8),LowerIdent(116:8-116:9),Comma(116:9-116:10),LowerIdent(116:10-116:11),CloseRound(116:11-116:12),CloseRound(116:12-116:13),Newline(1:1-1:1),
Newline(1:1-1:1),
KwExpect(118:1-118:7),OpenCurly(118:8-118:9),Newline(1:1-1:1),
LowerIdent(119:2-119:5),OpEquals(119:6-119:8),Int(119:9-119:10),Newline(1:1-1:1),
LowerIdent(120:1-120:2),OpEquals(120:3-120:5),LowerIdent(120:6-120:9),Newline(1:1-1:1),
CloseCurly(121:1-121:2),EndOfFile(121:2-121:2),
~~~
# PARSE
~~~clojure
(file @1.2-121.2
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
		(s-import @4.1-4.34 (module ".Stdout") (qualifier "pf")
			(exposing
				(exposed-lower-ident (text "line!"))))
		(s-import @6.1-8.6 (module "Stdot")
			(exposing
				(exposed-upper-ident (text "Cust"))))
		(s-import @10.1-10.19 (module "Bae") (alias "Gooe"))
		(s-import @11.1-12.4 (module "Ba"))
		(s-type-decl @13.1-14.6
			(header @13.1-13.10 (name "Map")
				(args
					(ty-var @13.5-13.6 (raw "a"))
					(ty-var @13.8-13.9 (raw "b"))))
			(ty-fn @13.13-13.33
				(ty (name "Lis"))
				(ty-tuple @13.18-13.22
					(ty-var @13.19-13.21 (raw "ab")))
				(ty-apply @13.26-13.33
					(ty (name "List"))
					(ty-var @13.31-13.32 (raw "b")))))
		(s-type-decl @14.1-22.5
			(header @14.1-15.2 (name "MapML")
				(args))
			(ty-fn @17.3-20.15
				(ty-apply @17.3-18.4
					(ty (name "List")))
				(ty-tuple @19.3-19.7
					(ty-var @19.4-19.6 (raw "ab")))
				(ty-apply @20.4-20.15
					(ty (name "List"))
					(ty-var @20.12-20.13 (raw "b")))))
		(s-type-anno @22.1-24.4 (name "line")
			(ty-tuple @22.8-23.2))
		(s-type-decl @24.1-25.3
			(header @24.1-24.4 (name "Som")
				(args))
			(ty-record @24.7-24.27
				(anno-record-field @24.9-24.17 (name "foo")
					(ty (name "O")))
				(anno-record-field @24.18-24.27 (name "bar")
					(ty-var @24.24-24.25 (raw "g")))))
		(s-type-decl @25.1-28.6
			(header @25.1-25.6 (name "Ml")
				(args
					(ty-var @25.4-25.5 (raw "a"))))
			(ty-record @25.9-26.2))
		(s-type-decl @28.1-30.6
			(header @28.1-28.9 (name "Soine")
				(args
					(ty-var @28.7-28.8 (raw "a"))))
			(ty-record @28.12-29.2))
		(s-type-decl @30.1-32.7
			(header @30.1-30.9 (name "Maybe")
				(args
					(ty-var @30.7-30.8 (raw "a"))))
			(ty-tag-union @30.12-30.19
				(tags
					(ty (name "Somne")))))
		(s-type-decl @32.1-35.4
			(header @32.1-32.10 (name "Mayine")
				(args
					(ty-var @32.8-32.9 (raw "a"))))
			(ty-tag-union @32.13-33.2
				(tags)))
		(s-decl @35.1-37.4
			(p-ident @35.1-35.4 (raw "ane"))
			(e-lambda @35.7-37.4
				(args
					(p-ident @35.8-35.11 (raw "num")))
				(e-if-then-else @35.13-37.4
					(e-ident @35.16-35.19 (qaul "") (raw "num"))
					(e-int @35.20-35.21 (raw "2"))
					(e-int @35.27-35.28 (raw "5")))))
		(s-type-anno @37.1-38.4 (name "one")
			(ty (name "U6")))
		(s-decl @38.1-47.2
			(p-ident @38.1-38.4 (raw "add"))
			(e-lambda @38.7-47.2
				(args
					(p-ident @38.8-38.11 (raw "num")))
				(e-block @38.13-47.2
					(statements
						(e-int @39.2-39.3 (raw "1"))
						(e-if-then-else @40.2-47.2
							(e-ident @40.5-40.8 (qaul "") (raw "num"))
							(e-block @40.9-43.3
								(statements
									(e-dbg
										(e-ident @42.4-42.5 (qaul "") (raw "s")))
									(e-ident @42.6-42.10 (qaul "") (raw "exp0"))))
							(e-block @43.9-46.3
								(statements
									(e-dbg
										(e-int @44.7-44.10 (raw "123")))
									(e-ident @45.3-45.4 (qaul "") (raw "r")))))))))
		(s-decl @49.1-71.7
			(p-ident @49.1-49.3 (raw "me"))
			(e-lambda @49.6-71.7
				(args
					(p-ident @50.2-50.3 (raw "a"))
					(p-tag @50.5-50.7 (raw "Tb")))
				(e-match
					(e-ident @52.8-52.9 (qaul "") (raw "a"))
					(branches
						(branch @52.11-55.7
							(p-ident @52.11-52.14 (raw "lue"))
							(e-block @52.16-54.4
								(statements
									(e-ident @53.2-53.3 (qaul "") (raw "x")))))
						(branch @55.3-57.4
							(p-tag @55.3-55.7 (raw "Blue"))
							(e-block @55.10-56.5
								(statements
									(e-ident @55.11-55.12 (qaul "") (raw "x")))))
						(branch @57.2-58.7
							(p-ident @57.2-57.4 (raw "er"))
							(e-int @58.4-58.5 (raw "1")))
						(branch @58.6-58.18
							(p-string @58.6-58.7 (raw """))
							(e-int @58.15-58.17 (raw "20")))
						(branch @58.17-60.4
							(p-list @58.17-58.22
								(p-int @58.18-58.19 (raw "1")))
							(e-ident @59.3-59.7 (qaul "") (raw "ment")))
						(branch @60.3-61.4
							(p-list @60.3-60.16
								(p-int @60.4-60.5 (raw "1"))
								(p-int @60.7-60.8 (raw "2"))
								(p-int @60.10-60.11 (raw "3"))
								(p-ident @60.12-60.15 (raw "est")))
							(e-int @60.16-60.19 (raw "123")))
						(branch @61.3-63.6
							(p-list @61.3-62.4)
							(e-int @62.5-62.7 (raw "23")))
						(branch @63.3-64.7
							(p-frac @63.3-63.6 (raw "3.1"))
							(e-int @63.7-63.10 (raw "314")))
						(branch @64.3-65.4
							(p-alternatives
								(p-frac @64.3-64.7 (raw "3.14"))
								(p-frac @64.10-64.14 (raw "6.28")))
							(e-int @64.18-64.21 (raw "314")))
						(branch @65.3-66.4
							(p-tuple @65.3-65.8
								(p-int @65.4-65.5 (raw "1")))
							(e-int @65.12-65.15 (raw "123")))
						(branch @66.3-67.4
							(p-tuple @66.3-66.12
								(p-int @66.4-66.5 (raw "1"))
								(p-int @66.7-66.8 (raw "2"))
								(p-int @66.10-66.11 (raw "3")))
							(e-int @66.12-66.15 (raw "123")))
						(branch @67.3-68.5
							(p-record @67.3-67.7)
							(e-int @67.11-67.13 (raw "12")))
						(branch @68.3-69.3
							(p-tag @68.3-68.10 (raw "Ok")
								(p-int @68.6-68.9 (raw "123")))
							(e-int @68.14-68.16 (raw "12")))))))
		(s-expect @71.1-74.6
			(e-ident @72.2-72.4 (qaul "") (raw "nt")))
		(s-type-anno @74.1-75.3 (name "main!")
			(ty-apply @74.9-74.22
				(ty (name "Listlt"))
				(ty-record @74.16-74.18)
				(_)))
		(s-decl @75.1-111.2
			(p-ident @75.1-75.3 (raw "ma"))
			(e-lambda @75.5-111.2
				(args
					(p-underscore))
				(e-block @75.9-111.2
					(statements
						(e-ident @75.11-75.12 (qaul "") (raw "e"))
						(s-decl @76.2-76.9
							(p-ident @76.2-76.3 (raw "w"))
							(e-string @76.6-76.9
								(e-string-part @76.7-76.8 (raw "d"))))
						(s-var @77.2-78.8 (name "er")
							(e-int @77.11-77.14 (raw "123")))
						(s-expect @78.2-79.8
							(e-ident @78.9-78.14 (qaul "") (raw "blaue")))
						(s-return @79.2-83.5
							(e-ident @80.3-80.6 (qaul "") (raw "tag")))
						(e-ellipsis)
						(e-apply @84.2-86.3
							(e-ident @84.2-84.4 (qaul "") (raw "me"))
							(e-ellipsis))
						(s-crash @86.3-86.12
							(e-ident @86.9-86.11 (qaul "") (raw "ke")))
						(e-string @86.11-86.17
							(e-string-part @86.12-86.16 (raw "Unr!")))
						(s-decl @87.2-87.14
							(p-ident @87.2-87.3 (raw "i"))
							(e-string @87.5-87.14
								(e-string-part @87.6-87.9 (raw "H, "))
								(e-ident @87.11-87.12 (qaul "") (raw "d"))
								(e-string-part @87.13-87.13 (raw ""))))
						(s-decl @88.1-91.3
							(p-ident @88.1-88.2 (raw "t"))
							(e-list @88.5-91.3
								(e-apply @89.3-89.14
									(e-ident @89.3-89.6 (qaul "") (raw "one"))
									(e-ident @89.7-89.9 (qaul "") (raw "er")))
								(e-int @89.16-89.19 (raw "456"))
								(e-int @90.1-90.2 (raw "9"))))
						(s-for
							(p-ident @92.6-92.7 (raw "n"))
							(e-ident @92.11-92.15 (qaul "") (raw "list"))
							(e-block @92.16-95.3
								(statements
									(e-apply @93.2-93.27
										(e-ident @93.2-93.7 (qaul "") (raw "line!"))
										(e-string @93.8-93.26
											(e-string-part @93.9-93.12 (raw "Ag "))
											(e-ident @93.14-93.15 (qaul "") (raw "n"))
											(e-string-part @93.16-93.20 (raw " to "))
											(e-ident @93.22-93.24 (qaul "") (raw "er"))
											(e-string-part @93.25-93.25 (raw ""))))
									(e-binop @94.3-95.3 (op "+")
										(e-ident @94.3-94.6 (qaul "") (raw "ber"))
										(e-ident @94.9-94.10 (qaul "") (raw "n"))))))
						(s-decl @96.2-96.59
							(p-ident @96.2-96.4 (raw "rd"))
							(e-record @96.7-96.59
								(field (field "foo") (optional false)
									(e-int @96.14-96.17 (raw "123")))
								(field (field "bar") (optional false)
									(e-string @96.24-96.27
										(e-string-part @96.25-96.26 (raw "H"))))
								(field (field "baz") (optional false)
									(e-ident @96.34-96.37 (qaul "") (raw "tag")))
								(field (field "qux") (optional false)
									(e-apply @96.44-96.53
										(e-tag @96.44-96.46 (raw "Ok"))
										(e-ident @96.47-96.52 (qaul "") (raw "world"))))
								(field (field "ned") (optional false))))
						(s-decl @97.2-97.48
							(p-ident @97.2-97.3 (raw "t"))
							(e-tuple @97.6-97.48
								(e-int @97.7-97.10 (raw "123"))
								(e-string @97.12-97.19
									(e-string-part @97.13-97.18 (raw "World")))
								(e-ident @97.21-97.24 (qaul "") (raw "tag"))
								(e-tag @97.26-97.27 (raw "O"))
								(e-tuple @97.29-97.36
									(e-ident @97.30-97.32 (qaul "") (raw "nd"))
									(e-ident @97.34-97.35 (qaul "") (raw "t")))
								(e-list @97.38-97.47
									(e-int @97.39-97.40 (raw "1"))
									(e-int @97.42-97.43 (raw "2"))
									(e-int @97.45-97.46 (raw "3")))))
						(e-ident @98.2-98.3 (qaul "") (raw "m"))
						(e-tuple @98.4-104.3
							(e-int @99.3-99.6 (raw "123"))
							(e-string @100.3-100.10
								(e-string-part @100.4-100.9 (raw "World")))
							(e-ident @100.11-100.14 (qaul "") (raw "ag1"))
							(e-tag @101.3-101.4 (raw "O"))
							(e-tuple @102.3-102.14
								(e-ident @102.4-102.6 (qaul "") (raw "ne"))
								(e-ident @102.8-102.13 (qaul "") (raw "tuple")))
							(e-list @103.3-103.12
								(e-int @103.4-103.5 (raw "1"))
								(e-int @103.7-103.8 (raw "2"))
								(e-int @103.10-103.11 (raw "3"))))
						(e-binop @105.2-105.59 (op "or")
							(e-binop @105.2-105.46 (op "or")
								(e-binop @105.2-105.15 (op ">")
									(e-binop @105.2-105.10 (op "??")
										(e-ident @105.2-105.3 (qaul "") (raw "b"))
										(e-int @105.6-105.8 (raw "12")))
									(e-int @105.11-105.12 (raw "5")))
								(e-binop @105.16-105.46 (op "and")
									(e-binop @105.16-105.30 (op "<")
										(e-binop @105.16-105.24 (op "+")
											(e-int @105.16-105.18 (raw "13"))
											(e-int @105.21-105.22 (raw "2")))
										(e-int @105.25-105.26 (raw "5")))
									(e-binop @105.31-105.46 (op ">=")
										(e-binop @105.31-105.40 (op "-")
											(e-int @105.31-105.33 (raw "10"))
											(e-int @105.36-105.37 (raw "1")))
										(e-int @105.41-105.43 (raw "16")))))
							(e-binop @105.47-105.59 (op "<=")
								(e-int @105.47-105.49 (raw "12"))
								(e-int @105.53-105.54 (raw "3"))))
						(e-field-access @105.55-106.7
							(e-field-access @105.55-105.84
								(e-field-access @105.55-105.76
									(e-question-suffix @105.55-105.66
										(e-apply @105.55-105.65
											(e-ident @105.55-105.59 (qaul "") (raw "e_fn"))
											(e-ident @105.60-105.64 (qaul "") (raw "arg1"))))
									(e-question-suffix @105.66-105.72
										(e-apply @105.66-105.71
											(e-ident @105.66-105.69 (qaul "") (raw ".od")))))
								(e-question-suffix @105.72-105.79
									(e-apply @105.72-105.78
										(e-ident @105.72-105.76 (qaul "") (raw ".ned")))))
							(e-question-suffix @105.79-105.85
								(e-ident @105.79-105.84 (qaul "") (raw ".recd"))))
						(e-apply @106.2-110.3
							(e-tag @106.2-106.7 (raw "Stdo!"))
							(e-string @107.3-109.6
								(e-string-part @107.4-107.6 (raw "Ho"))
								(e-apply @108.4-108.9
									(e-ident @108.4-108.5 (qaul "") (raw "r"))
									(e-ident @108.6-108.8 (qaul "") (raw "nu")))
								(e-string-part @109.4-109.5 (raw " "))))))))
		(s-type-anno @113.1-114.2 (name "y")
			(ty-record @113.5-113.7))
		(s-decl @114.1-114.7
			(p-ident @114.1-114.2 (raw "e"))
			(e-record @114.5-114.7))
		(s-type-anno @116.1-118.7 (name "t")
			(ty-apply @116.5-116.13
				(ty (name "V"))
				(ty-tuple @116.7-116.12
					(ty-var @116.8-116.9 (raw "a"))
					(ty-var @116.10-116.11 (raw "c")))))
		(s-expect @118.1-121.2
			(e-block @118.8-121.2
				(statements
					(e-binop @119.2-120.2 (op "==")
						(e-ident @119.2-119.5 (qaul "") (raw "foo"))
						(e-int @119.9-119.10 (raw "1")))
					(e-binop @120.1-121.2 (op "==")
						(e-ident @120.1-120.2 (qaul "") (raw "h"))
						(e-ident @120.6-120.9 (qaul "") (raw "foo"))))))))
~~~
# FORMATTED
~~~roc
# Thnt!
app [main!] { pf: platform "c" }

import pf.Stdout exposing [line!]

import Stdot
	exposing [Cust]

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

Soine(a) : {}
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
	match a {		lue => {
			x
		}
		Blue => {
			x
		}
		er # ent
			=> # ent
				1		"for" => 20		[
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
		"Ho${
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
		(p-assign @35.1-35.4 (ident "ane"))
		(e-lambda @35.7-37.4
			(args
				(p-assign @35.8-35.11 (ident "num")))
			(e-if @35.13-37.4
				(if-branches
					(if-branch
						(e-lookup-local @35.16-35.19
							(pattern @35.8-35.11))
						(e-int @35.20-35.21 (value "2"))))
				(if-else
					(e-int @35.27-35.28 (value "5"))))))
	(d-let
		(p-assign @38.1-38.4 (ident "add"))
		(e-lambda @38.7-47.2
			(args
				(p-assign @38.8-38.11 (ident "num")))
			(e-block @38.13-47.2
				(s-expr @39.2-40.4
					(e-int @39.2-39.3 (value "1")))
				(e-if @40.2-47.2
					(if-branches
						(if-branch
							(e-lookup-local @40.5-40.8
								(pattern @38.8-38.11))
							(e-block @40.9-43.3
								(s-expr @41.3-42.10
									(e-runtime-error (tag "not_implemented")))
								(e-runtime-error (tag "ident_not_in_scope")))))
					(if-else
						(e-block @43.9-46.3
							(s-expr @44.3-45.4
								(e-runtime-error (tag "not_implemented")))
							(e-runtime-error (tag "ident_not_in_scope"))))))))
	(d-let
		(p-assign @49.1-49.3 (ident "me"))
		(e-lambda @49.6-71.7
			(args
				(p-assign @50.2-50.3 (ident "a"))
				(p-applied-tag @50.5-50.7))
			(e-match @52.2-71.7
				(match @52.2-71.7
					(cond
						(e-lookup-local @52.8-52.9
							(pattern @50.2-50.3)))
					(branches
						(branch
							(patterns
								(p-assign @52.11-52.14 (ident "lue") (degenerate false)))
							(value
								(e-block @52.16-54.4
									(e-runtime-error (tag "ident_not_in_scope")))))
						(branch
							(patterns
								(p-applied-tag @55.3-55.7 (degenerate false)))
							(value
								(e-block @55.10-56.5
									(e-runtime-error (tag "ident_not_in_scope")))))
						(branch
							(patterns
								(p-assign @57.2-57.4 (ident "er") (degenerate false)))
							(value
								(e-int @58.4-58.5 (value "1"))))
						(branch
							(patterns
								(p-str @58.6-58.7 (text """) (degenerate false)))
							(value
								(e-int @58.15-58.17 (value "20"))))
						(branch
							(patterns
								(p-runtime-error @1.1-1.1 (tag "not_implemented") (degenerate false)))
							(value
								(e-runtime-error (tag "ident_not_in_scope"))))
						(branch
							(patterns
								(p-runtime-error @1.1-1.1 (tag "not_implemented") (degenerate false)))
							(value
								(e-int @60.16-60.19 (value "123"))))
						(branch
							(patterns
								(p-runtime-error @1.1-1.1 (tag "not_implemented") (degenerate false)))
							(value
								(e-int @62.5-62.7 (value "23"))))
						(branch
							(patterns
								(p-small-dec @63.3-63.6 (degenerate false)))
							(value
								(e-int @63.7-63.10 (value "314"))))
						(branch
							(patterns
								(p-runtime-error @1.1-1.1 (tag "not_implemented") (degenerate false)))
							(value
								(e-int @64.18-64.21 (value "314"))))
						(branch
							(patterns
								(p-tuple @65.3-65.8 (degenerate false)
									(patterns
										(p-int @65.4-65.5))))
							(value
								(e-int @65.12-65.15 (value "123"))))
						(branch
							(patterns
								(p-tuple @66.3-66.12 (degenerate false)
									(patterns
										(p-int @66.4-66.5)
										(p-int @66.7-66.8)
										(p-int @66.10-66.11))))
							(value
								(e-int @66.12-66.15 (value "123"))))
						(branch
							(patterns
								(p-record-destructure @67.3-67.7 (degenerate false)
									(destructs)))
							(value
								(e-int @67.11-67.13 (value "12"))))
						(branch
							(patterns
								(p-applied-tag @68.3-68.10 (degenerate false)))
							(value
								(e-int @68.14-68.16 (value "12")))))))))
	(d-let
		(p-assign @75.1-75.3 (ident "ma"))
		(e-lambda @75.5-111.2
			(args
				(p-underscore @75.6-75.7))
			(e-block @75.9-111.2
				(s-expr @75.11-76.3
					(e-runtime-error (tag "ident_not_in_scope")))
				(s-let @76.2-76.9
					(p-assign @76.2-76.3 (ident "w"))
					(e-string @76.6-76.9
						(e-literal @76.7-76.8 (string "d"))))
				(s-var @77.2-78.8
					(p-assign @77.2-78.8 (ident "er"))
					(e-int @77.11-77.14 (value "123")))
				(s-expr @83.2-84.4
					(e-runtime-error (tag "not_implemented")))
				(s-expr @84.2-86.8
					(e-call @84.2-86.3
						(e-lookup-local @84.2-84.4
							(pattern @49.1-49.3))
						(e-runtime-error (tag "not_implemented"))))
				(s-expr @86.11-86.20
					(e-string @86.11-86.17
						(e-literal @86.12-86.16 (string "Unr!"))))
				(s-let @87.2-87.14
					(p-assign @87.2-87.3 (ident "i"))
					(e-string @87.5-87.14
						(e-literal @87.6-87.9 (string "H, "))
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-literal @87.13-87.13 (string ""))))
				(s-let @88.1-91.3
					(p-assign @88.1-88.2 (ident "t"))
					(e-list @88.5-91.3
						(elems
							(e-call @89.3-89.14
								(e-runtime-error (tag "ident_not_in_scope"))
								(e-lookup-local @89.7-89.9
									(pattern @77.2-78.8)))
							(e-int @89.16-89.19 (value "456"))
							(e-int @90.1-90.2 (value "9")))))
				(s-let @96.2-96.59
					(p-assign @96.2-96.4 (ident "rd"))
					(e-record @96.7-96.59
						(fields
							(field (name "foo")
								(e-int @96.14-96.17 (value "123")))
							(field (name "bar")
								(e-string @96.24-96.27
									(e-literal @96.25-96.26 (string "H"))))
							(field (name "baz")
								(e-runtime-error (tag "ident_not_in_scope")))
							(field (name "qux")
								(e-call @96.44-96.53
									(e-tag @96.44-96.46 (name "Ok") (args "TODO"))
									(e-runtime-error (tag "ident_not_in_scope"))))
							(field (name "ned")
								(e-runtime-error (tag "ident_not_in_scope"))))))
				(s-let @97.2-97.48
					(p-assign @97.2-97.3 (ident "t"))
					(e-tuple @97.6-97.48
						(elems
							(e-int @97.7-97.10 (value "123"))
							(e-string @97.12-97.19
								(e-literal @97.13-97.18 (string "World")))
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-tag @97.26-97.27 (name "O") (args "TODO"))
							(e-tuple @97.29-97.36
								(elems
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-lookup-local @97.34-97.35
										(pattern @97.2-97.3))))
							(e-list @97.38-97.47
								(elems
									(e-int @97.39-97.40 (value "1"))
									(e-int @97.42-97.43 (value "2"))
									(e-int @97.45-97.46 (value "3")))))))
				(s-expr @98.2-98.5
					(e-runtime-error (tag "ident_not_in_scope")))
				(s-expr @98.4-105.3
					(e-tuple @98.4-104.3
						(elems
							(e-int @99.3-99.6 (value "123"))
							(e-string @100.3-100.10
								(e-literal @100.4-100.9 (string "World")))
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-tag @101.3-101.4 (name "O") (args "TODO"))
							(e-tuple @102.3-102.14
								(elems
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-runtime-error (tag "ident_not_in_scope"))))
							(e-list @103.3-103.12
								(elems
									(e-int @103.4-103.5 (value "1"))
									(e-int @103.7-103.8 (value "2"))
									(e-int @103.10-103.11 (value "3")))))))
				(s-expr @105.2-105.59
					(e-binop @105.2-105.59 (op "or")
						(e-binop @105.2-105.46 (op "or")
							(e-binop @105.2-105.15 (op "gt")
								(e-binop @105.2-105.10 (op "null_coalesce")
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-int @105.6-105.8 (value "12")))
								(e-int @105.11-105.12 (value "5")))
							(e-binop @105.16-105.46 (op "and")
								(e-binop @105.16-105.30 (op "lt")
									(e-binop @105.16-105.24 (op "add")
										(e-int @105.16-105.18 (value "13"))
										(e-int @105.21-105.22 (value "2")))
									(e-int @105.25-105.26 (value "5")))
								(e-binop @105.31-105.46 (op "ge")
									(e-binop @105.31-105.40 (op "sub")
										(e-int @105.31-105.33 (value "10"))
										(e-int @105.36-105.37 (value "1")))
									(e-int @105.41-105.43 (value "16")))))
						(e-binop @105.47-105.59 (op "le")
							(e-int @105.47-105.49 (value "12"))
							(e-int @105.53-105.54 (value "3")))))
				(s-expr @105.55-106.7
					(e-dot-access @105.55-106.7 (field "unknown")
						(receiver
							(e-dot-access @105.55-105.84 (field "unknown")
								(receiver
									(e-dot-access @105.55-105.76 (field "unknown")
										(receiver
											(e-runtime-error (tag "not_implemented")))))))))
				(e-call @106.2-110.3
					(e-tag @106.2-106.7 (name "Stdo!") (args "TODO"))
					(e-string @107.3-109.6
						(e-literal @107.4-107.6 (string "Ho"))
						(e-call @108.4-108.9
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-runtime-error (tag "ident_not_in_scope")))
						(e-literal @109.4-109.5 (string " ")))))))
	(d-let
		(p-assign @114.1-114.2 (ident "e"))
		(e-empty_record @114.5-114.7))
	(s-type-decl @13.1-14.6
		(ty-header @13.1-13.10 (name "Map")
			(ty-args
				(ty-var @13.5-13.6 (name "a"))
				(ty-var @13.8-13.9 (name "b"))))
		(ty-fn @13.13-13.33 (effectful false)
			(ty @13.13-13.16 (name "Lis"))
			(ty-tuple @13.18-13.22
				(ty-var @13.19-13.21 (name "ab")))
			(ty-apply @13.26-13.33 (symbol "List")
				(ty-var @13.31-13.32 (name "b")))))
	(s-type-decl @14.1-22.5
		(ty-header @14.1-15.2 (name "MapML"))
		(ty-fn @17.3-20.15 (effectful false)
			(ty-apply @17.3-18.4 (symbol "List"))
			(ty-tuple @19.3-19.7
				(ty-var @19.4-19.6 (name "ab")))
			(ty-apply @20.4-20.15 (symbol "List")
				(ty-var @20.12-20.13 (name "b")))))
	(s-type-decl @24.1-25.3
		(ty-header @24.1-24.4 (name "Som"))
		(ty-record @24.7-24.27
			(field (field "foo")
				(ty @24.15-24.16 (name "O")))
			(field (field "bar")
				(ty-var @24.24-24.25 (name "g")))))
	(s-type-decl @25.1-28.6
		(ty-header @25.1-25.6 (name "Ml")
			(ty-args
				(ty-var @25.4-25.5 (name "a"))))
		(ty-record @25.9-26.2))
	(s-type-decl @28.1-30.6
		(ty-header @28.1-28.9 (name "Soine")
			(ty-args
				(ty-var @28.7-28.8 (name "a"))))
		(ty-record @28.12-29.2))
	(s-type-decl @30.1-32.7
		(ty-header @30.1-30.9 (name "Maybe")
			(ty-args
				(ty-var @30.7-30.8 (name "a"))))
		(ty-tag-union @30.12-30.19
			(ty @30.13-30.18 (name "Somne"))))
	(s-type-decl @32.1-35.4
		(ty-header @32.1-32.10 (name "Mayine")
			(ty-args
				(ty-var @32.8-32.9 (name "a"))))
		(ty-tag-union @32.13-33.2))
	(s-import @4.1-4.34 (module "pf.Stdout") (qualifier "pf")
		(exposes
			(exposed (name "line!") (wildcard false))))
	(s-import @6.1-8.6 (module "Stdot")
		(exposes
			(exposed (name "Cust") (wildcard false))))
	(s-import @10.1-10.19 (module "Bae") (alias "Gooe")
		(exposes))
	(s-import @11.1-12.4 (module "Ba")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @35.1-35.4 (type "* ? Num(*)"))
		(patt @38.1-38.4 (type "* ? Error"))
		(patt @49.1-49.3 (type "*, [Tb]* ? *"))
		(patt @75.1-75.3 (type "* ? *"))
		(patt @114.1-114.2 (type "{}")))
	(expressions
		(expr @35.7-37.4 (type "* ? Num(*)"))
		(expr @38.7-47.2 (type "* ? Error"))
		(expr @49.6-71.7 (type "*, [Tb]* ? *"))
		(expr @75.5-111.2 (type "* ? *"))
		(expr @114.5-114.7 (type "{}"))))
~~~
