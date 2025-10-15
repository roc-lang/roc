# META
~~~ini
description=Multiline formatting everything
type=snippet
~~~
# SOURCE
~~~roc
# Import exposing
import I1 exposing [
	I11,
	I12,
]
import I2 exposing [
	I21 as Ias1,
	I22 as Ias2,
]

# Where constraint
A(a) : a
	where [
		a.a1 : (
			a,
			a,
		) -> Str,
		a.a2 : (
			a,
			a,
		) -> Str,
	]
B(b) : b
	where [
		b.b1 : (
			b,
			b,
		) -> Str,
		b.b2 : (
			b,
			b,
		) -> Str,
	]

C(
	a,
	b,
) : (
	a,
	b,
)
D(
	a,
	b,
) : C(
	a,
	b,
)
E : {
	a : Str,
	b : Str,
}
F : [
	A,
	B,
]

g : e -> e
	where [
		e.A,
		e.B,
	]

h = |x, y| {
	h1 = {
		h11: x,
		h12: x,
		h13: {
			h131: x,
			h132: y,
		},
	}
	h2 = h(
		x,
		y,
	)
	h3 = A(
		x,
		y,
	)
	h4 = [
		x,
		y,
	]
	h5 = (
		x,
		y,
	)

	match x {
		Z1(
			(
				a,
				b,
			),
		) => a
		Z2(
			a,
			b,
		) => a
		Z3(
			{
				a,
				b,
			},
		) => a
		Z4(
			[
				a,
				b,
			],
		) => a
	}
}
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:12:1:22:3
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:23:1:33:3
MODULE NOT FOUND - everything.md:2:1:5:2
MODULE NOT FOUND - everything.md:6:1:9:2
UNUSED VARIABLE - everything.md:94:5:94:6
UNUSED VARIABLE - everything.md:99:4:99:5
UNUSED VARIABLE - everything.md:104:5:104:6
UNUSED VARIABLE - everything.md:110:5:110:6
UNUSED VARIABLE - everything.md:65:2:65:4
UNUSED VARIABLE - everything.md:73:2:73:4
UNUSED VARIABLE - everything.md:77:2:77:4
UNUSED VARIABLE - everything.md:81:2:81:4
UNUSED VARIABLE - everything.md:85:2:85:4
# PROBLEMS
**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**everything.md:12:1:22:3:**
```roc
A(a) : a
	where [
		a.a1 : (
			a,
			a,
		) -> Str,
		a.a2 : (
			a,
			a,
		) -> Str,
	]
```


**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**everything.md:23:1:33:3:**
```roc
B(b) : b
	where [
		b.b1 : (
			b,
			b,
		) -> Str,
		b.b2 : (
			b,
			b,
		) -> Str,
	]
```


**MODULE NOT FOUND**
The module `I1` was not found in this Roc project.

You're attempting to use this module here:
**everything.md:2:1:5:2:**
```roc
import I1 exposing [
	I11,
	I12,
]
```


**MODULE NOT FOUND**
The module `I2` was not found in this Roc project.

You're attempting to use this module here:
**everything.md:6:1:9:2:**
```roc
import I2 exposing [
	I21 as Ias1,
	I22 as Ias2,
]
```


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:94:5:94:6:**
```roc
				b,
```
				^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:99:4:99:5:**
```roc
			b,
```
			^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:104:5:104:6:**
```roc
				b,
```
				^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:110:5:110:6:**
```roc
				b,
```
				^


**UNUSED VARIABLE**
Variable `h1` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h1` to suppress this warning.
The unused variable is declared here:
**everything.md:65:2:65:4:**
```roc
	h1 = {
```
	^^


**UNUSED VARIABLE**
Variable `h2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h2` to suppress this warning.
The unused variable is declared here:
**everything.md:73:2:73:4:**
```roc
	h2 = h(
```
	^^


**UNUSED VARIABLE**
Variable `h3` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h3` to suppress this warning.
The unused variable is declared here:
**everything.md:77:2:77:4:**
```roc
	h3 = A(
```
	^^


**UNUSED VARIABLE**
Variable `h4` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h4` to suppress this warning.
The unused variable is declared here:
**everything.md:81:2:81:4:**
```roc
	h4 = [
```
	^^


**UNUSED VARIABLE**
Variable `h5` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h5` to suppress this warning.
The unused variable is declared here:
**everything.md:85:2:85:4:**
```roc
	h5 = (
```
	^^


# TOKENS
~~~zig
KwImport(2:1-2:7),UpperIdent(2:8-2:10),KwExposing(2:11-2:19),OpenSquare(2:20-2:21),
UpperIdent(3:2-3:5),Comma(3:5-3:6),
UpperIdent(4:2-4:5),Comma(4:5-4:6),
CloseSquare(5:1-5:2),
KwImport(6:1-6:7),UpperIdent(6:8-6:10),KwExposing(6:11-6:19),OpenSquare(6:20-6:21),
UpperIdent(7:2-7:5),KwAs(7:6-7:8),UpperIdent(7:9-7:13),Comma(7:13-7:14),
UpperIdent(8:2-8:5),KwAs(8:6-8:8),UpperIdent(8:9-8:13),Comma(8:13-8:14),
CloseSquare(9:1-9:2),
UpperIdent(12:1-12:2),NoSpaceOpenRound(12:2-12:3),LowerIdent(12:3-12:4),CloseRound(12:4-12:5),OpColon(12:6-12:7),LowerIdent(12:8-12:9),
KwWhere(13:2-13:7),OpenSquare(13:8-13:9),
LowerIdent(14:3-14:4),NoSpaceDotLowerIdent(14:4-14:7),OpColon(14:8-14:9),OpenRound(14:10-14:11),
LowerIdent(15:4-15:5),Comma(15:5-15:6),
LowerIdent(16:4-16:5),Comma(16:5-16:6),
CloseRound(17:3-17:4),OpArrow(17:5-17:7),UpperIdent(17:8-17:11),Comma(17:11-17:12),
LowerIdent(18:3-18:4),NoSpaceDotLowerIdent(18:4-18:7),OpColon(18:8-18:9),OpenRound(18:10-18:11),
LowerIdent(19:4-19:5),Comma(19:5-19:6),
LowerIdent(20:4-20:5),Comma(20:5-20:6),
CloseRound(21:3-21:4),OpArrow(21:5-21:7),UpperIdent(21:8-21:11),Comma(21:11-21:12),
CloseSquare(22:2-22:3),
UpperIdent(23:1-23:2),NoSpaceOpenRound(23:2-23:3),LowerIdent(23:3-23:4),CloseRound(23:4-23:5),OpColon(23:6-23:7),LowerIdent(23:8-23:9),
KwWhere(24:2-24:7),OpenSquare(24:8-24:9),
LowerIdent(25:3-25:4),NoSpaceDotLowerIdent(25:4-25:7),OpColon(25:8-25:9),OpenRound(25:10-25:11),
LowerIdent(26:4-26:5),Comma(26:5-26:6),
LowerIdent(27:4-27:5),Comma(27:5-27:6),
CloseRound(28:3-28:4),OpArrow(28:5-28:7),UpperIdent(28:8-28:11),Comma(28:11-28:12),
LowerIdent(29:3-29:4),NoSpaceDotLowerIdent(29:4-29:7),OpColon(29:8-29:9),OpenRound(29:10-29:11),
LowerIdent(30:4-30:5),Comma(30:5-30:6),
LowerIdent(31:4-31:5),Comma(31:5-31:6),
CloseRound(32:3-32:4),OpArrow(32:5-32:7),UpperIdent(32:8-32:11),Comma(32:11-32:12),
CloseSquare(33:2-33:3),
UpperIdent(35:1-35:2),NoSpaceOpenRound(35:2-35:3),
LowerIdent(36:2-36:3),Comma(36:3-36:4),
LowerIdent(37:2-37:3),Comma(37:3-37:4),
CloseRound(38:1-38:2),OpColon(38:3-38:4),OpenRound(38:5-38:6),
LowerIdent(39:2-39:3),Comma(39:3-39:4),
LowerIdent(40:2-40:3),Comma(40:3-40:4),
CloseRound(41:1-41:2),
UpperIdent(42:1-42:2),NoSpaceOpenRound(42:2-42:3),
LowerIdent(43:2-43:3),Comma(43:3-43:4),
LowerIdent(44:2-44:3),Comma(44:3-44:4),
CloseRound(45:1-45:2),OpColon(45:3-45:4),UpperIdent(45:5-45:6),NoSpaceOpenRound(45:6-45:7),
LowerIdent(46:2-46:3),Comma(46:3-46:4),
LowerIdent(47:2-47:3),Comma(47:3-47:4),
CloseRound(48:1-48:2),
UpperIdent(49:1-49:2),OpColon(49:3-49:4),OpenCurly(49:5-49:6),
LowerIdent(50:2-50:3),OpColon(50:4-50:5),UpperIdent(50:6-50:9),Comma(50:9-50:10),
LowerIdent(51:2-51:3),OpColon(51:4-51:5),UpperIdent(51:6-51:9),Comma(51:9-51:10),
CloseCurly(52:1-52:2),
UpperIdent(53:1-53:2),OpColon(53:3-53:4),OpenSquare(53:5-53:6),
UpperIdent(54:2-54:3),Comma(54:3-54:4),
UpperIdent(55:2-55:3),Comma(55:3-55:4),
CloseSquare(56:1-56:2),
LowerIdent(58:1-58:2),OpColon(58:3-58:4),LowerIdent(58:5-58:6),OpArrow(58:7-58:9),LowerIdent(58:10-58:11),
KwWhere(59:2-59:7),OpenSquare(59:8-59:9),
LowerIdent(60:3-60:4),NoSpaceDotUpperIdent(60:4-60:6),Comma(60:6-60:7),
LowerIdent(61:3-61:4),NoSpaceDotUpperIdent(61:4-61:6),Comma(61:6-61:7),
CloseSquare(62:2-62:3),
LowerIdent(64:1-64:2),OpAssign(64:3-64:4),OpBar(64:5-64:6),LowerIdent(64:6-64:7),Comma(64:7-64:8),LowerIdent(64:9-64:10),OpBar(64:10-64:11),OpenCurly(64:12-64:13),
LowerIdent(65:2-65:4),OpAssign(65:5-65:6),OpenCurly(65:7-65:8),
LowerIdent(66:3-66:6),OpColon(66:6-66:7),LowerIdent(66:8-66:9),Comma(66:9-66:10),
LowerIdent(67:3-67:6),OpColon(67:6-67:7),LowerIdent(67:8-67:9),Comma(67:9-67:10),
LowerIdent(68:3-68:6),OpColon(68:6-68:7),OpenCurly(68:8-68:9),
LowerIdent(69:4-69:8),OpColon(69:8-69:9),LowerIdent(69:10-69:11),Comma(69:11-69:12),
LowerIdent(70:4-70:8),OpColon(70:8-70:9),LowerIdent(70:10-70:11),Comma(70:11-70:12),
CloseCurly(71:3-71:4),Comma(71:4-71:5),
CloseCurly(72:2-72:3),
LowerIdent(73:2-73:4),OpAssign(73:5-73:6),LowerIdent(73:7-73:8),NoSpaceOpenRound(73:8-73:9),
LowerIdent(74:3-74:4),Comma(74:4-74:5),
LowerIdent(75:3-75:4),Comma(75:4-75:5),
CloseRound(76:2-76:3),
LowerIdent(77:2-77:4),OpAssign(77:5-77:6),UpperIdent(77:7-77:8),NoSpaceOpenRound(77:8-77:9),
LowerIdent(78:3-78:4),Comma(78:4-78:5),
LowerIdent(79:3-79:4),Comma(79:4-79:5),
CloseRound(80:2-80:3),
LowerIdent(81:2-81:4),OpAssign(81:5-81:6),OpenSquare(81:7-81:8),
LowerIdent(82:3-82:4),Comma(82:4-82:5),
LowerIdent(83:3-83:4),Comma(83:4-83:5),
CloseSquare(84:2-84:3),
LowerIdent(85:2-85:4),OpAssign(85:5-85:6),OpenRound(85:7-85:8),
LowerIdent(86:3-86:4),Comma(86:4-86:5),
LowerIdent(87:3-87:4),Comma(87:4-87:5),
CloseRound(88:2-88:3),
KwMatch(90:2-90:7),LowerIdent(90:8-90:9),OpenCurly(90:10-90:11),
UpperIdent(91:3-91:5),NoSpaceOpenRound(91:5-91:6),
OpenRound(92:4-92:5),
LowerIdent(93:5-93:6),Comma(93:6-93:7),
LowerIdent(94:5-94:6),Comma(94:6-94:7),
CloseRound(95:4-95:5),Comma(95:5-95:6),
CloseRound(96:3-96:4),OpFatArrow(96:5-96:7),LowerIdent(96:8-96:9),
UpperIdent(97:3-97:5),NoSpaceOpenRound(97:5-97:6),
LowerIdent(98:4-98:5),Comma(98:5-98:6),
LowerIdent(99:4-99:5),Comma(99:5-99:6),
CloseRound(100:3-100:4),OpFatArrow(100:5-100:7),LowerIdent(100:8-100:9),
UpperIdent(101:3-101:5),NoSpaceOpenRound(101:5-101:6),
OpenCurly(102:4-102:5),
LowerIdent(103:5-103:6),Comma(103:6-103:7),
LowerIdent(104:5-104:6),Comma(104:6-104:7),
CloseCurly(105:4-105:5),Comma(105:5-105:6),
CloseRound(106:3-106:4),OpFatArrow(106:5-106:7),LowerIdent(106:8-106:9),
UpperIdent(107:3-107:5),NoSpaceOpenRound(107:5-107:6),
OpenSquare(108:4-108:5),
LowerIdent(109:5-109:6),Comma(109:6-109:7),
LowerIdent(110:5-110:6),Comma(110:6-110:7),
CloseSquare(111:4-111:5),Comma(111:5-111:6),
CloseRound(112:3-112:4),OpFatArrow(112:5-112:7),LowerIdent(112:8-112:9),
CloseCurly(113:2-113:3),
CloseCurly(114:1-114:2),
EndOfFile(115:1-115:1),
~~~
# PARSE
~~~clojure
(file @2.1-114.2
	(type-module @2.1-2.7)
	(statements
		(s-import @2.1-5.2 (raw "I1")
			(exposing
				(exposed-upper-ident @3.2-3.5 (text "I11"))
				(exposed-upper-ident @4.2-4.5 (text "I12"))))
		(s-import @6.1-9.2 (raw "I2")
			(exposing
				(exposed-upper-ident @7.2-7.13 (text "I21") (as "Ias1"))
				(exposed-upper-ident @8.2-8.13 (text "I22") (as "Ias2"))))
		(s-type-decl @12.1-22.3
			(header @12.1-12.5 (name "A")
				(args
					(ty-var @12.3-12.4 (raw "a"))))
			(ty-var @12.8-12.9 (raw "a")))
		(s-type-decl @23.1-33.3
			(header @23.1-23.5 (name "B")
				(args
					(ty-var @23.3-23.4 (raw "b"))))
			(ty-var @23.8-23.9 (raw "b")))
		(s-type-decl @35.1-41.2
			(header @35.1-38.2 (name "C")
				(args
					(ty-var @36.2-36.3 (raw "a"))
					(ty-var @37.2-37.3 (raw "b"))))
			(ty-tuple @38.5-41.2
				(ty-var @39.2-39.3 (raw "a"))
				(ty-var @40.2-40.3 (raw "b"))))
		(s-type-decl @42.1-48.2
			(header @42.1-45.2 (name "D")
				(args
					(ty-var @43.2-43.3 (raw "a"))
					(ty-var @44.2-44.3 (raw "b"))))
			(ty-apply @45.5-48.2
				(ty @45.5-45.6 (name "C"))
				(ty-var @46.2-46.3 (raw "a"))
				(ty-var @47.2-47.3 (raw "b"))))
		(s-type-decl @49.1-52.2
			(header @49.1-49.2 (name "E")
				(args))
			(ty-record @49.5-52.2
				(anno-record-field @50.2-50.9 (name "a")
					(ty @50.6-50.9 (name "Str")))
				(anno-record-field @51.2-51.9 (name "b")
					(ty @51.6-51.9 (name "Str")))))
		(s-type-decl @53.1-56.2
			(header @53.1-53.2 (name "F")
				(args))
			(ty-tag-union @53.5-56.2
				(tags
					(ty @54.2-54.3 (name "A"))
					(ty @55.2-55.3 (name "B")))))
		(s-type-anno @58.1-62.3 (name "g")
			(ty-fn @58.5-58.11
				(ty-var @58.5-58.6 (raw "e"))
				(ty-var @58.10-58.11 (raw "e")))
			(where
				(alias @60.3-60.6 (module-of "e") (name "A"))
				(alias @61.3-61.6 (module-of "e") (name "B"))))
		(s-decl @64.1-114.2
			(p-ident @64.1-64.2 (raw "h"))
			(e-lambda @64.5-114.2
				(args
					(p-ident @64.6-64.7 (raw "x"))
					(p-ident @64.9-64.10 (raw "y")))
				(e-block @64.12-114.2
					(statements
						(s-decl @65.2-72.3
							(p-ident @65.2-65.4 (raw "h1"))
							(e-record @65.7-72.3
								(field (field "h11")
									(e-ident @66.8-66.9 (raw "x")))
								(field (field "h12")
									(e-ident @67.8-67.9 (raw "x")))
								(field (field "h13")
									(e-record @68.8-71.4
										(field (field "h131")
											(e-ident @69.10-69.11 (raw "x")))
										(field (field "h132")
											(e-ident @70.10-70.11 (raw "y")))))))
						(s-decl @73.2-76.3
							(p-ident @73.2-73.4 (raw "h2"))
							(e-apply @73.7-76.3
								(e-ident @73.7-73.8 (raw "h"))
								(e-ident @74.3-74.4 (raw "x"))
								(e-ident @75.3-75.4 (raw "y"))))
						(s-decl @77.2-80.3
							(p-ident @77.2-77.4 (raw "h3"))
							(e-apply @77.7-80.3
								(e-tag @77.7-77.8 (raw "A"))
								(e-ident @78.3-78.4 (raw "x"))
								(e-ident @79.3-79.4 (raw "y"))))
						(s-decl @81.2-84.3
							(p-ident @81.2-81.4 (raw "h4"))
							(e-list @81.7-84.3
								(e-ident @82.3-82.4 (raw "x"))
								(e-ident @83.3-83.4 (raw "y"))))
						(s-decl @85.2-88.3
							(p-ident @85.2-85.4 (raw "h5"))
							(e-tuple @85.7-88.3
								(e-ident @86.3-86.4 (raw "x"))
								(e-ident @87.3-87.4 (raw "y"))))
						(e-match
							(e-ident @90.8-90.9 (raw "x"))
							(branches
								(branch @91.3-96.9
									(p-tag @91.3-96.4 (raw "Z1")
										(p-tuple @92.4-95.5
											(p-ident @93.5-93.6 (raw "a"))
											(p-ident @94.5-94.6 (raw "b"))))
									(e-ident @96.8-96.9 (raw "a")))
								(branch @97.3-100.9
									(p-tag @97.3-100.4 (raw "Z2")
										(p-ident @98.4-98.5 (raw "a"))
										(p-ident @99.4-99.5 (raw "b")))
									(e-ident @100.8-100.9 (raw "a")))
								(branch @101.3-106.9
									(p-tag @101.3-106.4 (raw "Z3")
										(p-record @102.4-105.5
											(field @103.5-103.6 (name "a") (rest false))
											(field @104.5-104.6 (name "b") (rest false))))
									(e-ident @106.8-106.9 (raw "a")))
								(branch @107.3-112.9
									(p-tag @107.3-112.4 (raw "Z4")
										(p-list @108.4-111.5
											(p-ident @109.5-109.6 (raw "a"))
											(p-ident @110.5-110.6 (raw "b"))))
									(e-ident @112.8-112.9 (raw "a")))))))))))
~~~
# FORMATTED
~~~roc
# Import exposing
import I1 exposing [
	I11,
	I12,
]
import I2 exposing [
	I21 as Ias1,
	I22 as Ias2,
]

# Where constraint
A(a) : a
	where
		[

			a.a1 : (
			a,
			a,
		) -> Str
,
		
			a.a2 : (
			a,
			a,
		) -> Str]
B(b) : b
	where
		[

			b.b1 : (
			b,
			b,
		) -> Str
,
		
			b.b2 : (
			b,
			b,
		) -> Str]

C(
	a,
	b,
) : (
	a,
	b,
)
D(
	a,
	b,
) : C(
	a,
	b,
)
E : {
	a : Str,
	b : Str,
}
F : [
	A,
	B,
]

g : e -> e
	where
		[
e.A
,
		e.B]

h = |x, y| {
	h1 = {
		h11: x,
		h12: x,
		h13: {
			h131: x,
			h132: y,
		},
	}
	h2 = h(
		x,
		y,
	)
	h3 = A(
		x,
		y,
	)
	h4 = [
		x,
		y,
	]
	h5 = (
		x,
		y,
	)

	match x {
		Z1(
			(
				a,
				b,
			),
		) => a
		Z2(
			a,
			b,
		) => a
		Z3(
			{
				a,
				b,
			},
		) => a
		Z4(
			[
				a,
				b,
			],
		) => a
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @64.1-64.2 (ident "h"))
		(e-closure @64.5-114.2
			(captures
				(capture @109.5-109.6 (ident "a"))
				(capture @103.5-103.6 (ident "a"))
				(capture @93.5-93.6 (ident "a"))
				(capture @64.1-64.2 (ident "h"))
				(capture @98.4-98.5 (ident "a")))
			(e-lambda @64.5-114.2
				(args
					(p-assign @64.6-64.7 (ident "x"))
					(p-assign @64.9-64.10 (ident "y")))
				(e-block @64.12-114.2
					(s-let @65.2-72.3
						(p-assign @65.2-65.4 (ident "h1"))
						(e-record @65.7-72.3
							(fields
								(field (name "h11")
									(e-lookup-local @66.8-66.9
										(p-assign @64.6-64.7 (ident "x"))))
								(field (name "h12")
									(e-lookup-local @67.8-67.9
										(p-assign @64.6-64.7 (ident "x"))))
								(field (name "h13")
									(e-record @68.8-71.4
										(fields
											(field (name "h131")
												(e-lookup-local @69.10-69.11
													(p-assign @64.6-64.7 (ident "x"))))
											(field (name "h132")
												(e-lookup-local @70.10-70.11
													(p-assign @64.9-64.10 (ident "y"))))))))))
					(s-let @73.2-76.3
						(p-assign @73.2-73.4 (ident "h2"))
						(e-call @73.7-76.3
							(e-lookup-local @73.7-73.8
								(p-assign @64.1-64.2 (ident "h")))
							(e-lookup-local @74.3-74.4
								(p-assign @64.6-64.7 (ident "x")))
							(e-lookup-local @75.3-75.4
								(p-assign @64.9-64.10 (ident "y")))))
					(s-let @77.2-80.3
						(p-assign @77.2-77.4 (ident "h3"))
						(e-tag @77.7-80.3 (name "A")
							(args
								(e-lookup-local @78.3-78.4
									(p-assign @64.6-64.7 (ident "x")))
								(e-lookup-local @79.3-79.4
									(p-assign @64.9-64.10 (ident "y"))))))
					(s-let @81.2-84.3
						(p-assign @81.2-81.4 (ident "h4"))
						(e-list @81.7-84.3
							(elems
								(e-lookup-local @82.3-82.4
									(p-assign @64.6-64.7 (ident "x")))
								(e-lookup-local @83.3-83.4
									(p-assign @64.9-64.10 (ident "y"))))))
					(s-let @85.2-88.3
						(p-assign @85.2-85.4 (ident "h5"))
						(e-tuple @85.7-88.3
							(elems
								(e-lookup-local @86.3-86.4
									(p-assign @64.6-64.7 (ident "x")))
								(e-lookup-local @87.3-87.4
									(p-assign @64.9-64.10 (ident "y"))))))
					(e-match @90.2-113.3
						(match @90.2-113.3
							(cond
								(e-lookup-local @90.8-90.9
									(p-assign @64.6-64.7 (ident "x"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @91.3-96.4)))
									(value
										(e-lookup-local @96.8-96.9
											(p-assign @93.5-93.6 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @97.3-100.4)))
									(value
										(e-lookup-local @100.8-100.9
											(p-assign @98.4-98.5 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @101.3-106.4)))
									(value
										(e-lookup-local @106.8-106.9
											(p-assign @103.5-103.6 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @107.3-112.4)))
									(value
										(e-lookup-local @112.8-112.9
											(p-assign @109.5-109.6 (ident "a"))))))))))))
	(s-alias-decl @12.1-22.3
		(ty-header @12.1-12.5 (name "A")
			(ty-args
				(ty-rigid-var @12.3-12.4 (name "a"))))
		(ty-rigid-var-lookup (ty-rigid-var @12.3-12.4 (name "a"))))
	(s-alias-decl @23.1-33.3
		(ty-header @23.1-23.5 (name "B")
			(ty-args
				(ty-rigid-var @23.3-23.4 (name "b"))))
		(ty-rigid-var-lookup (ty-rigid-var @23.3-23.4 (name "b"))))
	(s-alias-decl @35.1-41.2
		(ty-header @35.1-38.2 (name "C")
			(ty-args
				(ty-rigid-var @36.2-36.3 (name "a"))
				(ty-rigid-var @37.2-37.3 (name "b"))))
		(ty-tuple @38.5-41.2
			(ty-rigid-var-lookup (ty-rigid-var @36.2-36.3 (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var @37.2-37.3 (name "b")))))
	(s-alias-decl @42.1-48.2
		(ty-header @42.1-45.2 (name "D")
			(ty-args
				(ty-rigid-var @43.2-43.3 (name "a"))
				(ty-rigid-var @44.2-44.3 (name "b"))))
		(ty-apply @45.5-48.2 (name "C") (local)
			(ty-rigid-var-lookup (ty-rigid-var @43.2-43.3 (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var @44.2-44.3 (name "b")))))
	(s-alias-decl @49.1-52.2
		(ty-header @49.1-49.2 (name "E"))
		(ty-record @49.5-52.2
			(field (field "a")
				(ty-lookup @50.6-50.9 (name "Str") (builtin)))
			(field (field "b")
				(ty-lookup @51.6-51.9 (name "Str") (builtin)))))
	(s-alias-decl @53.1-56.2
		(ty-header @53.1-53.2 (name "F"))
		(ty-tag-union @53.5-56.2
			(ty-tag-name @54.2-54.3 (name "A"))
			(ty-tag-name @55.2-55.3 (name "B"))))
	(s-import @2.1-5.2 (module "I1")
		(exposes
			(exposed (name "I11") (wildcard false))
			(exposed (name "I12") (wildcard false))))
	(s-import @6.1-9.2 (module "I2")
		(exposes
			(exposed (name "I21") (alias "Ias1") (wildcard false))
			(exposed (name "I22") (alias "Ias2") (wildcard false))))
	(s-type-anno @58.1-62.3 (name "g")
		(ty-fn @58.5-58.11 (effectful false)
			(ty-rigid-var @58.5-58.6 (name "e"))
			(ty-rigid-var-lookup (ty-rigid-var @58.5-58.6 (name "e"))))
		(where
			(alias @60.3-60.6 (module-of "e") (ident "A"))
			(alias @61.3-61.6 (module-of "e") (ident "B"))))
	(ext-decl @60.3-60.6 (ident "e.A") (kind "type"))
	(ext-decl @61.3-61.6 (ident "e.B") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @64.1-64.2 (type "[Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j, [Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j -> c")))
	(type_decls
		(alias @12.1-22.3 (type "A(a)")
			(ty-header @12.1-12.5 (name "A")
				(ty-args
					(ty-rigid-var @12.3-12.4 (name "a")))))
		(alias @23.1-33.3 (type "B(b)")
			(ty-header @23.1-23.5 (name "B")
				(ty-args
					(ty-rigid-var @23.3-23.4 (name "b")))))
		(alias @35.1-41.2 (type "C(a, b)")
			(ty-header @35.1-38.2 (name "C")
				(ty-args
					(ty-rigid-var @36.2-36.3 (name "a"))
					(ty-rigid-var @37.2-37.3 (name "b")))))
		(alias @42.1-48.2 (type "D(a, b)")
			(ty-header @42.1-45.2 (name "D")
				(ty-args
					(ty-rigid-var @43.2-43.3 (name "a"))
					(ty-rigid-var @44.2-44.3 (name "b")))))
		(alias @49.1-52.2 (type "E")
			(ty-header @49.1-49.2 (name "E")))
		(alias @53.1-56.2 (type "F")
			(ty-header @53.1-53.2 (name "F"))))
	(expressions
		(expr @64.5-114.2 (type "[Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j, [Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j -> c"))))
~~~
