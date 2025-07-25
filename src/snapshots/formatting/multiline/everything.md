# META
~~~ini
description=Multiline formatting everything
type=file
~~~
# SOURCE
~~~roc
module []

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
		module(a).a1 : (
			a,
			a,
		) -> Str,
		module(a).a2 : (
			a,
			a,
		) -> Str
B(b) : b
	where
		module(b).b1 : (
			b,
			b,
		) -> Str,
		module(b).b2 : (
			b,
			b,
		) -> Str

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

g : e -> e where module(e).A, module(e).B

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
UNUSED VARIABLE - everything.md:90:5:90:6
UNUSED VARIABLE - everything.md:95:4:95:5
UNUSED VARIABLE - everything.md:100:5:100:6
UNUSED VARIABLE - everything.md:106:5:106:6
UNUSED VARIABLE - everything.md:73:2:73:4
UNUSED VARIABLE - everything.md:77:2:77:4
UNUSED VARIABLE - everything.md:81:2:81:4
UNUSED VARIABLE - everything.md:61:2:61:4
UNUSED VARIABLE - everything.md:69:2:69:4
# PROBLEMS
**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:90:5:90:6:**
```roc
				b,
```
    ^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:95:4:95:5:**
```roc
			b,
```
   ^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:100:5:100:6:**
```roc
				b,
```
    ^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:106:5:106:6:**
```roc
				b,
```
    ^


**UNUSED VARIABLE**
Variable `h3` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h3` to suppress this warning.
The unused variable is declared here:
**everything.md:73:2:73:4:**
```roc
	h3 = A(
```
 ^^


**UNUSED VARIABLE**
Variable `h4` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h4` to suppress this warning.
The unused variable is declared here:
**everything.md:77:2:77:4:**
```roc
	h4 = [
```
 ^^


**UNUSED VARIABLE**
Variable `h5` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h5` to suppress this warning.
The unused variable is declared here:
**everything.md:81:2:81:4:**
```roc
	h5 = (
```
 ^^


**UNUSED VARIABLE**
Variable `h1` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h1` to suppress this warning.
The unused variable is declared here:
**everything.md:61:2:61:4:**
```roc
	h1 = {
```
 ^^


**UNUSED VARIABLE**
Variable `h2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h2` to suppress this warning.
The unused variable is declared here:
**everything.md:69:2:69:4:**
```roc
	h2 = h(
```
 ^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
KwImport(4:1-4:7),UpperIdent(4:8-4:10),KwExposing(4:11-4:19),OpenSquare(4:20-4:21),
UpperIdent(5:2-5:5),Comma(5:5-5:6),
UpperIdent(6:2-6:5),Comma(6:5-6:6),
CloseSquare(7:1-7:2),
KwImport(8:1-8:7),UpperIdent(8:8-8:10),KwExposing(8:11-8:19),OpenSquare(8:20-8:21),
UpperIdent(9:2-9:5),KwAs(9:6-9:8),UpperIdent(9:9-9:13),Comma(9:13-9:14),
UpperIdent(10:2-10:5),KwAs(10:6-10:8),UpperIdent(10:9-10:13),Comma(10:13-10:14),
CloseSquare(11:1-11:2),
UpperIdent(14:1-14:2),NoSpaceOpenRound(14:2-14:3),LowerIdent(14:3-14:4),CloseRound(14:4-14:5),OpColon(14:6-14:7),LowerIdent(14:8-14:9),
KwWhere(15:2-15:7),
KwModule(16:3-16:9),NoSpaceOpenRound(16:9-16:10),LowerIdent(16:10-16:11),CloseRound(16:11-16:12),NoSpaceDotLowerIdent(16:12-16:15),OpColon(16:16-16:17),OpenRound(16:18-16:19),
LowerIdent(17:4-17:5),Comma(17:5-17:6),
LowerIdent(18:4-18:5),Comma(18:5-18:6),
CloseRound(19:3-19:4),OpArrow(19:5-19:7),UpperIdent(19:8-19:11),Comma(19:11-19:12),
KwModule(20:3-20:9),NoSpaceOpenRound(20:9-20:10),LowerIdent(20:10-20:11),CloseRound(20:11-20:12),NoSpaceDotLowerIdent(20:12-20:15),OpColon(20:16-20:17),OpenRound(20:18-20:19),
LowerIdent(21:4-21:5),Comma(21:5-21:6),
LowerIdent(22:4-22:5),Comma(22:5-22:6),
CloseRound(23:3-23:4),OpArrow(23:5-23:7),UpperIdent(23:8-23:11),
UpperIdent(24:1-24:2),NoSpaceOpenRound(24:2-24:3),LowerIdent(24:3-24:4),CloseRound(24:4-24:5),OpColon(24:6-24:7),LowerIdent(24:8-24:9),
KwWhere(25:2-25:7),
KwModule(26:3-26:9),NoSpaceOpenRound(26:9-26:10),LowerIdent(26:10-26:11),CloseRound(26:11-26:12),NoSpaceDotLowerIdent(26:12-26:15),OpColon(26:16-26:17),OpenRound(26:18-26:19),
LowerIdent(27:4-27:5),Comma(27:5-27:6),
LowerIdent(28:4-28:5),Comma(28:5-28:6),
CloseRound(29:3-29:4),OpArrow(29:5-29:7),UpperIdent(29:8-29:11),Comma(29:11-29:12),
KwModule(30:3-30:9),NoSpaceOpenRound(30:9-30:10),LowerIdent(30:10-30:11),CloseRound(30:11-30:12),NoSpaceDotLowerIdent(30:12-30:15),OpColon(30:16-30:17),OpenRound(30:18-30:19),
LowerIdent(31:4-31:5),Comma(31:5-31:6),
LowerIdent(32:4-32:5),Comma(32:5-32:6),
CloseRound(33:3-33:4),OpArrow(33:5-33:7),UpperIdent(33:8-33:11),
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
LowerIdent(58:1-58:2),OpColon(58:3-58:4),LowerIdent(58:5-58:6),OpArrow(58:7-58:9),LowerIdent(58:10-58:11),KwWhere(58:12-58:17),KwModule(58:18-58:24),NoSpaceOpenRound(58:24-58:25),LowerIdent(58:25-58:26),CloseRound(58:26-58:27),NoSpaceDotUpperIdent(58:27-58:29),Comma(58:29-58:30),KwModule(58:31-58:37),NoSpaceOpenRound(58:37-58:38),LowerIdent(58:38-58:39),CloseRound(58:39-58:40),NoSpaceDotUpperIdent(58:40-58:42),
LowerIdent(60:1-60:2),OpAssign(60:3-60:4),OpBar(60:5-60:6),LowerIdent(60:6-60:7),Comma(60:7-60:8),LowerIdent(60:9-60:10),OpBar(60:10-60:11),OpenCurly(60:12-60:13),
LowerIdent(61:2-61:4),OpAssign(61:5-61:6),OpenCurly(61:7-61:8),
LowerIdent(62:3-62:6),OpColon(62:6-62:7),LowerIdent(62:8-62:9),Comma(62:9-62:10),
LowerIdent(63:3-63:6),OpColon(63:6-63:7),LowerIdent(63:8-63:9),Comma(63:9-63:10),
LowerIdent(64:3-64:6),OpColon(64:6-64:7),OpenCurly(64:8-64:9),
LowerIdent(65:4-65:8),OpColon(65:8-65:9),LowerIdent(65:10-65:11),Comma(65:11-65:12),
LowerIdent(66:4-66:8),OpColon(66:8-66:9),LowerIdent(66:10-66:11),Comma(66:11-66:12),
CloseCurly(67:3-67:4),Comma(67:4-67:5),
CloseCurly(68:2-68:3),
LowerIdent(69:2-69:4),OpAssign(69:5-69:6),LowerIdent(69:7-69:8),NoSpaceOpenRound(69:8-69:9),
LowerIdent(70:3-70:4),Comma(70:4-70:5),
LowerIdent(71:3-71:4),Comma(71:4-71:5),
CloseRound(72:2-72:3),
LowerIdent(73:2-73:4),OpAssign(73:5-73:6),UpperIdent(73:7-73:8),NoSpaceOpenRound(73:8-73:9),
LowerIdent(74:3-74:4),Comma(74:4-74:5),
LowerIdent(75:3-75:4),Comma(75:4-75:5),
CloseRound(76:2-76:3),
LowerIdent(77:2-77:4),OpAssign(77:5-77:6),OpenSquare(77:7-77:8),
LowerIdent(78:3-78:4),Comma(78:4-78:5),
LowerIdent(79:3-79:4),Comma(79:4-79:5),
CloseSquare(80:2-80:3),
LowerIdent(81:2-81:4),OpAssign(81:5-81:6),OpenRound(81:7-81:8),
LowerIdent(82:3-82:4),Comma(82:4-82:5),
LowerIdent(83:3-83:4),Comma(83:4-83:5),
CloseRound(84:2-84:3),
KwMatch(86:2-86:7),LowerIdent(86:8-86:9),OpenCurly(86:10-86:11),
UpperIdent(87:3-87:5),NoSpaceOpenRound(87:5-87:6),
OpenRound(88:4-88:5),
LowerIdent(89:5-89:6),Comma(89:6-89:7),
LowerIdent(90:5-90:6),Comma(90:6-90:7),
CloseRound(91:4-91:5),Comma(91:5-91:6),
CloseRound(92:3-92:4),OpFatArrow(92:5-92:7),LowerIdent(92:8-92:9),
UpperIdent(93:3-93:5),NoSpaceOpenRound(93:5-93:6),
LowerIdent(94:4-94:5),Comma(94:5-94:6),
LowerIdent(95:4-95:5),Comma(95:5-95:6),
CloseRound(96:3-96:4),OpFatArrow(96:5-96:7),LowerIdent(96:8-96:9),
UpperIdent(97:3-97:5),NoSpaceOpenRound(97:5-97:6),
OpenCurly(98:4-98:5),
LowerIdent(99:5-99:6),Comma(99:6-99:7),
LowerIdent(100:5-100:6),Comma(100:6-100:7),
CloseCurly(101:4-101:5),Comma(101:5-101:6),
CloseRound(102:3-102:4),OpFatArrow(102:5-102:7),LowerIdent(102:8-102:9),
UpperIdent(103:3-103:5),NoSpaceOpenRound(103:5-103:6),
OpenSquare(104:4-104:5),
LowerIdent(105:5-105:6),Comma(105:6-105:7),
LowerIdent(106:5-106:6),Comma(106:6-106:7),
CloseSquare(107:4-107:5),Comma(107:5-107:6),
CloseRound(108:3-108:4),OpFatArrow(108:5-108:7),LowerIdent(108:8-108:9),
CloseCurly(109:2-109:3),
CloseCurly(110:1-110:2),EndOfFile(110:2-110:2),
~~~
# PARSE
~~~clojure
(file @1.1-110.2
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-import @4.1-7.2 (raw "I1")
			(exposing
				(exposed-upper-ident @5.2-5.5 (text "I11"))
				(exposed-upper-ident @6.2-6.5 (text "I12"))))
		(s-import @8.1-11.2 (raw "I2")
			(exposing
				(exposed-upper-ident @9.2-9.13 (text "I21") (as "Ias1"))
				(exposed-upper-ident @10.2-10.13 (text "I22") (as "Ias2"))))
		(s-type-decl @14.1-23.11
			(header @14.1-14.5 (name "A")
				(args
					(ty-var @14.3-14.4 (raw "a"))))
			(ty-var @14.8-14.9 (raw "a"))
			(where
				(method @16.3-19.11 (module-of "a") (name "a1")
					(args
						(ty-tuple @16.18-19.4
							(ty-var @17.4-17.5 (raw "a"))
							(ty-var @18.4-18.5 (raw "a"))))
					(ty @19.8-19.11 (name "Str")))
				(method @20.3-23.11 (module-of "a") (name "a2")
					(args
						(ty-tuple @20.18-23.4
							(ty-var @21.4-21.5 (raw "a"))
							(ty-var @22.4-22.5 (raw "a"))))
					(ty @23.8-23.11 (name "Str")))))
		(s-type-decl @24.1-33.11
			(header @24.1-24.5 (name "B")
				(args
					(ty-var @24.3-24.4 (raw "b"))))
			(ty-var @24.8-24.9 (raw "b"))
			(where
				(method @26.3-29.11 (module-of "b") (name "b1")
					(args
						(ty-tuple @26.18-29.4
							(ty-var @27.4-27.5 (raw "b"))
							(ty-var @28.4-28.5 (raw "b"))))
					(ty @29.8-29.11 (name "Str")))
				(method @30.3-33.11 (module-of "b") (name "b2")
					(args
						(ty-tuple @30.18-33.4
							(ty-var @31.4-31.5 (raw "b"))
							(ty-var @32.4-32.5 (raw "b"))))
					(ty @33.8-33.11 (name "Str")))))
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
		(s-type-anno @58.1-58.42 (name "g")
			(ty-fn @58.5-58.11
				(ty-var @58.5-58.6 (raw "e"))
				(ty-var @58.10-58.11 (raw "e")))
			(where
				(alias @58.18-58.29 (module-of "e") (name "A"))
				(alias @58.31-58.42 (module-of "e") (name "B"))))
		(s-decl @60.1-110.2
			(p-ident @60.1-60.2 (raw "h"))
			(e-lambda @60.5-110.2
				(args
					(p-ident @60.6-60.7 (raw "x"))
					(p-ident @60.9-60.10 (raw "y")))
				(e-block @60.12-110.2
					(statements
						(s-decl @61.2-68.3
							(p-ident @61.2-61.4 (raw "h1"))
							(e-record @61.7-68.3
								(field (field "h11")
									(e-ident @62.8-62.9 (raw "x")))
								(field (field "h12")
									(e-ident @63.8-63.9 (raw "x")))
								(field (field "h13")
									(e-record @64.8-67.4
										(field (field "h131")
											(e-ident @65.10-65.11 (raw "x")))
										(field (field "h132")
											(e-ident @66.10-66.11 (raw "y")))))))
						(s-decl @69.2-72.3
							(p-ident @69.2-69.4 (raw "h2"))
							(e-apply @69.7-72.3
								(e-ident @69.7-69.8 (raw "h"))
								(e-ident @70.3-70.4 (raw "x"))
								(e-ident @71.3-71.4 (raw "y"))))
						(s-decl @73.2-76.3
							(p-ident @73.2-73.4 (raw "h3"))
							(e-apply @73.7-76.3
								(e-tag @73.7-73.8 (raw "A"))
								(e-ident @74.3-74.4 (raw "x"))
								(e-ident @75.3-75.4 (raw "y"))))
						(s-decl @77.2-80.3
							(p-ident @77.2-77.4 (raw "h4"))
							(e-list @77.7-80.3
								(e-ident @78.3-78.4 (raw "x"))
								(e-ident @79.3-79.4 (raw "y"))))
						(s-decl @81.2-84.3
							(p-ident @81.2-81.4 (raw "h5"))
							(e-tuple @81.7-84.3
								(e-ident @82.3-82.4 (raw "x"))
								(e-ident @83.3-83.4 (raw "y"))))
						(e-match
							(e-ident @86.8-86.9 (raw "x"))
							(branches
								(branch @87.3-92.9
									(p-tag @87.3-92.4 (raw "Z1")
										(p-tuple @88.4-91.5
											(p-ident @89.5-89.6 (raw "a"))
											(p-ident @90.5-90.6 (raw "b"))))
									(e-ident @92.8-92.9 (raw "a")))
								(branch @93.3-96.9
									(p-tag @93.3-96.4 (raw "Z2")
										(p-ident @94.4-94.5 (raw "a"))
										(p-ident @95.4-95.5 (raw "b")))
									(e-ident @96.8-96.9 (raw "a")))
								(branch @97.3-102.9
									(p-tag @97.3-102.4 (raw "Z3")
										(p-record @98.4-101.5
											(field @99.5-99.6 (name "a") (rest false))
											(field @100.5-100.6 (name "b") (rest false))))
									(e-ident @102.8-102.9 (raw "a")))
								(branch @103.3-108.9
									(p-tag @103.3-108.4 (raw "Z4")
										(p-list @104.4-107.5
											(p-ident @105.5-105.6 (raw "a"))
											(p-ident @106.5-106.6 (raw "b"))))
									(e-ident @108.8-108.9 (raw "a")))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @60.1-60.2 (ident "h"))
		(e-lambda @60.5-110.2
			(args
				(p-assign @60.6-60.7 (ident "x"))
				(p-assign @60.9-60.10 (ident "y")))
			(captures
				(capture @60.1-60.2 (ident "h")))
			(e-block @60.12-110.2
				(s-let @61.2-68.3
					(p-assign @61.2-61.4 (ident "h1"))
					(e-record @61.7-68.3
						(fields
							(field (name "h11")
								(e-lookup-local @62.8-62.9
									(p-assign @60.6-60.7 (ident "x"))))
							(field (name "h12")
								(e-lookup-local @63.8-63.9
									(p-assign @60.6-60.7 (ident "x"))))
							(field (name "h13")
								(e-record @64.8-67.4
									(fields
										(field (name "h131")
											(e-lookup-local @65.10-65.11
												(p-assign @60.6-60.7 (ident "x"))))
										(field (name "h132")
											(e-lookup-local @66.10-66.11
												(p-assign @60.9-60.10 (ident "y"))))))))))
				(s-let @69.2-72.3
					(p-assign @69.2-69.4 (ident "h2"))
					(e-call @69.7-72.3
						(e-lookup-local @69.7-69.8
							(p-assign @60.1-60.2 (ident "h")))
						(e-lookup-local @70.3-70.4
							(p-assign @60.6-60.7 (ident "x")))
						(e-lookup-local @71.3-71.4
							(p-assign @60.9-60.10 (ident "y")))))
				(s-let @73.2-76.3
					(p-assign @73.2-73.4 (ident "h3"))
					(e-tag @73.7-73.8 (name "A")
						(args
							(e-lookup-local @74.3-74.4
								(p-assign @60.6-60.7 (ident "x")))
							(e-lookup-local @75.3-75.4
								(p-assign @60.9-60.10 (ident "y"))))))
				(s-let @77.2-80.3
					(p-assign @77.2-77.4 (ident "h4"))
					(e-list @77.7-80.3
						(elems
							(e-lookup-local @78.3-78.4
								(p-assign @60.6-60.7 (ident "x")))
							(e-lookup-local @79.3-79.4
								(p-assign @60.9-60.10 (ident "y"))))))
				(s-let @81.2-84.3
					(p-assign @81.2-81.4 (ident "h5"))
					(e-tuple @81.7-84.3
						(elems
							(e-lookup-local @82.3-82.4
								(p-assign @60.6-60.7 (ident "x")))
							(e-lookup-local @83.3-83.4
								(p-assign @60.9-60.10 (ident "y"))))))
				(e-match @86.2-109.3
					(match @86.2-109.3
						(cond
							(e-lookup-local @86.8-86.9
								(p-assign @60.6-60.7 (ident "x"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @87.3-92.4)))
								(value
									(e-lookup-local @92.8-92.9
										(p-assign @89.5-89.6 (ident "a")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @93.3-96.4)))
								(value
									(e-lookup-local @96.8-96.9
										(p-assign @94.4-94.5 (ident "a")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @97.3-102.4)))
								(value
									(e-lookup-local @102.8-102.9
										(p-assign @99.5-99.6 (ident "a")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @103.3-108.4)))
								(value
									(e-lookup-local @108.8-108.9
										(p-assign @105.5-105.6 (ident "a")))))))))))
	(s-alias-decl @14.1-23.11
		(ty-header @14.1-14.5 (name "A")
			(ty-args
				(ty-var @14.3-14.4 (name "a"))))
		(ty-var @14.8-14.9 (name "a"))
		(where
			(method @16.3-19.11 (module-of "a") (ident "a1")
				(args
					(ty-tuple @16.18-19.4
						(ty-var @17.4-17.5 (name "a"))
						(ty-var @18.4-18.5 (name "a"))))
				(ty @19.8-19.11 (name "Str")))
			(method @20.3-23.11 (module-of "a") (ident "a2")
				(args
					(ty-tuple @20.18-23.4
						(ty-var @21.4-21.5 (name "a"))
						(ty-var @22.4-22.5 (name "a"))))
				(ty @23.8-23.11 (name "Str")))))
	(s-alias-decl @24.1-33.11
		(ty-header @24.1-24.5 (name "B")
			(ty-args
				(ty-var @24.3-24.4 (name "b"))))
		(ty-var @24.8-24.9 (name "b"))
		(where
			(method @26.3-29.11 (module-of "b") (ident "b1")
				(args
					(ty-tuple @26.18-29.4
						(ty-var @27.4-27.5 (name "b"))
						(ty-var @28.4-28.5 (name "b"))))
				(ty @29.8-29.11 (name "Str")))
			(method @30.3-33.11 (module-of "b") (ident "b2")
				(args
					(ty-tuple @30.18-33.4
						(ty-var @31.4-31.5 (name "b"))
						(ty-var @32.4-32.5 (name "b"))))
				(ty @33.8-33.11 (name "Str")))))
	(s-alias-decl @35.1-41.2
		(ty-header @35.1-38.2 (name "C")
			(ty-args
				(ty-var @36.2-36.3 (name "a"))
				(ty-var @37.2-37.3 (name "b"))))
		(ty-tuple @38.5-41.2
			(ty-var @39.2-39.3 (name "a"))
			(ty-var @40.2-40.3 (name "b"))))
	(s-alias-decl @42.1-48.2
		(ty-header @42.1-45.2 (name "D")
			(ty-args
				(ty-var @43.2-43.3 (name "a"))
				(ty-var @44.2-44.3 (name "b"))))
		(ty-apply @45.5-48.2 (symbol "C")
			(ty-var @46.2-46.3 (name "a"))
			(ty-var @47.2-47.3 (name "b"))))
	(s-alias-decl @49.1-52.2
		(ty-header @49.1-49.2 (name "E"))
		(ty-record @49.5-52.2
			(field (field "a")
				(ty @50.6-50.9 (name "Str")))
			(field (field "b")
				(ty @51.6-51.9 (name "Str")))))
	(s-alias-decl @53.1-56.2
		(ty-header @53.1-53.2 (name "F"))
		(ty-tag-union @53.5-56.2
			(ty @54.2-54.3 (name "A"))
			(ty @55.2-55.3 (name "B"))))
	(s-import @4.1-7.2 (module "I1")
		(exposes
			(exposed (name "I11") (wildcard false))
			(exposed (name "I12") (wildcard false))))
	(s-import @8.1-11.2 (module "I2")
		(exposes
			(exposed (name "I21") (alias "Ias1") (wildcard false))
			(exposed (name "I22") (alias "Ias2") (wildcard false))))
	(s-type-anno @58.1-58.42 (name "g")
		(ty-fn @58.5-58.11 (effectful false)
			(ty-var @58.5-58.6 (name "e"))
			(ty-var @58.10-58.11 (name "e")))
		(where
			(alias @58.18-58.29 (module-of "e") (ident "A"))
			(alias @58.31-58.42 (module-of "e") (ident "B"))))
	(ext-decl @16.3-19.11 (ident "module(a).a1") (kind "value"))
	(ext-decl @20.3-23.11 (ident "module(a).a2") (kind "value"))
	(ext-decl @26.3-29.11 (ident "module(b).b1") (kind "value"))
	(ext-decl @30.3-33.11 (ident "module(b).b2") (kind "value"))
	(ext-decl @58.18-58.29 (ident "module(e).A") (kind "type"))
	(ext-decl @58.31-58.42 (ident "module(e).B") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @60.1-60.2 (type "[Z1((field, field2)), Z2(c, d), Z3(f), Z4(List(elem))]others, [Z1((field3, field4)), Z2(i, j), Z3(k), Z4(List(elem2))]others2 -> _ret")))
	(type_decls
		(alias @14.1-23.11 (type "A(a)")
			(ty-header @14.1-14.5 (name "A")
				(ty-args
					(ty-var @14.3-14.4 (name "a")))))
		(alias @24.1-33.11 (type "B(b)")
			(ty-header @24.1-24.5 (name "B")
				(ty-args
					(ty-var @24.3-24.4 (name "b")))))
		(alias @35.1-41.2 (type "C(a, b)")
			(ty-header @35.1-38.2 (name "C")
				(ty-args
					(ty-var @36.2-36.3 (name "a"))
					(ty-var @37.2-37.3 (name "b")))))
		(alias @42.1-48.2 (type "D(a, b)")
			(ty-header @42.1-45.2 (name "D")
				(ty-args
					(ty-var @43.2-43.3 (name "a"))
					(ty-var @44.2-44.3 (name "b")))))
		(alias @49.1-52.2 (type "E")
			(ty-header @49.1-49.2 (name "E")))
		(alias @53.1-56.2 (type "F")
			(ty-header @53.1-53.2 (name "F"))))
	(expressions
		(expr @60.5-110.2 (type "[Z1((field, field2)), Z2(c, d), Z3(f), Z4(List(elem))]others, [Z1((field3, field4)), Z2(i, j), Z3(k), Z4(List(elem2))]others2 -> _ret"))))
~~~
