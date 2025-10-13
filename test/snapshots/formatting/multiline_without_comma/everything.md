# META
~~~ini
description=Multiline without comma formatting everything
type=snippet
~~~
# SOURCE
~~~roc
# Import exposing
import I1 exposing [
	I11,
	I12
]
import I2 exposing [
	I21 as Ias1,
	I22 as Ias2
]

# Where constraint
A(a) : a
	where
		module(a).a1 : (
			a,
			a
		) -> Str,
		module(a).a2 : (
			a,
			a
		) -> Str
B(b) : b
	where
		module(b).b1 : (
			b,
			b
		) -> Str,
		module(b).b2 : (
			b,
			b
		) -> Str

C(
	a,
	b
) : (
	a,
	b
)
D(
	a,
	b
) : C(
	a,
	b
)
E : {
	a : Str,
	b : Str
}
F : [
	A,
	B
]

g : e -> e where module(e).A, module(e).B

h = |x, y| {
	h1 = {
		h11: x,
		h12: x,
		h13: {
			h131: x,
			h132: y
		}
	}
	h2 = h(
		x,
		y
	)
	h3 = A(
		x,
		y
	)
	h4 = [
		x,
		y
	]
	h5 = (
		x,
		y
	)

	match x {
		Z1(
			(
				a,
				b
			)
		) => a
		Z2(
			a,
			b
		) => a
		Z3(
			{
				a,
				b
			}
		) => a
		Z4(
			[
				a,
				b
			]
		) => a
	}
}
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:12:1:21:11
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:22:1:31:11
MODULE NOT FOUND - everything.md:2:1:5:2
MODULE NOT FOUND - everything.md:6:1:9:2
UNUSED VARIABLE - everything.md:88:5:88:6
UNUSED VARIABLE - everything.md:93:4:93:5
UNUSED VARIABLE - everything.md:98:5:98:6
UNUSED VARIABLE - everything.md:104:5:104:6
UNUSED VARIABLE - everything.md:59:2:59:4
UNUSED VARIABLE - everything.md:67:2:67:4
UNUSED VARIABLE - everything.md:71:2:71:4
UNUSED VARIABLE - everything.md:75:2:75:4
UNUSED VARIABLE - everything.md:79:2:79:4
# PROBLEMS
**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**everything.md:12:1:21:11:**
```roc
A(a) : a
	where
		module(a).a1 : (
			a,
			a
		) -> Str,
		module(a).a2 : (
			a,
			a
		) -> Str
```


**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**everything.md:22:1:31:11:**
```roc
B(b) : b
	where
		module(b).b1 : (
			b,
			b
		) -> Str,
		module(b).b2 : (
			b,
			b
		) -> Str
```


**MODULE NOT FOUND**
The module `I1` was not found in this Roc project.

You're attempting to use this module here:
**everything.md:2:1:5:2:**
```roc
import I1 exposing [
	I11,
	I12
]
```


**MODULE NOT FOUND**
The module `I2` was not found in this Roc project.

You're attempting to use this module here:
**everything.md:6:1:9:2:**
```roc
import I2 exposing [
	I21 as Ias1,
	I22 as Ias2
]
```


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:88:5:88:6:**
```roc
				b
```
				^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:93:4:93:5:**
```roc
			b
```
			^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:98:5:98:6:**
```roc
				b
```
				^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:104:5:104:6:**
```roc
				b
```
				^


**UNUSED VARIABLE**
Variable `h1` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h1` to suppress this warning.
The unused variable is declared here:
**everything.md:59:2:59:4:**
```roc
	h1 = {
```
	^^


**UNUSED VARIABLE**
Variable `h2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h2` to suppress this warning.
The unused variable is declared here:
**everything.md:67:2:67:4:**
```roc
	h2 = h(
```
	^^


**UNUSED VARIABLE**
Variable `h3` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h3` to suppress this warning.
The unused variable is declared here:
**everything.md:71:2:71:4:**
```roc
	h3 = A(
```
	^^


**UNUSED VARIABLE**
Variable `h4` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h4` to suppress this warning.
The unused variable is declared here:
**everything.md:75:2:75:4:**
```roc
	h4 = [
```
	^^


**UNUSED VARIABLE**
Variable `h5` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h5` to suppress this warning.
The unused variable is declared here:
**everything.md:79:2:79:4:**
```roc
	h5 = (
```
	^^


# TOKENS
~~~zig
KwImport(2:1-2:7),UpperIdent(2:8-2:10),KwExposing(2:11-2:19),OpenSquare(2:20-2:21),
UpperIdent(3:2-3:5),Comma(3:5-3:6),
UpperIdent(4:2-4:5),
CloseSquare(5:1-5:2),
KwImport(6:1-6:7),UpperIdent(6:8-6:10),KwExposing(6:11-6:19),OpenSquare(6:20-6:21),
UpperIdent(7:2-7:5),KwAs(7:6-7:8),UpperIdent(7:9-7:13),Comma(7:13-7:14),
UpperIdent(8:2-8:5),KwAs(8:6-8:8),UpperIdent(8:9-8:13),
CloseSquare(9:1-9:2),
UpperIdent(12:1-12:2),NoSpaceOpenRound(12:2-12:3),LowerIdent(12:3-12:4),CloseRound(12:4-12:5),OpColon(12:6-12:7),LowerIdent(12:8-12:9),
KwWhere(13:2-13:7),
KwModule(14:3-14:9),NoSpaceOpenRound(14:9-14:10),LowerIdent(14:10-14:11),CloseRound(14:11-14:12),NoSpaceDotLowerIdent(14:12-14:15),OpColon(14:16-14:17),OpenRound(14:18-14:19),
LowerIdent(15:4-15:5),Comma(15:5-15:6),
LowerIdent(16:4-16:5),
CloseRound(17:3-17:4),OpArrow(17:5-17:7),UpperIdent(17:8-17:11),Comma(17:11-17:12),
KwModule(18:3-18:9),NoSpaceOpenRound(18:9-18:10),LowerIdent(18:10-18:11),CloseRound(18:11-18:12),NoSpaceDotLowerIdent(18:12-18:15),OpColon(18:16-18:17),OpenRound(18:18-18:19),
LowerIdent(19:4-19:5),Comma(19:5-19:6),
LowerIdent(20:4-20:5),
CloseRound(21:3-21:4),OpArrow(21:5-21:7),UpperIdent(21:8-21:11),
UpperIdent(22:1-22:2),NoSpaceOpenRound(22:2-22:3),LowerIdent(22:3-22:4),CloseRound(22:4-22:5),OpColon(22:6-22:7),LowerIdent(22:8-22:9),
KwWhere(23:2-23:7),
KwModule(24:3-24:9),NoSpaceOpenRound(24:9-24:10),LowerIdent(24:10-24:11),CloseRound(24:11-24:12),NoSpaceDotLowerIdent(24:12-24:15),OpColon(24:16-24:17),OpenRound(24:18-24:19),
LowerIdent(25:4-25:5),Comma(25:5-25:6),
LowerIdent(26:4-26:5),
CloseRound(27:3-27:4),OpArrow(27:5-27:7),UpperIdent(27:8-27:11),Comma(27:11-27:12),
KwModule(28:3-28:9),NoSpaceOpenRound(28:9-28:10),LowerIdent(28:10-28:11),CloseRound(28:11-28:12),NoSpaceDotLowerIdent(28:12-28:15),OpColon(28:16-28:17),OpenRound(28:18-28:19),
LowerIdent(29:4-29:5),Comma(29:5-29:6),
LowerIdent(30:4-30:5),
CloseRound(31:3-31:4),OpArrow(31:5-31:7),UpperIdent(31:8-31:11),
UpperIdent(33:1-33:2),NoSpaceOpenRound(33:2-33:3),
LowerIdent(34:2-34:3),Comma(34:3-34:4),
LowerIdent(35:2-35:3),
CloseRound(36:1-36:2),OpColon(36:3-36:4),OpenRound(36:5-36:6),
LowerIdent(37:2-37:3),Comma(37:3-37:4),
LowerIdent(38:2-38:3),
CloseRound(39:1-39:2),
UpperIdent(40:1-40:2),NoSpaceOpenRound(40:2-40:3),
LowerIdent(41:2-41:3),Comma(41:3-41:4),
LowerIdent(42:2-42:3),
CloseRound(43:1-43:2),OpColon(43:3-43:4),UpperIdent(43:5-43:6),NoSpaceOpenRound(43:6-43:7),
LowerIdent(44:2-44:3),Comma(44:3-44:4),
LowerIdent(45:2-45:3),
CloseRound(46:1-46:2),
UpperIdent(47:1-47:2),OpColon(47:3-47:4),OpenCurly(47:5-47:6),
LowerIdent(48:2-48:3),OpColon(48:4-48:5),UpperIdent(48:6-48:9),Comma(48:9-48:10),
LowerIdent(49:2-49:3),OpColon(49:4-49:5),UpperIdent(49:6-49:9),
CloseCurly(50:1-50:2),
UpperIdent(51:1-51:2),OpColon(51:3-51:4),OpenSquare(51:5-51:6),
UpperIdent(52:2-52:3),Comma(52:3-52:4),
UpperIdent(53:2-53:3),
CloseSquare(54:1-54:2),
LowerIdent(56:1-56:2),OpColon(56:3-56:4),LowerIdent(56:5-56:6),OpArrow(56:7-56:9),LowerIdent(56:10-56:11),KwWhere(56:12-56:17),KwModule(56:18-56:24),NoSpaceOpenRound(56:24-56:25),LowerIdent(56:25-56:26),CloseRound(56:26-56:27),NoSpaceDotUpperIdent(56:27-56:29),Comma(56:29-56:30),KwModule(56:31-56:37),NoSpaceOpenRound(56:37-56:38),LowerIdent(56:38-56:39),CloseRound(56:39-56:40),NoSpaceDotUpperIdent(56:40-56:42),
LowerIdent(58:1-58:2),OpAssign(58:3-58:4),OpBar(58:5-58:6),LowerIdent(58:6-58:7),Comma(58:7-58:8),LowerIdent(58:9-58:10),OpBar(58:10-58:11),OpenCurly(58:12-58:13),
LowerIdent(59:2-59:4),OpAssign(59:5-59:6),OpenCurly(59:7-59:8),
LowerIdent(60:3-60:6),OpColon(60:6-60:7),LowerIdent(60:8-60:9),Comma(60:9-60:10),
LowerIdent(61:3-61:6),OpColon(61:6-61:7),LowerIdent(61:8-61:9),Comma(61:9-61:10),
LowerIdent(62:3-62:6),OpColon(62:6-62:7),OpenCurly(62:8-62:9),
LowerIdent(63:4-63:8),OpColon(63:8-63:9),LowerIdent(63:10-63:11),Comma(63:11-63:12),
LowerIdent(64:4-64:8),OpColon(64:8-64:9),LowerIdent(64:10-64:11),
CloseCurly(65:3-65:4),
CloseCurly(66:2-66:3),
LowerIdent(67:2-67:4),OpAssign(67:5-67:6),LowerIdent(67:7-67:8),NoSpaceOpenRound(67:8-67:9),
LowerIdent(68:3-68:4),Comma(68:4-68:5),
LowerIdent(69:3-69:4),
CloseRound(70:2-70:3),
LowerIdent(71:2-71:4),OpAssign(71:5-71:6),UpperIdent(71:7-71:8),NoSpaceOpenRound(71:8-71:9),
LowerIdent(72:3-72:4),Comma(72:4-72:5),
LowerIdent(73:3-73:4),
CloseRound(74:2-74:3),
LowerIdent(75:2-75:4),OpAssign(75:5-75:6),OpenSquare(75:7-75:8),
LowerIdent(76:3-76:4),Comma(76:4-76:5),
LowerIdent(77:3-77:4),
CloseSquare(78:2-78:3),
LowerIdent(79:2-79:4),OpAssign(79:5-79:6),OpenRound(79:7-79:8),
LowerIdent(80:3-80:4),Comma(80:4-80:5),
LowerIdent(81:3-81:4),
CloseRound(82:2-82:3),
KwMatch(84:2-84:7),LowerIdent(84:8-84:9),OpenCurly(84:10-84:11),
UpperIdent(85:3-85:5),NoSpaceOpenRound(85:5-85:6),
OpenRound(86:4-86:5),
LowerIdent(87:5-87:6),Comma(87:6-87:7),
LowerIdent(88:5-88:6),
CloseRound(89:4-89:5),
CloseRound(90:3-90:4),OpFatArrow(90:5-90:7),LowerIdent(90:8-90:9),
UpperIdent(91:3-91:5),NoSpaceOpenRound(91:5-91:6),
LowerIdent(92:4-92:5),Comma(92:5-92:6),
LowerIdent(93:4-93:5),
CloseRound(94:3-94:4),OpFatArrow(94:5-94:7),LowerIdent(94:8-94:9),
UpperIdent(95:3-95:5),NoSpaceOpenRound(95:5-95:6),
OpenCurly(96:4-96:5),
LowerIdent(97:5-97:6),Comma(97:6-97:7),
LowerIdent(98:5-98:6),
CloseCurly(99:4-99:5),
CloseRound(100:3-100:4),OpFatArrow(100:5-100:7),LowerIdent(100:8-100:9),
UpperIdent(101:3-101:5),NoSpaceOpenRound(101:5-101:6),
OpenSquare(102:4-102:5),
LowerIdent(103:5-103:6),Comma(103:6-103:7),
LowerIdent(104:5-104:6),
CloseSquare(105:4-105:5),
CloseRound(106:3-106:4),OpFatArrow(106:5-106:7),LowerIdent(106:8-106:9),
CloseCurly(107:2-107:3),
CloseCurly(108:1-108:2),
EndOfFile(109:1-109:1),
~~~
# PARSE
~~~clojure
(file @2.1-108.2
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
		(s-type-decl @12.1-21.11
			(header @12.1-12.5 (name "A")
				(args
					(ty-var @12.3-12.4 (raw "a"))))
			(ty-var @12.8-12.9 (raw "a")))
		(s-type-decl @22.1-31.11
			(header @22.1-22.5 (name "B")
				(args
					(ty-var @22.3-22.4 (raw "b"))))
			(ty-var @22.8-22.9 (raw "b")))
		(s-type-decl @33.1-39.2
			(header @33.1-36.2 (name "C")
				(args
					(ty-var @34.2-34.3 (raw "a"))
					(ty-var @35.2-35.3 (raw "b"))))
			(ty-tuple @36.5-39.2
				(ty-var @37.2-37.3 (raw "a"))
				(ty-var @38.2-38.3 (raw "b"))))
		(s-type-decl @40.1-46.2
			(header @40.1-43.2 (name "D")
				(args
					(ty-var @41.2-41.3 (raw "a"))
					(ty-var @42.2-42.3 (raw "b"))))
			(ty-apply @43.5-46.2
				(ty @43.5-43.6 (name "C"))
				(ty-var @44.2-44.3 (raw "a"))
				(ty-var @45.2-45.3 (raw "b"))))
		(s-type-decl @47.1-50.2
			(header @47.1-47.2 (name "E")
				(args))
			(ty-record @47.5-50.2
				(anno-record-field @48.2-48.9 (name "a")
					(ty @48.6-48.9 (name "Str")))
				(anno-record-field @49.2-49.9 (name "b")
					(ty @49.6-49.9 (name "Str")))))
		(s-type-decl @51.1-54.2
			(header @51.1-51.2 (name "F")
				(args))
			(ty-tag-union @51.5-54.2
				(tags
					(ty @52.2-52.3 (name "A"))
					(ty @53.2-53.3 (name "B")))))
		(s-type-anno @56.1-56.42 (name "g")
			(ty-fn @56.5-56.11
				(ty-var @56.5-56.6 (raw "e"))
				(ty-var @56.10-56.11 (raw "e")))
			(where
				(alias @56.18-56.29 (module-of "e") (name "A"))
				(alias @56.31-56.42 (module-of "e") (name "B"))))
		(s-decl @58.1-108.2
			(p-ident @58.1-58.2 (raw "h"))
			(e-lambda @58.5-108.2
				(args
					(p-ident @58.6-58.7 (raw "x"))
					(p-ident @58.9-58.10 (raw "y")))
				(e-block @58.12-108.2
					(statements
						(s-decl @59.2-66.3
							(p-ident @59.2-59.4 (raw "h1"))
							(e-record @59.7-66.3
								(field (field "h11")
									(e-ident @60.8-60.9 (raw "x")))
								(field (field "h12")
									(e-ident @61.8-61.9 (raw "x")))
								(field (field "h13")
									(e-record @62.8-65.4
										(field (field "h131")
											(e-ident @63.10-63.11 (raw "x")))
										(field (field "h132")
											(e-ident @64.10-64.11 (raw "y")))))))
						(s-decl @67.2-70.3
							(p-ident @67.2-67.4 (raw "h2"))
							(e-apply @67.7-70.3
								(e-ident @67.7-67.8 (raw "h"))
								(e-ident @68.3-68.4 (raw "x"))
								(e-ident @69.3-69.4 (raw "y"))))
						(s-decl @71.2-74.3
							(p-ident @71.2-71.4 (raw "h3"))
							(e-apply @71.7-74.3
								(e-tag @71.7-71.8 (raw "A"))
								(e-ident @72.3-72.4 (raw "x"))
								(e-ident @73.3-73.4 (raw "y"))))
						(s-decl @75.2-78.3
							(p-ident @75.2-75.4 (raw "h4"))
							(e-list @75.7-78.3
								(e-ident @76.3-76.4 (raw "x"))
								(e-ident @77.3-77.4 (raw "y"))))
						(s-decl @79.2-82.3
							(p-ident @79.2-79.4 (raw "h5"))
							(e-tuple @79.7-82.3
								(e-ident @80.3-80.4 (raw "x"))
								(e-ident @81.3-81.4 (raw "y"))))
						(e-match
							(e-ident @84.8-84.9 (raw "x"))
							(branches
								(branch @85.3-90.9
									(p-tag @85.3-90.4 (raw "Z1")
										(p-tuple @86.4-89.5
											(p-ident @87.5-87.6 (raw "a"))
											(p-ident @88.5-88.6 (raw "b"))))
									(e-ident @90.8-90.9 (raw "a")))
								(branch @91.3-94.9
									(p-tag @91.3-94.4 (raw "Z2")
										(p-ident @92.4-92.5 (raw "a"))
										(p-ident @93.4-93.5 (raw "b")))
									(e-ident @94.8-94.9 (raw "a")))
								(branch @95.3-100.9
									(p-tag @95.3-100.4 (raw "Z3")
										(p-record @96.4-99.5
											(field @97.5-97.6 (name "a") (rest false))
											(field @98.5-98.6 (name "b") (rest false))))
									(e-ident @100.8-100.9 (raw "a")))
								(branch @101.3-106.9
									(p-tag @101.3-106.4 (raw "Z4")
										(p-list @102.4-105.5
											(p-ident @103.5-103.6 (raw "a"))
											(p-ident @104.5-104.6 (raw "b"))))
									(e-ident @106.8-106.9 (raw "a")))))))))))
~~~
# FORMATTED
~~~roc
# Import exposing
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
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @58.1-58.2 (ident "h"))
		(e-closure @58.5-108.2
			(captures
				(capture @97.5-97.6 (ident "a"))
				(capture @103.5-103.6 (ident "a"))
				(capture @58.1-58.2 (ident "h"))
				(capture @92.4-92.5 (ident "a"))
				(capture @87.5-87.6 (ident "a")))
			(e-lambda @58.5-108.2
				(args
					(p-assign @58.6-58.7 (ident "x"))
					(p-assign @58.9-58.10 (ident "y")))
				(e-block @58.12-108.2
					(s-let @59.2-66.3
						(p-assign @59.2-59.4 (ident "h1"))
						(e-record @59.7-66.3
							(fields
								(field (name "h11")
									(e-lookup-local @60.8-60.9
										(p-assign @58.6-58.7 (ident "x"))))
								(field (name "h12")
									(e-lookup-local @61.8-61.9
										(p-assign @58.6-58.7 (ident "x"))))
								(field (name "h13")
									(e-record @62.8-65.4
										(fields
											(field (name "h131")
												(e-lookup-local @63.10-63.11
													(p-assign @58.6-58.7 (ident "x"))))
											(field (name "h132")
												(e-lookup-local @64.10-64.11
													(p-assign @58.9-58.10 (ident "y"))))))))))
					(s-let @67.2-70.3
						(p-assign @67.2-67.4 (ident "h2"))
						(e-call @67.7-70.3
							(e-lookup-local @67.7-67.8
								(p-assign @58.1-58.2 (ident "h")))
							(e-lookup-local @68.3-68.4
								(p-assign @58.6-58.7 (ident "x")))
							(e-lookup-local @69.3-69.4
								(p-assign @58.9-58.10 (ident "y")))))
					(s-let @71.2-74.3
						(p-assign @71.2-71.4 (ident "h3"))
						(e-tag @71.7-74.3 (name "A")
							(args
								(e-lookup-local @72.3-72.4
									(p-assign @58.6-58.7 (ident "x")))
								(e-lookup-local @73.3-73.4
									(p-assign @58.9-58.10 (ident "y"))))))
					(s-let @75.2-78.3
						(p-assign @75.2-75.4 (ident "h4"))
						(e-list @75.7-78.3
							(elems
								(e-lookup-local @76.3-76.4
									(p-assign @58.6-58.7 (ident "x")))
								(e-lookup-local @77.3-77.4
									(p-assign @58.9-58.10 (ident "y"))))))
					(s-let @79.2-82.3
						(p-assign @79.2-79.4 (ident "h5"))
						(e-tuple @79.7-82.3
							(elems
								(e-lookup-local @80.3-80.4
									(p-assign @58.6-58.7 (ident "x")))
								(e-lookup-local @81.3-81.4
									(p-assign @58.9-58.10 (ident "y"))))))
					(e-match @84.2-107.3
						(match @84.2-107.3
							(cond
								(e-lookup-local @84.8-84.9
									(p-assign @58.6-58.7 (ident "x"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @85.3-90.4)))
									(value
										(e-lookup-local @90.8-90.9
											(p-assign @87.5-87.6 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @91.3-94.4)))
									(value
										(e-lookup-local @94.8-94.9
											(p-assign @92.4-92.5 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @95.3-100.4)))
									(value
										(e-lookup-local @100.8-100.9
											(p-assign @97.5-97.6 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @101.3-106.4)))
									(value
										(e-lookup-local @106.8-106.9
											(p-assign @103.5-103.6 (ident "a"))))))))))))
	(s-alias-decl @12.1-21.11
		(ty-header @12.1-12.5 (name "A")
			(ty-args
				(ty-rigid-var @12.3-12.4 (name "a"))))
		(ty-rigid-var-lookup (ty-rigid-var @12.3-12.4 (name "a"))))
	(s-alias-decl @22.1-31.11
		(ty-header @22.1-22.5 (name "B")
			(ty-args
				(ty-rigid-var @22.3-22.4 (name "b"))))
		(ty-rigid-var-lookup (ty-rigid-var @22.3-22.4 (name "b"))))
	(s-alias-decl @33.1-39.2
		(ty-header @33.1-36.2 (name "C")
			(ty-args
				(ty-rigid-var @34.2-34.3 (name "a"))
				(ty-rigid-var @35.2-35.3 (name "b"))))
		(ty-tuple @36.5-39.2
			(ty-rigid-var-lookup (ty-rigid-var @34.2-34.3 (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var @35.2-35.3 (name "b")))))
	(s-alias-decl @40.1-46.2
		(ty-header @40.1-43.2 (name "D")
			(ty-args
				(ty-rigid-var @41.2-41.3 (name "a"))
				(ty-rigid-var @42.2-42.3 (name "b"))))
		(ty-apply @43.5-46.2 (name "C") (local)
			(ty-rigid-var-lookup (ty-rigid-var @41.2-41.3 (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var @42.2-42.3 (name "b")))))
	(s-alias-decl @47.1-50.2
		(ty-header @47.1-47.2 (name "E"))
		(ty-record @47.5-50.2
			(field (field "a")
				(ty-lookup @48.6-48.9 (name "Str") (builtin)))
			(field (field "b")
				(ty-lookup @49.6-49.9 (name "Str") (builtin)))))
	(s-alias-decl @51.1-54.2
		(ty-header @51.1-51.2 (name "F"))
		(ty-tag-union @51.5-54.2
			(ty-tag-name @52.2-52.3 (name "A"))
			(ty-tag-name @53.2-53.3 (name "B"))))
	(s-import @2.1-5.2 (module "I1")
		(exposes
			(exposed (name "I11") (wildcard false))
			(exposed (name "I12") (wildcard false))))
	(s-import @6.1-9.2 (module "I2")
		(exposes
			(exposed (name "I21") (alias "Ias1") (wildcard false))
			(exposed (name "I22") (alias "Ias2") (wildcard false))))
	(s-type-anno @56.1-56.42 (name "g")
		(ty-fn @56.5-56.11 (effectful false)
			(ty-rigid-var @56.5-56.6 (name "e"))
			(ty-rigid-var-lookup (ty-rigid-var @56.5-56.6 (name "e"))))
		(where
			(alias @56.18-56.29 (module-of "e") (ident "A"))
			(alias @56.31-56.42 (module-of "e") (ident "B"))))
	(ext-decl @56.18-56.29 (ident "module(e).A") (kind "type"))
	(ext-decl @56.31-56.42 (ident "module(e).B") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @58.1-58.2 (type "[Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j, [Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j -> c")))
	(type_decls
		(alias @12.1-21.11 (type "A(a)")
			(ty-header @12.1-12.5 (name "A")
				(ty-args
					(ty-rigid-var @12.3-12.4 (name "a")))))
		(alias @22.1-31.11 (type "B(b)")
			(ty-header @22.1-22.5 (name "B")
				(ty-args
					(ty-rigid-var @22.3-22.4 (name "b")))))
		(alias @33.1-39.2 (type "C(a, b)")
			(ty-header @33.1-36.2 (name "C")
				(ty-args
					(ty-rigid-var @34.2-34.3 (name "a"))
					(ty-rigid-var @35.2-35.3 (name "b")))))
		(alias @40.1-46.2 (type "D(a, b)")
			(ty-header @40.1-43.2 (name "D")
				(ty-args
					(ty-rigid-var @41.2-41.3 (name "a"))
					(ty-rigid-var @42.2-42.3 (name "b")))))
		(alias @47.1-50.2 (type "E")
			(ty-header @47.1-47.2 (name "E")))
		(alias @51.1-54.2 (type "F")
			(ty-header @51.1-51.2 (name "F"))))
	(expressions
		(expr @58.5-108.2 (type "[Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j, [Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j -> c"))))
~~~
