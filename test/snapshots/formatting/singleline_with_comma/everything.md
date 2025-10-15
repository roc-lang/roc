# META
~~~ini
description=Singleline with comma formatting everything
type=snippet
~~~
# SOURCE
~~~roc
# Import exposing
import I1 exposing [I11, I12,]
import I2 exposing [I21 as Ias1, I22 as Ias2,]

# Where constraint
A(a) : a where [a.a1 : (a, a,) -> Str, a.a2 : (a, a,) -> Str,]
B(b) : b where [b.b1 : (b, b,) -> Str, b.b2 : (b, b,) -> Str,]

C(a, b,) : (a, b,)
D(a, b,) : C(a, b,)
E : { a : Str, b : Str, }
F : [A, B,]

g : e -> e where [e.A, e.B,]

h = |x, y,| {
	h1 = { h11: x, h12: x, h13: { h131: x, h132: y, }, }
	h2 = h(x, y,)
	h3 = A(x, y,)
	h4 = [x, y,]
	h5 = (x, y,)

	match x {
		Z1((a, b,)) => a
		Z2(a, b,) => a
		Z3({ a, b, }) => a
		Z4([a, b,]) => a
	}
}
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:6:1:6:63
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:7:1:7:63
MODULE NOT FOUND - everything.md:2:1:2:31
MODULE NOT FOUND - everything.md:3:1:3:47
UNUSED VARIABLE - everything.md:24:10:24:11
UNUSED VARIABLE - everything.md:25:9:25:10
UNUSED VARIABLE - everything.md:26:11:26:12
UNUSED VARIABLE - everything.md:27:10:27:11
UNUSED VARIABLE - everything.md:17:2:17:4
UNUSED VARIABLE - everything.md:18:2:18:4
UNUSED VARIABLE - everything.md:19:2:19:4
UNUSED VARIABLE - everything.md:20:2:20:4
UNUSED VARIABLE - everything.md:21:2:21:4
# PROBLEMS
**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**everything.md:6:1:6:63:**
```roc
A(a) : a where [a.a1 : (a, a,) -> Str, a.a2 : (a, a,) -> Str,]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**everything.md:7:1:7:63:**
```roc
B(b) : b where [b.b1 : (b, b,) -> Str, b.b2 : (b, b,) -> Str,]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `I1` was not found in this Roc project.

You're attempting to use this module here:
**everything.md:2:1:2:31:**
```roc
import I1 exposing [I11, I12,]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `I2` was not found in this Roc project.

You're attempting to use this module here:
**everything.md:3:1:3:47:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2,]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:24:10:24:11:**
```roc
		Z1((a, b,)) => a
```
		       ^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:25:9:25:10:**
```roc
		Z2(a, b,) => a
```
		      ^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:26:11:26:12:**
```roc
		Z3({ a, b, }) => a
```
		        ^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:27:10:27:11:**
```roc
		Z4([a, b,]) => a
```
		       ^


**UNUSED VARIABLE**
Variable `h1` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h1` to suppress this warning.
The unused variable is declared here:
**everything.md:17:2:17:4:**
```roc
	h1 = { h11: x, h12: x, h13: { h131: x, h132: y, }, }
```
	^^


**UNUSED VARIABLE**
Variable `h2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h2` to suppress this warning.
The unused variable is declared here:
**everything.md:18:2:18:4:**
```roc
	h2 = h(x, y,)
```
	^^


**UNUSED VARIABLE**
Variable `h3` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h3` to suppress this warning.
The unused variable is declared here:
**everything.md:19:2:19:4:**
```roc
	h3 = A(x, y,)
```
	^^


**UNUSED VARIABLE**
Variable `h4` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h4` to suppress this warning.
The unused variable is declared here:
**everything.md:20:2:20:4:**
```roc
	h4 = [x, y,]
```
	^^


**UNUSED VARIABLE**
Variable `h5` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h5` to suppress this warning.
The unused variable is declared here:
**everything.md:21:2:21:4:**
```roc
	h5 = (x, y,)
```
	^^


# TOKENS
~~~zig
KwImport(2:1-2:7),UpperIdent(2:8-2:10),KwExposing(2:11-2:19),OpenSquare(2:20-2:21),UpperIdent(2:21-2:24),Comma(2:24-2:25),UpperIdent(2:26-2:29),Comma(2:29-2:30),CloseSquare(2:30-2:31),
KwImport(3:1-3:7),UpperIdent(3:8-3:10),KwExposing(3:11-3:19),OpenSquare(3:20-3:21),UpperIdent(3:21-3:24),KwAs(3:25-3:27),UpperIdent(3:28-3:32),Comma(3:32-3:33),UpperIdent(3:34-3:37),KwAs(3:38-3:40),UpperIdent(3:41-3:45),Comma(3:45-3:46),CloseSquare(3:46-3:47),
UpperIdent(6:1-6:2),NoSpaceOpenRound(6:2-6:3),LowerIdent(6:3-6:4),CloseRound(6:4-6:5),OpColon(6:6-6:7),LowerIdent(6:8-6:9),KwWhere(6:10-6:15),OpenSquare(6:16-6:17),LowerIdent(6:17-6:18),NoSpaceDotLowerIdent(6:18-6:21),OpColon(6:22-6:23),OpenRound(6:24-6:25),LowerIdent(6:25-6:26),Comma(6:26-6:27),LowerIdent(6:28-6:29),Comma(6:29-6:30),CloseRound(6:30-6:31),OpArrow(6:32-6:34),UpperIdent(6:35-6:38),Comma(6:38-6:39),LowerIdent(6:40-6:41),NoSpaceDotLowerIdent(6:41-6:44),OpColon(6:45-6:46),OpenRound(6:47-6:48),LowerIdent(6:48-6:49),Comma(6:49-6:50),LowerIdent(6:51-6:52),Comma(6:52-6:53),CloseRound(6:53-6:54),OpArrow(6:55-6:57),UpperIdent(6:58-6:61),Comma(6:61-6:62),CloseSquare(6:62-6:63),
UpperIdent(7:1-7:2),NoSpaceOpenRound(7:2-7:3),LowerIdent(7:3-7:4),CloseRound(7:4-7:5),OpColon(7:6-7:7),LowerIdent(7:8-7:9),KwWhere(7:10-7:15),OpenSquare(7:16-7:17),LowerIdent(7:17-7:18),NoSpaceDotLowerIdent(7:18-7:21),OpColon(7:22-7:23),OpenRound(7:24-7:25),LowerIdent(7:25-7:26),Comma(7:26-7:27),LowerIdent(7:28-7:29),Comma(7:29-7:30),CloseRound(7:30-7:31),OpArrow(7:32-7:34),UpperIdent(7:35-7:38),Comma(7:38-7:39),LowerIdent(7:40-7:41),NoSpaceDotLowerIdent(7:41-7:44),OpColon(7:45-7:46),OpenRound(7:47-7:48),LowerIdent(7:48-7:49),Comma(7:49-7:50),LowerIdent(7:51-7:52),Comma(7:52-7:53),CloseRound(7:53-7:54),OpArrow(7:55-7:57),UpperIdent(7:58-7:61),Comma(7:61-7:62),CloseSquare(7:62-7:63),
UpperIdent(9:1-9:2),NoSpaceOpenRound(9:2-9:3),LowerIdent(9:3-9:4),Comma(9:4-9:5),LowerIdent(9:6-9:7),Comma(9:7-9:8),CloseRound(9:8-9:9),OpColon(9:10-9:11),OpenRound(9:12-9:13),LowerIdent(9:13-9:14),Comma(9:14-9:15),LowerIdent(9:16-9:17),Comma(9:17-9:18),CloseRound(9:18-9:19),
UpperIdent(10:1-10:2),NoSpaceOpenRound(10:2-10:3),LowerIdent(10:3-10:4),Comma(10:4-10:5),LowerIdent(10:6-10:7),Comma(10:7-10:8),CloseRound(10:8-10:9),OpColon(10:10-10:11),UpperIdent(10:12-10:13),NoSpaceOpenRound(10:13-10:14),LowerIdent(10:14-10:15),Comma(10:15-10:16),LowerIdent(10:17-10:18),Comma(10:18-10:19),CloseRound(10:19-10:20),
UpperIdent(11:1-11:2),OpColon(11:3-11:4),OpenCurly(11:5-11:6),LowerIdent(11:7-11:8),OpColon(11:9-11:10),UpperIdent(11:11-11:14),Comma(11:14-11:15),LowerIdent(11:16-11:17),OpColon(11:18-11:19),UpperIdent(11:20-11:23),Comma(11:23-11:24),CloseCurly(11:25-11:26),
UpperIdent(12:1-12:2),OpColon(12:3-12:4),OpenSquare(12:5-12:6),UpperIdent(12:6-12:7),Comma(12:7-12:8),UpperIdent(12:9-12:10),Comma(12:10-12:11),CloseSquare(12:11-12:12),
LowerIdent(14:1-14:2),OpColon(14:3-14:4),LowerIdent(14:5-14:6),OpArrow(14:7-14:9),LowerIdent(14:10-14:11),KwWhere(14:12-14:17),OpenSquare(14:18-14:19),LowerIdent(14:19-14:20),NoSpaceDotUpperIdent(14:20-14:22),Comma(14:22-14:23),LowerIdent(14:24-14:25),NoSpaceDotUpperIdent(14:25-14:27),Comma(14:27-14:28),CloseSquare(14:28-14:29),
LowerIdent(16:1-16:2),OpAssign(16:3-16:4),OpBar(16:5-16:6),LowerIdent(16:6-16:7),Comma(16:7-16:8),LowerIdent(16:9-16:10),Comma(16:10-16:11),OpBar(16:11-16:12),OpenCurly(16:13-16:14),
LowerIdent(17:2-17:4),OpAssign(17:5-17:6),OpenCurly(17:7-17:8),LowerIdent(17:9-17:12),OpColon(17:12-17:13),LowerIdent(17:14-17:15),Comma(17:15-17:16),LowerIdent(17:17-17:20),OpColon(17:20-17:21),LowerIdent(17:22-17:23),Comma(17:23-17:24),LowerIdent(17:25-17:28),OpColon(17:28-17:29),OpenCurly(17:30-17:31),LowerIdent(17:32-17:36),OpColon(17:36-17:37),LowerIdent(17:38-17:39),Comma(17:39-17:40),LowerIdent(17:41-17:45),OpColon(17:45-17:46),LowerIdent(17:47-17:48),Comma(17:48-17:49),CloseCurly(17:50-17:51),Comma(17:51-17:52),CloseCurly(17:53-17:54),
LowerIdent(18:2-18:4),OpAssign(18:5-18:6),LowerIdent(18:7-18:8),NoSpaceOpenRound(18:8-18:9),LowerIdent(18:9-18:10),Comma(18:10-18:11),LowerIdent(18:12-18:13),Comma(18:13-18:14),CloseRound(18:14-18:15),
LowerIdent(19:2-19:4),OpAssign(19:5-19:6),UpperIdent(19:7-19:8),NoSpaceOpenRound(19:8-19:9),LowerIdent(19:9-19:10),Comma(19:10-19:11),LowerIdent(19:12-19:13),Comma(19:13-19:14),CloseRound(19:14-19:15),
LowerIdent(20:2-20:4),OpAssign(20:5-20:6),OpenSquare(20:7-20:8),LowerIdent(20:8-20:9),Comma(20:9-20:10),LowerIdent(20:11-20:12),Comma(20:12-20:13),CloseSquare(20:13-20:14),
LowerIdent(21:2-21:4),OpAssign(21:5-21:6),OpenRound(21:7-21:8),LowerIdent(21:8-21:9),Comma(21:9-21:10),LowerIdent(21:11-21:12),Comma(21:12-21:13),CloseRound(21:13-21:14),
KwMatch(23:2-23:7),LowerIdent(23:8-23:9),OpenCurly(23:10-23:11),
UpperIdent(24:3-24:5),NoSpaceOpenRound(24:5-24:6),NoSpaceOpenRound(24:6-24:7),LowerIdent(24:7-24:8),Comma(24:8-24:9),LowerIdent(24:10-24:11),Comma(24:11-24:12),CloseRound(24:12-24:13),CloseRound(24:13-24:14),OpFatArrow(24:15-24:17),LowerIdent(24:18-24:19),
UpperIdent(25:3-25:5),NoSpaceOpenRound(25:5-25:6),LowerIdent(25:6-25:7),Comma(25:7-25:8),LowerIdent(25:9-25:10),Comma(25:10-25:11),CloseRound(25:11-25:12),OpFatArrow(25:13-25:15),LowerIdent(25:16-25:17),
UpperIdent(26:3-26:5),NoSpaceOpenRound(26:5-26:6),OpenCurly(26:6-26:7),LowerIdent(26:8-26:9),Comma(26:9-26:10),LowerIdent(26:11-26:12),Comma(26:12-26:13),CloseCurly(26:14-26:15),CloseRound(26:15-26:16),OpFatArrow(26:17-26:19),LowerIdent(26:20-26:21),
UpperIdent(27:3-27:5),NoSpaceOpenRound(27:5-27:6),OpenSquare(27:6-27:7),LowerIdent(27:7-27:8),Comma(27:8-27:9),LowerIdent(27:10-27:11),Comma(27:11-27:12),CloseSquare(27:12-27:13),CloseRound(27:13-27:14),OpFatArrow(27:15-27:17),LowerIdent(27:18-27:19),
CloseCurly(28:2-28:3),
CloseCurly(29:1-29:2),
EndOfFile(30:1-30:1),
~~~
# PARSE
~~~clojure
(file @2.1-29.2
	(type-module @2.1-2.7)
	(statements
		(s-import @2.1-2.31 (raw "I1")
			(exposing
				(exposed-upper-ident @2.21-2.24 (text "I11"))
				(exposed-upper-ident @2.26-2.29 (text "I12"))))
		(s-import @3.1-3.47 (raw "I2")
			(exposing
				(exposed-upper-ident @3.21-3.32 (text "I21") (as "Ias1"))
				(exposed-upper-ident @3.34-3.45 (text "I22") (as "Ias2"))))
		(s-type-decl @6.1-6.63
			(header @6.1-6.5 (name "A")
				(args
					(ty-var @6.3-6.4 (raw "a"))))
			(ty-var @6.8-6.9 (raw "a")))
		(s-type-decl @7.1-7.63
			(header @7.1-7.5 (name "B")
				(args
					(ty-var @7.3-7.4 (raw "b"))))
			(ty-var @7.8-7.9 (raw "b")))
		(s-type-decl @9.1-9.19
			(header @9.1-9.9 (name "C")
				(args
					(ty-var @9.3-9.4 (raw "a"))
					(ty-var @9.6-9.7 (raw "b"))))
			(ty-tuple @9.12-9.19
				(ty-var @9.13-9.14 (raw "a"))
				(ty-var @9.16-9.17 (raw "b"))))
		(s-type-decl @10.1-10.20
			(header @10.1-10.9 (name "D")
				(args
					(ty-var @10.3-10.4 (raw "a"))
					(ty-var @10.6-10.7 (raw "b"))))
			(ty-apply @10.12-10.20
				(ty @10.12-10.13 (name "C"))
				(ty-var @10.14-10.15 (raw "a"))
				(ty-var @10.17-10.18 (raw "b"))))
		(s-type-decl @11.1-11.26
			(header @11.1-11.2 (name "E")
				(args))
			(ty-record @11.5-11.26
				(anno-record-field @11.7-11.14 (name "a")
					(ty @11.11-11.14 (name "Str")))
				(anno-record-field @11.16-11.23 (name "b")
					(ty @11.20-11.23 (name "Str")))))
		(s-type-decl @12.1-12.12
			(header @12.1-12.2 (name "F")
				(args))
			(ty-tag-union @12.5-12.12
				(tags
					(ty @12.6-12.7 (name "A"))
					(ty @12.9-12.10 (name "B")))))
		(s-type-anno @14.1-14.29 (name "g")
			(ty-fn @14.5-14.11
				(ty-var @14.5-14.6 (raw "e"))
				(ty-var @14.10-14.11 (raw "e")))
			(where
				(alias @14.19-14.22 (module-of "e") (name "A"))
				(alias @14.24-14.27 (module-of "e") (name "B"))))
		(s-decl @16.1-29.2
			(p-ident @16.1-16.2 (raw "h"))
			(e-lambda @16.5-29.2
				(args
					(p-ident @16.6-16.7 (raw "x"))
					(p-ident @16.9-16.10 (raw "y")))
				(e-block @16.13-29.2
					(statements
						(s-decl @17.2-17.54
							(p-ident @17.2-17.4 (raw "h1"))
							(e-record @17.7-17.54
								(field (field "h11")
									(e-ident @17.14-17.15 (raw "x")))
								(field (field "h12")
									(e-ident @17.22-17.23 (raw "x")))
								(field (field "h13")
									(e-record @17.30-17.51
										(field (field "h131")
											(e-ident @17.38-17.39 (raw "x")))
										(field (field "h132")
											(e-ident @17.47-17.48 (raw "y")))))))
						(s-decl @18.2-18.15
							(p-ident @18.2-18.4 (raw "h2"))
							(e-apply @18.7-18.15
								(e-ident @18.7-18.8 (raw "h"))
								(e-ident @18.9-18.10 (raw "x"))
								(e-ident @18.12-18.13 (raw "y"))))
						(s-decl @19.2-19.15
							(p-ident @19.2-19.4 (raw "h3"))
							(e-apply @19.7-19.15
								(e-tag @19.7-19.8 (raw "A"))
								(e-ident @19.9-19.10 (raw "x"))
								(e-ident @19.12-19.13 (raw "y"))))
						(s-decl @20.2-20.14
							(p-ident @20.2-20.4 (raw "h4"))
							(e-list @20.7-20.14
								(e-ident @20.8-20.9 (raw "x"))
								(e-ident @20.11-20.12 (raw "y"))))
						(s-decl @21.2-21.14
							(p-ident @21.2-21.4 (raw "h5"))
							(e-tuple @21.7-21.14
								(e-ident @21.8-21.9 (raw "x"))
								(e-ident @21.11-21.12 (raw "y"))))
						(e-match
							(e-ident @23.8-23.9 (raw "x"))
							(branches
								(branch @24.3-24.19
									(p-tag @24.3-24.14 (raw "Z1")
										(p-tuple @24.6-24.13
											(p-ident @24.7-24.8 (raw "a"))
											(p-ident @24.10-24.11 (raw "b"))))
									(e-ident @24.18-24.19 (raw "a")))
								(branch @25.3-25.17
									(p-tag @25.3-25.12 (raw "Z2")
										(p-ident @25.6-25.7 (raw "a"))
										(p-ident @25.9-25.10 (raw "b")))
									(e-ident @25.16-25.17 (raw "a")))
								(branch @26.3-26.21
									(p-tag @26.3-26.16 (raw "Z3")
										(p-record @26.6-26.15
											(field @26.8-26.9 (name "a") (rest false))
											(field @26.11-26.12 (name "b") (rest false))))
									(e-ident @26.20-26.21 (raw "a")))
								(branch @27.3-27.19
									(p-tag @27.3-27.14 (raw "Z4")
										(p-list @27.6-27.13
											(p-ident @27.7-27.8 (raw "a"))
											(p-ident @27.10-27.11 (raw "b"))))
									(e-ident @27.18-27.19 (raw "a")))))))))))
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
		[a.a1 : (
			a,
			a,
		) -> Str,
		a.a2 : (
			a,
			a,
		) -> Str]
B(b) : b
	where
		[b.b1 : (
			b,
			b,
		) -> Str,
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
		[e.A,
		e.B]

h = |
	x,
	y,
| {
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
		(p-assign @16.1-16.2 (ident "h"))
		(e-closure @16.5-29.2
			(captures
				(capture @27.7-27.8 (ident "a"))
				(capture @26.8-26.9 (ident "a"))
				(capture @24.7-24.8 (ident "a"))
				(capture @16.1-16.2 (ident "h"))
				(capture @25.6-25.7 (ident "a")))
			(e-lambda @16.5-29.2
				(args
					(p-assign @16.6-16.7 (ident "x"))
					(p-assign @16.9-16.10 (ident "y")))
				(e-block @16.13-29.2
					(s-let @17.2-17.54
						(p-assign @17.2-17.4 (ident "h1"))
						(e-record @17.7-17.54
							(fields
								(field (name "h11")
									(e-lookup-local @17.14-17.15
										(p-assign @16.6-16.7 (ident "x"))))
								(field (name "h12")
									(e-lookup-local @17.22-17.23
										(p-assign @16.6-16.7 (ident "x"))))
								(field (name "h13")
									(e-record @17.30-17.51
										(fields
											(field (name "h131")
												(e-lookup-local @17.38-17.39
													(p-assign @16.6-16.7 (ident "x"))))
											(field (name "h132")
												(e-lookup-local @17.47-17.48
													(p-assign @16.9-16.10 (ident "y"))))))))))
					(s-let @18.2-18.15
						(p-assign @18.2-18.4 (ident "h2"))
						(e-call @18.7-18.15
							(e-lookup-local @18.7-18.8
								(p-assign @16.1-16.2 (ident "h")))
							(e-lookup-local @18.9-18.10
								(p-assign @16.6-16.7 (ident "x")))
							(e-lookup-local @18.12-18.13
								(p-assign @16.9-16.10 (ident "y")))))
					(s-let @19.2-19.15
						(p-assign @19.2-19.4 (ident "h3"))
						(e-tag @19.7-19.15 (name "A")
							(args
								(e-lookup-local @19.9-19.10
									(p-assign @16.6-16.7 (ident "x")))
								(e-lookup-local @19.12-19.13
									(p-assign @16.9-16.10 (ident "y"))))))
					(s-let @20.2-20.14
						(p-assign @20.2-20.4 (ident "h4"))
						(e-list @20.7-20.14
							(elems
								(e-lookup-local @20.8-20.9
									(p-assign @16.6-16.7 (ident "x")))
								(e-lookup-local @20.11-20.12
									(p-assign @16.9-16.10 (ident "y"))))))
					(s-let @21.2-21.14
						(p-assign @21.2-21.4 (ident "h5"))
						(e-tuple @21.7-21.14
							(elems
								(e-lookup-local @21.8-21.9
									(p-assign @16.6-16.7 (ident "x")))
								(e-lookup-local @21.11-21.12
									(p-assign @16.9-16.10 (ident "y"))))))
					(e-match @23.2-28.3
						(match @23.2-28.3
							(cond
								(e-lookup-local @23.8-23.9
									(p-assign @16.6-16.7 (ident "x"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @24.3-24.14)))
									(value
										(e-lookup-local @24.18-24.19
											(p-assign @24.7-24.8 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @25.3-25.12)))
									(value
										(e-lookup-local @25.16-25.17
											(p-assign @25.6-25.7 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @26.3-26.16)))
									(value
										(e-lookup-local @26.20-26.21
											(p-assign @26.8-26.9 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @27.3-27.14)))
									(value
										(e-lookup-local @27.18-27.19
											(p-assign @27.7-27.8 (ident "a"))))))))))))
	(s-alias-decl @6.1-6.63
		(ty-header @6.1-6.5 (name "A")
			(ty-args
				(ty-rigid-var @6.3-6.4 (name "a"))))
		(ty-rigid-var-lookup (ty-rigid-var @6.3-6.4 (name "a"))))
	(s-alias-decl @7.1-7.63
		(ty-header @7.1-7.5 (name "B")
			(ty-args
				(ty-rigid-var @7.3-7.4 (name "b"))))
		(ty-rigid-var-lookup (ty-rigid-var @7.3-7.4 (name "b"))))
	(s-alias-decl @9.1-9.19
		(ty-header @9.1-9.9 (name "C")
			(ty-args
				(ty-rigid-var @9.3-9.4 (name "a"))
				(ty-rigid-var @9.6-9.7 (name "b"))))
		(ty-tuple @9.12-9.19
			(ty-rigid-var-lookup (ty-rigid-var @9.3-9.4 (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var @9.6-9.7 (name "b")))))
	(s-alias-decl @10.1-10.20
		(ty-header @10.1-10.9 (name "D")
			(ty-args
				(ty-rigid-var @10.3-10.4 (name "a"))
				(ty-rigid-var @10.6-10.7 (name "b"))))
		(ty-apply @10.12-10.20 (name "C") (local)
			(ty-rigid-var-lookup (ty-rigid-var @10.3-10.4 (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var @10.6-10.7 (name "b")))))
	(s-alias-decl @11.1-11.26
		(ty-header @11.1-11.2 (name "E"))
		(ty-record @11.5-11.26
			(field (field "a")
				(ty-lookup @11.11-11.14 (name "Str") (builtin)))
			(field (field "b")
				(ty-lookup @11.20-11.23 (name "Str") (builtin)))))
	(s-alias-decl @12.1-12.12
		(ty-header @12.1-12.2 (name "F"))
		(ty-tag-union @12.5-12.12
			(ty-tag-name @12.6-12.7 (name "A"))
			(ty-tag-name @12.9-12.10 (name "B"))))
	(s-import @2.1-2.31 (module "I1")
		(exposes
			(exposed (name "I11") (wildcard false))
			(exposed (name "I12") (wildcard false))))
	(s-import @3.1-3.47 (module "I2")
		(exposes
			(exposed (name "I21") (alias "Ias1") (wildcard false))
			(exposed (name "I22") (alias "Ias2") (wildcard false))))
	(s-type-anno @14.1-14.29 (name "g")
		(ty-fn @14.5-14.11 (effectful false)
			(ty-rigid-var @14.5-14.6 (name "e"))
			(ty-rigid-var-lookup (ty-rigid-var @14.5-14.6 (name "e"))))
		(where
			(alias @14.19-14.22 (module-of "e") (ident "A"))
			(alias @14.24-14.27 (module-of "e") (ident "B"))))
	(ext-decl @14.19-14.22 (ident "e.A") (kind "type"))
	(ext-decl @14.24-14.27 (ident "e.B") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @16.1-16.2 (type "[Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j, [Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j -> c")))
	(type_decls
		(alias @6.1-6.63 (type "A(a)")
			(ty-header @6.1-6.5 (name "A")
				(ty-args
					(ty-rigid-var @6.3-6.4 (name "a")))))
		(alias @7.1-7.63 (type "B(b)")
			(ty-header @7.1-7.5 (name "B")
				(ty-args
					(ty-rigid-var @7.3-7.4 (name "b")))))
		(alias @9.1-9.19 (type "C(a, b)")
			(ty-header @9.1-9.9 (name "C")
				(ty-args
					(ty-rigid-var @9.3-9.4 (name "a"))
					(ty-rigid-var @9.6-9.7 (name "b")))))
		(alias @10.1-10.20 (type "D(a, b)")
			(ty-header @10.1-10.9 (name "D")
				(ty-args
					(ty-rigid-var @10.3-10.4 (name "a"))
					(ty-rigid-var @10.6-10.7 (name "b")))))
		(alias @11.1-11.26 (type "E")
			(ty-header @11.1-11.2 (name "E")))
		(alias @12.1-12.12 (type "F")
			(ty-header @12.1-12.2 (name "F"))))
	(expressions
		(expr @16.5-29.2 (type "[Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j, [Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j -> c"))))
~~~
