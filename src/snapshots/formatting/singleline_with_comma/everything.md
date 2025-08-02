# META
~~~ini
description=Singleline with comma formatting everything
type=file
~~~
# SOURCE
~~~roc
module []

# Import exposing
import I1 exposing [I11, I12,]
import I2 exposing [I21 as Ias1, I22 as Ias2,]

# Where constraint
A(a) : a where module(a).a1 : (a, a,) -> Str, module(a).a2 : (a, a,) -> Str,
B(b) : b where module(b).b1 : (b, b,) -> Str, module(b).b2 : (b, b,) -> Str,

C(a, b,) : (a, b,)
D(a, b,) : C(a, b,)
E : { a : Str, b : Str, }
F : [A, B,]

g : e -> e where module(e).A, module(e).B,

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
MODULE NOT FOUND - everything.md:4:1:4:31
MODULE NOT FOUND - everything.md:5:1:5:47
UNUSED VARIABLE - everything.md:26:10:26:11
UNUSED VARIABLE - everything.md:27:9:27:10
UNUSED VARIABLE - everything.md:28:11:28:12
UNUSED VARIABLE - everything.md:29:10:29:11
UNUSED VARIABLE - everything.md:19:2:19:4
UNUSED VARIABLE - everything.md:20:2:20:4
UNUSED VARIABLE - everything.md:21:2:21:4
UNUSED VARIABLE - everything.md:22:2:22:4
UNUSED VARIABLE - everything.md:23:2:23:4
# PROBLEMS
**MODULE NOT FOUND**
The module `I1` was not found in this Roc project.

You're attempting to use this module here:
**everything.md:4:1:4:31:**
```roc
import I1 exposing [I11, I12,]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `I2` was not found in this Roc project.

You're attempting to use this module here:
**everything.md:5:1:5:47:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2,]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:26:10:26:11:**
```roc
		Z1((a, b,)) => a
```
         ^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:27:9:27:10:**
```roc
		Z2(a, b,) => a
```
        ^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:28:11:28:12:**
```roc
		Z3({ a, b, }) => a
```
          ^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:29:10:29:11:**
```roc
		Z4([a, b,]) => a
```
         ^


**UNUSED VARIABLE**
Variable `h1` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h1` to suppress this warning.
The unused variable is declared here:
**everything.md:19:2:19:4:**
```roc
	h1 = { h11: x, h12: x, h13: { h131: x, h132: y, }, }
```
 ^^


**UNUSED VARIABLE**
Variable `h4` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h4` to suppress this warning.
The unused variable is declared here:
**everything.md:22:2:22:4:**
```roc
	h4 = [x, y,]
```
 ^^


**UNUSED VARIABLE**
Variable `h2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h2` to suppress this warning.
The unused variable is declared here:
**everything.md:20:2:20:4:**
```roc
	h2 = h(x, y,)
```
 ^^


**UNUSED VARIABLE**
Variable `h3` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h3` to suppress this warning.
The unused variable is declared here:
**everything.md:21:2:21:4:**
```roc
	h3 = A(x, y,)
```
 ^^


**UNUSED VARIABLE**
Variable `h5` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h5` to suppress this warning.
The unused variable is declared here:
**everything.md:23:2:23:4:**
```roc
	h5 = (x, y,)
```
 ^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
KwImport(4:1-4:7),UpperIdent(4:8-4:10),KwExposing(4:11-4:19),OpenSquare(4:20-4:21),UpperIdent(4:21-4:24),Comma(4:24-4:25),UpperIdent(4:26-4:29),Comma(4:29-4:30),CloseSquare(4:30-4:31),
KwImport(5:1-5:7),UpperIdent(5:8-5:10),KwExposing(5:11-5:19),OpenSquare(5:20-5:21),UpperIdent(5:21-5:24),KwAs(5:25-5:27),UpperIdent(5:28-5:32),Comma(5:32-5:33),UpperIdent(5:34-5:37),KwAs(5:38-5:40),UpperIdent(5:41-5:45),Comma(5:45-5:46),CloseSquare(5:46-5:47),
UpperIdent(8:1-8:2),NoSpaceOpenRound(8:2-8:3),LowerIdent(8:3-8:4),CloseRound(8:4-8:5),OpColon(8:6-8:7),LowerIdent(8:8-8:9),KwWhere(8:10-8:15),KwModule(8:16-8:22),NoSpaceOpenRound(8:22-8:23),LowerIdent(8:23-8:24),CloseRound(8:24-8:25),NoSpaceDotLowerIdent(8:25-8:28),OpColon(8:29-8:30),OpenRound(8:31-8:32),LowerIdent(8:32-8:33),Comma(8:33-8:34),LowerIdent(8:35-8:36),Comma(8:36-8:37),CloseRound(8:37-8:38),OpArrow(8:39-8:41),UpperIdent(8:42-8:45),Comma(8:45-8:46),KwModule(8:47-8:53),NoSpaceOpenRound(8:53-8:54),LowerIdent(8:54-8:55),CloseRound(8:55-8:56),NoSpaceDotLowerIdent(8:56-8:59),OpColon(8:60-8:61),OpenRound(8:62-8:63),LowerIdent(8:63-8:64),Comma(8:64-8:65),LowerIdent(8:66-8:67),Comma(8:67-8:68),CloseRound(8:68-8:69),OpArrow(8:70-8:72),UpperIdent(8:73-8:76),Comma(8:76-8:77),
UpperIdent(9:1-9:2),NoSpaceOpenRound(9:2-9:3),LowerIdent(9:3-9:4),CloseRound(9:4-9:5),OpColon(9:6-9:7),LowerIdent(9:8-9:9),KwWhere(9:10-9:15),KwModule(9:16-9:22),NoSpaceOpenRound(9:22-9:23),LowerIdent(9:23-9:24),CloseRound(9:24-9:25),NoSpaceDotLowerIdent(9:25-9:28),OpColon(9:29-9:30),OpenRound(9:31-9:32),LowerIdent(9:32-9:33),Comma(9:33-9:34),LowerIdent(9:35-9:36),Comma(9:36-9:37),CloseRound(9:37-9:38),OpArrow(9:39-9:41),UpperIdent(9:42-9:45),Comma(9:45-9:46),KwModule(9:47-9:53),NoSpaceOpenRound(9:53-9:54),LowerIdent(9:54-9:55),CloseRound(9:55-9:56),NoSpaceDotLowerIdent(9:56-9:59),OpColon(9:60-9:61),OpenRound(9:62-9:63),LowerIdent(9:63-9:64),Comma(9:64-9:65),LowerIdent(9:66-9:67),Comma(9:67-9:68),CloseRound(9:68-9:69),OpArrow(9:70-9:72),UpperIdent(9:73-9:76),Comma(9:76-9:77),
UpperIdent(11:1-11:2),NoSpaceOpenRound(11:2-11:3),LowerIdent(11:3-11:4),Comma(11:4-11:5),LowerIdent(11:6-11:7),Comma(11:7-11:8),CloseRound(11:8-11:9),OpColon(11:10-11:11),OpenRound(11:12-11:13),LowerIdent(11:13-11:14),Comma(11:14-11:15),LowerIdent(11:16-11:17),Comma(11:17-11:18),CloseRound(11:18-11:19),
UpperIdent(12:1-12:2),NoSpaceOpenRound(12:2-12:3),LowerIdent(12:3-12:4),Comma(12:4-12:5),LowerIdent(12:6-12:7),Comma(12:7-12:8),CloseRound(12:8-12:9),OpColon(12:10-12:11),UpperIdent(12:12-12:13),NoSpaceOpenRound(12:13-12:14),LowerIdent(12:14-12:15),Comma(12:15-12:16),LowerIdent(12:17-12:18),Comma(12:18-12:19),CloseRound(12:19-12:20),
UpperIdent(13:1-13:2),OpColon(13:3-13:4),OpenCurly(13:5-13:6),LowerIdent(13:7-13:8),OpColon(13:9-13:10),UpperIdent(13:11-13:14),Comma(13:14-13:15),LowerIdent(13:16-13:17),OpColon(13:18-13:19),UpperIdent(13:20-13:23),Comma(13:23-13:24),CloseCurly(13:25-13:26),
UpperIdent(14:1-14:2),OpColon(14:3-14:4),OpenSquare(14:5-14:6),UpperIdent(14:6-14:7),Comma(14:7-14:8),UpperIdent(14:9-14:10),Comma(14:10-14:11),CloseSquare(14:11-14:12),
LowerIdent(16:1-16:2),OpColon(16:3-16:4),LowerIdent(16:5-16:6),OpArrow(16:7-16:9),LowerIdent(16:10-16:11),KwWhere(16:12-16:17),KwModule(16:18-16:24),NoSpaceOpenRound(16:24-16:25),LowerIdent(16:25-16:26),CloseRound(16:26-16:27),NoSpaceDotUpperIdent(16:27-16:29),Comma(16:29-16:30),KwModule(16:31-16:37),NoSpaceOpenRound(16:37-16:38),LowerIdent(16:38-16:39),CloseRound(16:39-16:40),NoSpaceDotUpperIdent(16:40-16:42),Comma(16:42-16:43),
LowerIdent(18:1-18:2),OpAssign(18:3-18:4),OpBar(18:5-18:6),LowerIdent(18:6-18:7),Comma(18:7-18:8),LowerIdent(18:9-18:10),Comma(18:10-18:11),OpBar(18:11-18:12),OpenCurly(18:13-18:14),
LowerIdent(19:2-19:4),OpAssign(19:5-19:6),OpenCurly(19:7-19:8),LowerIdent(19:9-19:12),OpColon(19:12-19:13),LowerIdent(19:14-19:15),Comma(19:15-19:16),LowerIdent(19:17-19:20),OpColon(19:20-19:21),LowerIdent(19:22-19:23),Comma(19:23-19:24),LowerIdent(19:25-19:28),OpColon(19:28-19:29),OpenCurly(19:30-19:31),LowerIdent(19:32-19:36),OpColon(19:36-19:37),LowerIdent(19:38-19:39),Comma(19:39-19:40),LowerIdent(19:41-19:45),OpColon(19:45-19:46),LowerIdent(19:47-19:48),Comma(19:48-19:49),CloseCurly(19:50-19:51),Comma(19:51-19:52),CloseCurly(19:53-19:54),
LowerIdent(20:2-20:4),OpAssign(20:5-20:6),LowerIdent(20:7-20:8),NoSpaceOpenRound(20:8-20:9),LowerIdent(20:9-20:10),Comma(20:10-20:11),LowerIdent(20:12-20:13),Comma(20:13-20:14),CloseRound(20:14-20:15),
LowerIdent(21:2-21:4),OpAssign(21:5-21:6),UpperIdent(21:7-21:8),NoSpaceOpenRound(21:8-21:9),LowerIdent(21:9-21:10),Comma(21:10-21:11),LowerIdent(21:12-21:13),Comma(21:13-21:14),CloseRound(21:14-21:15),
LowerIdent(22:2-22:4),OpAssign(22:5-22:6),OpenSquare(22:7-22:8),LowerIdent(22:8-22:9),Comma(22:9-22:10),LowerIdent(22:11-22:12),Comma(22:12-22:13),CloseSquare(22:13-22:14),
LowerIdent(23:2-23:4),OpAssign(23:5-23:6),OpenRound(23:7-23:8),LowerIdent(23:8-23:9),Comma(23:9-23:10),LowerIdent(23:11-23:12),Comma(23:12-23:13),CloseRound(23:13-23:14),
KwMatch(25:2-25:7),LowerIdent(25:8-25:9),OpenCurly(25:10-25:11),
UpperIdent(26:3-26:5),NoSpaceOpenRound(26:5-26:6),NoSpaceOpenRound(26:6-26:7),LowerIdent(26:7-26:8),Comma(26:8-26:9),LowerIdent(26:10-26:11),Comma(26:11-26:12),CloseRound(26:12-26:13),CloseRound(26:13-26:14),OpFatArrow(26:15-26:17),LowerIdent(26:18-26:19),
UpperIdent(27:3-27:5),NoSpaceOpenRound(27:5-27:6),LowerIdent(27:6-27:7),Comma(27:7-27:8),LowerIdent(27:9-27:10),Comma(27:10-27:11),CloseRound(27:11-27:12),OpFatArrow(27:13-27:15),LowerIdent(27:16-27:17),
UpperIdent(28:3-28:5),NoSpaceOpenRound(28:5-28:6),OpenCurly(28:6-28:7),LowerIdent(28:8-28:9),Comma(28:9-28:10),LowerIdent(28:11-28:12),Comma(28:12-28:13),CloseCurly(28:14-28:15),CloseRound(28:15-28:16),OpFatArrow(28:17-28:19),LowerIdent(28:20-28:21),
UpperIdent(29:3-29:5),NoSpaceOpenRound(29:5-29:6),OpenSquare(29:6-29:7),LowerIdent(29:7-29:8),Comma(29:8-29:9),LowerIdent(29:10-29:11),Comma(29:11-29:12),CloseSquare(29:12-29:13),CloseRound(29:13-29:14),OpFatArrow(29:15-29:17),LowerIdent(29:18-29:19),
CloseCurly(30:2-30:3),
CloseCurly(31:1-31:2),EndOfFile(31:2-31:2),
~~~
# PARSE
~~~clojure
(file @1.1-31.2
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-import @4.1-4.31 (raw "I1")
			(exposing
				(exposed-upper-ident @4.21-4.24 (text "I11"))
				(exposed-upper-ident @4.26-4.29 (text "I12"))))
		(s-import @5.1-5.47 (raw "I2")
			(exposing
				(exposed-upper-ident @5.21-5.32 (text "I21") (as "Ias1"))
				(exposed-upper-ident @5.34-5.45 (text "I22") (as "Ias2"))))
		(s-type-decl @8.1-8.77
			(header @8.1-8.5 (name "A")
				(args
					(ty-var @8.3-8.4 (raw "a"))))
			(ty-var @8.8-8.9 (raw "a"))
			(where
				(method @8.16-8.45 (module-of "a") (name "a1")
					(args
						(ty-tuple @8.31-8.38
							(ty-var @8.32-8.33 (raw "a"))
							(ty-var @8.35-8.36 (raw "a"))))
					(ty @8.42-8.45 (name "Str")))
				(method @8.47-8.76 (module-of "a") (name "a2")
					(args
						(ty-tuple @8.62-8.69
							(ty-var @8.63-8.64 (raw "a"))
							(ty-var @8.66-8.67 (raw "a"))))
					(ty @8.73-8.76 (name "Str")))))
		(s-type-decl @9.1-9.77
			(header @9.1-9.5 (name "B")
				(args
					(ty-var @9.3-9.4 (raw "b"))))
			(ty-var @9.8-9.9 (raw "b"))
			(where
				(method @9.16-9.45 (module-of "b") (name "b1")
					(args
						(ty-tuple @9.31-9.38
							(ty-var @9.32-9.33 (raw "b"))
							(ty-var @9.35-9.36 (raw "b"))))
					(ty @9.42-9.45 (name "Str")))
				(method @9.47-9.76 (module-of "b") (name "b2")
					(args
						(ty-tuple @9.62-9.69
							(ty-var @9.63-9.64 (raw "b"))
							(ty-var @9.66-9.67 (raw "b"))))
					(ty @9.73-9.76 (name "Str")))))
		(s-type-decl @11.1-11.19
			(header @11.1-11.9 (name "C")
				(args
					(ty-var @11.3-11.4 (raw "a"))
					(ty-var @11.6-11.7 (raw "b"))))
			(ty-tuple @11.12-11.19
				(ty-var @11.13-11.14 (raw "a"))
				(ty-var @11.16-11.17 (raw "b"))))
		(s-type-decl @12.1-12.20
			(header @12.1-12.9 (name "D")
				(args
					(ty-var @12.3-12.4 (raw "a"))
					(ty-var @12.6-12.7 (raw "b"))))
			(ty-apply @12.12-12.20
				(ty @12.12-12.13 (name "C"))
				(ty-var @12.14-12.15 (raw "a"))
				(ty-var @12.17-12.18 (raw "b"))))
		(s-type-decl @13.1-13.26
			(header @13.1-13.2 (name "E")
				(args))
			(ty-record @13.5-13.26
				(anno-record-field @13.7-13.14 (name "a")
					(ty @13.11-13.14 (name "Str")))
				(anno-record-field @13.16-13.23 (name "b")
					(ty @13.20-13.23 (name "Str")))))
		(s-type-decl @14.1-14.12
			(header @14.1-14.2 (name "F")
				(args))
			(ty-tag-union @14.5-14.12
				(tags
					(ty @14.6-14.7 (name "A"))
					(ty @14.9-14.10 (name "B")))))
		(s-type-anno @16.1-16.43 (name "g")
			(ty-fn @16.5-16.11
				(ty-var @16.5-16.6 (raw "e"))
				(ty-var @16.10-16.11 (raw "e")))
			(where
				(alias @16.18-16.29 (module-of "e") (name "A"))
				(alias @16.31-16.42 (module-of "e") (name "B"))))
		(s-decl @18.1-31.2
			(p-ident @18.1-18.2 (raw "h"))
			(e-lambda @18.5-31.2
				(args
					(p-ident @18.6-18.7 (raw "x"))
					(p-ident @18.9-18.10 (raw "y")))
				(e-block @18.13-31.2
					(statements
						(s-decl @19.2-19.54
							(p-ident @19.2-19.4 (raw "h1"))
							(e-record @19.7-19.54
								(field (field "h11")
									(e-ident @19.14-19.15 (raw "x")))
								(field (field "h12")
									(e-ident @19.22-19.23 (raw "x")))
								(field (field "h13")
									(e-record @19.30-19.51
										(field (field "h131")
											(e-ident @19.38-19.39 (raw "x")))
										(field (field "h132")
											(e-ident @19.47-19.48 (raw "y")))))))
						(s-decl @20.2-20.15
							(p-ident @20.2-20.4 (raw "h2"))
							(e-apply @20.7-20.15
								(e-ident @20.7-20.8 (raw "h"))
								(e-ident @20.9-20.10 (raw "x"))
								(e-ident @20.12-20.13 (raw "y"))))
						(s-decl @21.2-21.15
							(p-ident @21.2-21.4 (raw "h3"))
							(e-apply @21.7-21.15
								(e-tag @21.7-21.8 (raw "A"))
								(e-ident @21.9-21.10 (raw "x"))
								(e-ident @21.12-21.13 (raw "y"))))
						(s-decl @22.2-22.14
							(p-ident @22.2-22.4 (raw "h4"))
							(e-list @22.7-22.14
								(e-ident @22.8-22.9 (raw "x"))
								(e-ident @22.11-22.12 (raw "y"))))
						(s-decl @23.2-23.14
							(p-ident @23.2-23.4 (raw "h5"))
							(e-tuple @23.7-23.14
								(e-ident @23.8-23.9 (raw "x"))
								(e-ident @23.11-23.12 (raw "y"))))
						(e-match
							(e-ident @25.8-25.9 (raw "x"))
							(branches
								(branch @26.3-26.19
									(p-tag @26.3-26.14 (raw "Z1")
										(p-tuple @26.6-26.13
											(p-ident @26.7-26.8 (raw "a"))
											(p-ident @26.10-26.11 (raw "b"))))
									(e-ident @26.18-26.19 (raw "a")))
								(branch @27.3-27.17
									(p-tag @27.3-27.12 (raw "Z2")
										(p-ident @27.6-27.7 (raw "a"))
										(p-ident @27.9-27.10 (raw "b")))
									(e-ident @27.16-27.17 (raw "a")))
								(branch @28.3-28.21
									(p-tag @28.3-28.16 (raw "Z3")
										(p-record @28.6-28.15
											(field @28.8-28.9 (name "a") (rest false))
											(field @28.11-28.12 (name "b") (rest false))))
									(e-ident @28.20-28.21 (raw "a")))
								(branch @29.3-29.19
									(p-tag @29.3-29.14 (raw "Z4")
										(p-list @29.6-29.13
											(p-ident @29.7-29.8 (raw "a"))
											(p-ident @29.10-29.11 (raw "b"))))
									(e-ident @29.18-29.19 (raw "a")))))))))))
~~~
# FORMATTED
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
		(p-assign @18.1-18.2 (ident "h"))
		(e-closure @18.5-31.2
			(captures
				(capture @28.8-28.9 (ident "a"))
				(capture @29.7-29.8 (ident "a"))
				(capture @18.1-18.2 (ident "h"))
				(capture @26.7-26.8 (ident "a"))
				(capture @27.6-27.7 (ident "a")))
			(e-lambda @18.5-31.2
				(args
					(p-assign @18.6-18.7 (ident "x"))
					(p-assign @18.9-18.10 (ident "y")))
				(e-block @18.13-31.2
					(s-let @19.2-19.54
						(p-assign @19.2-19.4 (ident "h1"))
						(e-record @19.7-19.54
							(fields
								(field (name "h11")
									(e-lookup-local @19.14-19.15
										(p-assign @18.6-18.7 (ident "x"))))
								(field (name "h12")
									(e-lookup-local @19.22-19.23
										(p-assign @18.6-18.7 (ident "x"))))
								(field (name "h13")
									(e-record @19.30-19.51
										(fields
											(field (name "h131")
												(e-lookup-local @19.38-19.39
													(p-assign @18.6-18.7 (ident "x"))))
											(field (name "h132")
												(e-lookup-local @19.47-19.48
													(p-assign @18.9-18.10 (ident "y"))))))))))
					(s-let @20.2-20.15
						(p-assign @20.2-20.4 (ident "h2"))
						(e-call @20.7-20.15
							(e-lookup-local @20.7-20.8
								(p-assign @18.1-18.2 (ident "h")))
							(e-lookup-local @20.9-20.10
								(p-assign @18.6-18.7 (ident "x")))
							(e-lookup-local @20.12-20.13
								(p-assign @18.9-18.10 (ident "y")))))
					(s-let @21.2-21.15
						(p-assign @21.2-21.4 (ident "h3"))
						(e-tag @21.7-21.8 (name "A")
							(args
								(e-lookup-local @21.9-21.10
									(p-assign @18.6-18.7 (ident "x")))
								(e-lookup-local @21.12-21.13
									(p-assign @18.9-18.10 (ident "y"))))))
					(s-let @22.2-22.14
						(p-assign @22.2-22.4 (ident "h4"))
						(e-list @22.7-22.14
							(elems
								(e-lookup-local @22.8-22.9
									(p-assign @18.6-18.7 (ident "x")))
								(e-lookup-local @22.11-22.12
									(p-assign @18.9-18.10 (ident "y"))))))
					(s-let @23.2-23.14
						(p-assign @23.2-23.4 (ident "h5"))
						(e-tuple @23.7-23.14
							(elems
								(e-lookup-local @23.8-23.9
									(p-assign @18.6-18.7 (ident "x")))
								(e-lookup-local @23.11-23.12
									(p-assign @18.9-18.10 (ident "y"))))))
					(e-match @25.2-30.3
						(match @25.2-30.3
							(cond
								(e-lookup-local @25.8-25.9
									(p-assign @18.6-18.7 (ident "x"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @26.3-26.14)))
									(value
										(e-lookup-local @26.18-26.19
											(p-assign @26.7-26.8 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @27.3-27.12)))
									(value
										(e-lookup-local @27.16-27.17
											(p-assign @27.6-27.7 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @28.3-28.16)))
									(value
										(e-lookup-local @28.20-28.21
											(p-assign @28.8-28.9 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @29.3-29.14)))
									(value
										(e-lookup-local @29.18-29.19
											(p-assign @29.7-29.8 (ident "a"))))))))))))
	(s-alias-decl @8.1-8.77
		(ty-header @8.1-8.5 (name "A")
			(ty-args
				(ty-var @8.3-8.4 (name "a"))))
		(ty-var @8.8-8.9 (name "a"))
		(where
			(method @8.16-8.45 (module-of "a") (ident "a1")
				(args
					(ty-tuple @8.31-8.38
						(ty-var @8.32-8.33 (name "a"))
						(ty-var @8.35-8.36 (name "a"))))
				(ty @8.42-8.45 (name "Str")))
			(method @8.47-8.76 (module-of "a") (ident "a2")
				(args
					(ty-tuple @8.62-8.69
						(ty-var @8.63-8.64 (name "a"))
						(ty-var @8.66-8.67 (name "a"))))
				(ty @8.73-8.76 (name "Str")))))
	(s-alias-decl @9.1-9.77
		(ty-header @9.1-9.5 (name "B")
			(ty-args
				(ty-var @9.3-9.4 (name "b"))))
		(ty-var @9.8-9.9 (name "b"))
		(where
			(method @9.16-9.45 (module-of "b") (ident "b1")
				(args
					(ty-tuple @9.31-9.38
						(ty-var @9.32-9.33 (name "b"))
						(ty-var @9.35-9.36 (name "b"))))
				(ty @9.42-9.45 (name "Str")))
			(method @9.47-9.76 (module-of "b") (ident "b2")
				(args
					(ty-tuple @9.62-9.69
						(ty-var @9.63-9.64 (name "b"))
						(ty-var @9.66-9.67 (name "b"))))
				(ty @9.73-9.76 (name "Str")))))
	(s-alias-decl @11.1-11.19
		(ty-header @11.1-11.9 (name "C")
			(ty-args
				(ty-var @11.3-11.4 (name "a"))
				(ty-var @11.6-11.7 (name "b"))))
		(ty-tuple @11.12-11.19
			(ty-var @11.13-11.14 (name "a"))
			(ty-var @11.16-11.17 (name "b"))))
	(s-alias-decl @12.1-12.20
		(ty-header @12.1-12.9 (name "D")
			(ty-args
				(ty-var @12.3-12.4 (name "a"))
				(ty-var @12.6-12.7 (name "b"))))
		(ty-apply @12.12-12.20 (symbol "C")
			(ty-var @12.14-12.15 (name "a"))
			(ty-var @12.17-12.18 (name "b"))))
	(s-alias-decl @13.1-13.26
		(ty-header @13.1-13.2 (name "E"))
		(ty-record @13.5-13.26
			(field (field "a")
				(ty @13.11-13.14 (name "Str")))
			(field (field "b")
				(ty @13.20-13.23 (name "Str")))))
	(s-alias-decl @14.1-14.12
		(ty-header @14.1-14.2 (name "F"))
		(ty-tag-union @14.5-14.12
			(ty @14.6-14.7 (name "A"))
			(ty @14.9-14.10 (name "B"))))
	(s-import @4.1-4.31 (module "I1")
		(exposes
			(exposed (name "I11") (wildcard false))
			(exposed (name "I12") (wildcard false))))
	(s-import @5.1-5.47 (module "I2")
		(exposes
			(exposed (name "I21") (alias "Ias1") (wildcard false))
			(exposed (name "I22") (alias "Ias2") (wildcard false))))
	(s-type-anno @16.1-16.43 (name "g")
		(ty-fn @16.5-16.11 (effectful false)
			(ty-var @16.5-16.6 (name "e"))
			(ty-var @16.10-16.11 (name "e")))
		(where
			(alias @16.18-16.29 (module-of "e") (ident "A"))
			(alias @16.31-16.42 (module-of "e") (ident "B"))))
	(ext-decl @8.16-8.45 (ident "module(a).a1") (kind "value"))
	(ext-decl @8.47-8.76 (ident "module(a).a2") (kind "value"))
	(ext-decl @9.16-9.45 (ident "module(b).b1") (kind "value"))
	(ext-decl @9.47-9.76 (ident "module(b).b2") (kind "value"))
	(ext-decl @16.18-16.29 (ident "module(e).A") (kind "type"))
	(ext-decl @16.31-16.42 (ident "module(e).B") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @18.1-18.2 (type "[Z1((field, field2)), Z2(c, d), Z3(f), Z4(List(elem))]others, [Z1((field3, field4)), Z2(i, j), Z3(k), Z4(List(elem2))]others2 -> _ret")))
	(type_decls
		(alias @8.1-8.77 (type "A(a)")
			(ty-header @8.1-8.5 (name "A")
				(ty-args
					(ty-var @8.3-8.4 (name "a")))))
		(alias @9.1-9.77 (type "B(b)")
			(ty-header @9.1-9.5 (name "B")
				(ty-args
					(ty-var @9.3-9.4 (name "b")))))
		(alias @11.1-11.19 (type "C(a, b)")
			(ty-header @11.1-11.9 (name "C")
				(ty-args
					(ty-var @11.3-11.4 (name "a"))
					(ty-var @11.6-11.7 (name "b")))))
		(alias @12.1-12.20 (type "D(a, b)")
			(ty-header @12.1-12.9 (name "D")
				(ty-args
					(ty-var @12.3-12.4 (name "a"))
					(ty-var @12.6-12.7 (name "b")))))
		(alias @13.1-13.26 (type "E")
			(ty-header @13.1-13.2 (name "E")))
		(alias @14.1-14.12 (type "F")
			(ty-header @14.1-14.2 (name "F"))))
	(expressions
		(expr @18.5-31.2 (type "[Z1((field, field2)), Z2(c, d), Z3(f), Z4(List(elem))]others, [Z1((field3, field4)), Z2(i, j), Z3(k), Z4(List(elem2))]others2 -> _ret"))))
~~~
