# META
~~~ini
description=Singleline formatting everything
type=file
~~~
# SOURCE
~~~roc
module []

# Import exposing
import I1 exposing [I11, I12]
import I2 exposing [I21 as Ias1, I22 as Ias2]

# Where constraint
A(a) : a where module(a).a1 : (a, a) -> Str, module(a).a2 : (a, a) -> Str
B(b) : b where module(b).b1 : (b, b) -> Str, module(b).b2 : (b, b) -> Str

C(a, b) : (a, b)
D(a, b) : C(a, b)
E : { a : Str, b : Str }
F : [A, B]

g : e -> e where module(e).A, module(e).B

h = |x, y| {
	h1 = { h11: x, h12: x, h13: { h131: x, h132: y } }
	h2 = h(x, y)
	h3 = A(x, y)
	h4 = [x, y]
	h5 = (x, y)

	match x {
		Z1((a, b)) => a
		Z2(a, b) => a
		Z3({ a, b }) => a
		Z4([a, b]) => a
	}
}
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:8:1:8:74
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:9:1:9:74
MODULE NOT FOUND - everything.md:4:1:4:30
MODULE NOT FOUND - everything.md:5:1:5:46
UNUSED VARIABLE - everything.md:26:10:26:11
UNUSED VARIABLE - everything.md:27:9:27:10
UNUSED VARIABLE - everything.md:28:11:28:12
UNUSED VARIABLE - everything.md:29:10:29:11
UNUSED VARIABLE - everything.md:19:2:19:4
UNUSED VARIABLE - everything.md:22:2:22:4
UNUSED VARIABLE - everything.md:20:2:20:4
UNUSED VARIABLE - everything.md:21:2:21:4
UNUSED VARIABLE - everything.md:23:2:23:4
# PROBLEMS
**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**everything.md:8:1:8:74:**
```roc
A(a) : a where module(a).a1 : (a, a) -> Str, module(a).a2 : (a, a) -> Str
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**everything.md:9:1:9:74:**
```roc
B(b) : b where module(b).b1 : (b, b) -> Str, module(b).b2 : (b, b) -> Str
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `I1` was not found in this Roc project.

You're attempting to use this module here:
**everything.md:4:1:4:30:**
```roc
import I1 exposing [I11, I12]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `I2` was not found in this Roc project.

You're attempting to use this module here:
**everything.md:5:1:5:46:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:26:10:26:11:**
```roc
		Z1((a, b)) => a
```
         ^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:27:9:27:10:**
```roc
		Z2(a, b) => a
```
        ^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:28:11:28:12:**
```roc
		Z3({ a, b }) => a
```
          ^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:29:10:29:11:**
```roc
		Z4([a, b]) => a
```
         ^


**UNUSED VARIABLE**
Variable `h1` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h1` to suppress this warning.
The unused variable is declared here:
**everything.md:19:2:19:4:**
```roc
	h1 = { h11: x, h12: x, h13: { h131: x, h132: y } }
```
 ^^


**UNUSED VARIABLE**
Variable `h4` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h4` to suppress this warning.
The unused variable is declared here:
**everything.md:22:2:22:4:**
```roc
	h4 = [x, y]
```
 ^^


**UNUSED VARIABLE**
Variable `h2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h2` to suppress this warning.
The unused variable is declared here:
**everything.md:20:2:20:4:**
```roc
	h2 = h(x, y)
```
 ^^


**UNUSED VARIABLE**
Variable `h3` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h3` to suppress this warning.
The unused variable is declared here:
**everything.md:21:2:21:4:**
```roc
	h3 = A(x, y)
```
 ^^


**UNUSED VARIABLE**
Variable `h5` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h5` to suppress this warning.
The unused variable is declared here:
**everything.md:23:2:23:4:**
```roc
	h5 = (x, y)
```
 ^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
KwImport(4:1-4:7),UpperIdent(4:8-4:10),KwExposing(4:11-4:19),OpenSquare(4:20-4:21),UpperIdent(4:21-4:24),Comma(4:24-4:25),UpperIdent(4:26-4:29),CloseSquare(4:29-4:30),
KwImport(5:1-5:7),UpperIdent(5:8-5:10),KwExposing(5:11-5:19),OpenSquare(5:20-5:21),UpperIdent(5:21-5:24),KwAs(5:25-5:27),UpperIdent(5:28-5:32),Comma(5:32-5:33),UpperIdent(5:34-5:37),KwAs(5:38-5:40),UpperIdent(5:41-5:45),CloseSquare(5:45-5:46),
UpperIdent(8:1-8:2),NoSpaceOpenRound(8:2-8:3),LowerIdent(8:3-8:4),CloseRound(8:4-8:5),OpColon(8:6-8:7),LowerIdent(8:8-8:9),KwWhere(8:10-8:15),KwModule(8:16-8:22),NoSpaceOpenRound(8:22-8:23),LowerIdent(8:23-8:24),CloseRound(8:24-8:25),NoSpaceDotLowerIdent(8:25-8:28),OpColon(8:29-8:30),OpenRound(8:31-8:32),LowerIdent(8:32-8:33),Comma(8:33-8:34),LowerIdent(8:35-8:36),CloseRound(8:36-8:37),OpArrow(8:38-8:40),UpperIdent(8:41-8:44),Comma(8:44-8:45),KwModule(8:46-8:52),NoSpaceOpenRound(8:52-8:53),LowerIdent(8:53-8:54),CloseRound(8:54-8:55),NoSpaceDotLowerIdent(8:55-8:58),OpColon(8:59-8:60),OpenRound(8:61-8:62),LowerIdent(8:62-8:63),Comma(8:63-8:64),LowerIdent(8:65-8:66),CloseRound(8:66-8:67),OpArrow(8:68-8:70),UpperIdent(8:71-8:74),
UpperIdent(9:1-9:2),NoSpaceOpenRound(9:2-9:3),LowerIdent(9:3-9:4),CloseRound(9:4-9:5),OpColon(9:6-9:7),LowerIdent(9:8-9:9),KwWhere(9:10-9:15),KwModule(9:16-9:22),NoSpaceOpenRound(9:22-9:23),LowerIdent(9:23-9:24),CloseRound(9:24-9:25),NoSpaceDotLowerIdent(9:25-9:28),OpColon(9:29-9:30),OpenRound(9:31-9:32),LowerIdent(9:32-9:33),Comma(9:33-9:34),LowerIdent(9:35-9:36),CloseRound(9:36-9:37),OpArrow(9:38-9:40),UpperIdent(9:41-9:44),Comma(9:44-9:45),KwModule(9:46-9:52),NoSpaceOpenRound(9:52-9:53),LowerIdent(9:53-9:54),CloseRound(9:54-9:55),NoSpaceDotLowerIdent(9:55-9:58),OpColon(9:59-9:60),OpenRound(9:61-9:62),LowerIdent(9:62-9:63),Comma(9:63-9:64),LowerIdent(9:65-9:66),CloseRound(9:66-9:67),OpArrow(9:68-9:70),UpperIdent(9:71-9:74),
UpperIdent(11:1-11:2),NoSpaceOpenRound(11:2-11:3),LowerIdent(11:3-11:4),Comma(11:4-11:5),LowerIdent(11:6-11:7),CloseRound(11:7-11:8),OpColon(11:9-11:10),OpenRound(11:11-11:12),LowerIdent(11:12-11:13),Comma(11:13-11:14),LowerIdent(11:15-11:16),CloseRound(11:16-11:17),
UpperIdent(12:1-12:2),NoSpaceOpenRound(12:2-12:3),LowerIdent(12:3-12:4),Comma(12:4-12:5),LowerIdent(12:6-12:7),CloseRound(12:7-12:8),OpColon(12:9-12:10),UpperIdent(12:11-12:12),NoSpaceOpenRound(12:12-12:13),LowerIdent(12:13-12:14),Comma(12:14-12:15),LowerIdent(12:16-12:17),CloseRound(12:17-12:18),
UpperIdent(13:1-13:2),OpColon(13:3-13:4),OpenCurly(13:5-13:6),LowerIdent(13:7-13:8),OpColon(13:9-13:10),UpperIdent(13:11-13:14),Comma(13:14-13:15),LowerIdent(13:16-13:17),OpColon(13:18-13:19),UpperIdent(13:20-13:23),CloseCurly(13:24-13:25),
UpperIdent(14:1-14:2),OpColon(14:3-14:4),OpenSquare(14:5-14:6),UpperIdent(14:6-14:7),Comma(14:7-14:8),UpperIdent(14:9-14:10),CloseSquare(14:10-14:11),
LowerIdent(16:1-16:2),OpColon(16:3-16:4),LowerIdent(16:5-16:6),OpArrow(16:7-16:9),LowerIdent(16:10-16:11),KwWhere(16:12-16:17),KwModule(16:18-16:24),NoSpaceOpenRound(16:24-16:25),LowerIdent(16:25-16:26),CloseRound(16:26-16:27),NoSpaceDotUpperIdent(16:27-16:29),Comma(16:29-16:30),KwModule(16:31-16:37),NoSpaceOpenRound(16:37-16:38),LowerIdent(16:38-16:39),CloseRound(16:39-16:40),NoSpaceDotUpperIdent(16:40-16:42),
LowerIdent(18:1-18:2),OpAssign(18:3-18:4),OpBar(18:5-18:6),LowerIdent(18:6-18:7),Comma(18:7-18:8),LowerIdent(18:9-18:10),OpBar(18:10-18:11),OpenCurly(18:12-18:13),
LowerIdent(19:2-19:4),OpAssign(19:5-19:6),OpenCurly(19:7-19:8),LowerIdent(19:9-19:12),OpColon(19:12-19:13),LowerIdent(19:14-19:15),Comma(19:15-19:16),LowerIdent(19:17-19:20),OpColon(19:20-19:21),LowerIdent(19:22-19:23),Comma(19:23-19:24),LowerIdent(19:25-19:28),OpColon(19:28-19:29),OpenCurly(19:30-19:31),LowerIdent(19:32-19:36),OpColon(19:36-19:37),LowerIdent(19:38-19:39),Comma(19:39-19:40),LowerIdent(19:41-19:45),OpColon(19:45-19:46),LowerIdent(19:47-19:48),CloseCurly(19:49-19:50),CloseCurly(19:51-19:52),
LowerIdent(20:2-20:4),OpAssign(20:5-20:6),LowerIdent(20:7-20:8),NoSpaceOpenRound(20:8-20:9),LowerIdent(20:9-20:10),Comma(20:10-20:11),LowerIdent(20:12-20:13),CloseRound(20:13-20:14),
LowerIdent(21:2-21:4),OpAssign(21:5-21:6),UpperIdent(21:7-21:8),NoSpaceOpenRound(21:8-21:9),LowerIdent(21:9-21:10),Comma(21:10-21:11),LowerIdent(21:12-21:13),CloseRound(21:13-21:14),
LowerIdent(22:2-22:4),OpAssign(22:5-22:6),OpenSquare(22:7-22:8),LowerIdent(22:8-22:9),Comma(22:9-22:10),LowerIdent(22:11-22:12),CloseSquare(22:12-22:13),
LowerIdent(23:2-23:4),OpAssign(23:5-23:6),OpenRound(23:7-23:8),LowerIdent(23:8-23:9),Comma(23:9-23:10),LowerIdent(23:11-23:12),CloseRound(23:12-23:13),
KwMatch(25:2-25:7),LowerIdent(25:8-25:9),OpenCurly(25:10-25:11),
UpperIdent(26:3-26:5),NoSpaceOpenRound(26:5-26:6),NoSpaceOpenRound(26:6-26:7),LowerIdent(26:7-26:8),Comma(26:8-26:9),LowerIdent(26:10-26:11),CloseRound(26:11-26:12),CloseRound(26:12-26:13),OpFatArrow(26:14-26:16),LowerIdent(26:17-26:18),
UpperIdent(27:3-27:5),NoSpaceOpenRound(27:5-27:6),LowerIdent(27:6-27:7),Comma(27:7-27:8),LowerIdent(27:9-27:10),CloseRound(27:10-27:11),OpFatArrow(27:12-27:14),LowerIdent(27:15-27:16),
UpperIdent(28:3-28:5),NoSpaceOpenRound(28:5-28:6),OpenCurly(28:6-28:7),LowerIdent(28:8-28:9),Comma(28:9-28:10),LowerIdent(28:11-28:12),CloseCurly(28:13-28:14),CloseRound(28:14-28:15),OpFatArrow(28:16-28:18),LowerIdent(28:19-28:20),
UpperIdent(29:3-29:5),NoSpaceOpenRound(29:5-29:6),OpenSquare(29:6-29:7),LowerIdent(29:7-29:8),Comma(29:8-29:9),LowerIdent(29:10-29:11),CloseSquare(29:11-29:12),CloseRound(29:12-29:13),OpFatArrow(29:14-29:16),LowerIdent(29:17-29:18),
CloseCurly(30:2-30:3),
CloseCurly(31:1-31:2),EndOfFile(31:2-31:2),
~~~
# PARSE
~~~clojure
(file @1.1-31.2
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-import @4.1-4.30 (raw "I1")
			(exposing
				(exposed-upper-ident @4.21-4.24 (text "I11"))
				(exposed-upper-ident @4.26-4.29 (text "I12"))))
		(s-import @5.1-5.46 (raw "I2")
			(exposing
				(exposed-upper-ident @5.21-5.32 (text "I21") (as "Ias1"))
				(exposed-upper-ident @5.34-5.45 (text "I22") (as "Ias2"))))
		(s-type-decl @8.1-8.74
			(header @8.1-8.5 (name "A")
				(args
					(ty-var @8.3-8.4 (raw "a"))))
			(ty-var @8.8-8.9 (raw "a"))
			(where
				(method @8.16-8.44 (module-of "a") (name "a1")
					(args
						(ty-tuple @8.31-8.37
							(ty-var @8.32-8.33 (raw "a"))
							(ty-var @8.35-8.36 (raw "a"))))
					(ty @8.41-8.44 (name "Str")))
				(method @8.46-8.74 (module-of "a") (name "a2")
					(args
						(ty-tuple @8.61-8.67
							(ty-var @8.62-8.63 (raw "a"))
							(ty-var @8.65-8.66 (raw "a"))))
					(ty @8.71-8.74 (name "Str")))))
		(s-type-decl @9.1-9.74
			(header @9.1-9.5 (name "B")
				(args
					(ty-var @9.3-9.4 (raw "b"))))
			(ty-var @9.8-9.9 (raw "b"))
			(where
				(method @9.16-9.44 (module-of "b") (name "b1")
					(args
						(ty-tuple @9.31-9.37
							(ty-var @9.32-9.33 (raw "b"))
							(ty-var @9.35-9.36 (raw "b"))))
					(ty @9.41-9.44 (name "Str")))
				(method @9.46-9.74 (module-of "b") (name "b2")
					(args
						(ty-tuple @9.61-9.67
							(ty-var @9.62-9.63 (raw "b"))
							(ty-var @9.65-9.66 (raw "b"))))
					(ty @9.71-9.74 (name "Str")))))
		(s-type-decl @11.1-11.17
			(header @11.1-11.8 (name "C")
				(args
					(ty-var @11.3-11.4 (raw "a"))
					(ty-var @11.6-11.7 (raw "b"))))
			(ty-tuple @11.11-11.17
				(ty-var @11.12-11.13 (raw "a"))
				(ty-var @11.15-11.16 (raw "b"))))
		(s-type-decl @12.1-12.18
			(header @12.1-12.8 (name "D")
				(args
					(ty-var @12.3-12.4 (raw "a"))
					(ty-var @12.6-12.7 (raw "b"))))
			(ty-apply @12.11-12.18
				(ty @12.11-12.12 (name "C"))
				(ty-var @12.13-12.14 (raw "a"))
				(ty-var @12.16-12.17 (raw "b"))))
		(s-type-decl @13.1-13.25
			(header @13.1-13.2 (name "E")
				(args))
			(ty-record @13.5-13.25
				(anno-record-field @13.7-13.14 (name "a")
					(ty @13.11-13.14 (name "Str")))
				(anno-record-field @13.16-13.23 (name "b")
					(ty @13.20-13.23 (name "Str")))))
		(s-type-decl @14.1-14.11
			(header @14.1-14.2 (name "F")
				(args))
			(ty-tag-union @14.5-14.11
				(tags
					(ty @14.6-14.7 (name "A"))
					(ty @14.9-14.10 (name "B")))))
		(s-type-anno @16.1-16.42 (name "g")
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
				(e-block @18.12-31.2
					(statements
						(s-decl @19.2-19.52
							(p-ident @19.2-19.4 (raw "h1"))
							(e-record @19.7-19.52
								(field (field "h11")
									(e-ident @19.14-19.15 (raw "x")))
								(field (field "h12")
									(e-ident @19.22-19.23 (raw "x")))
								(field (field "h13")
									(e-record @19.30-19.50
										(field (field "h131")
											(e-ident @19.38-19.39 (raw "x")))
										(field (field "h132")
											(e-ident @19.47-19.48 (raw "y")))))))
						(s-decl @20.2-20.14
							(p-ident @20.2-20.4 (raw "h2"))
							(e-apply @20.7-20.14
								(e-ident @20.7-20.8 (raw "h"))
								(e-ident @20.9-20.10 (raw "x"))
								(e-ident @20.12-20.13 (raw "y"))))
						(s-decl @21.2-21.14
							(p-ident @21.2-21.4 (raw "h3"))
							(e-apply @21.7-21.14
								(e-tag @21.7-21.8 (raw "A"))
								(e-ident @21.9-21.10 (raw "x"))
								(e-ident @21.12-21.13 (raw "y"))))
						(s-decl @22.2-22.13
							(p-ident @22.2-22.4 (raw "h4"))
							(e-list @22.7-22.13
								(e-ident @22.8-22.9 (raw "x"))
								(e-ident @22.11-22.12 (raw "y"))))
						(s-decl @23.2-23.13
							(p-ident @23.2-23.4 (raw "h5"))
							(e-tuple @23.7-23.13
								(e-ident @23.8-23.9 (raw "x"))
								(e-ident @23.11-23.12 (raw "y"))))
						(e-match
							(e-ident @25.8-25.9 (raw "x"))
							(branches
								(branch @26.3-26.18
									(p-tag @26.3-26.13 (raw "Z1")
										(p-tuple @26.6-26.12
											(p-ident @26.7-26.8 (raw "a"))
											(p-ident @26.10-26.11 (raw "b"))))
									(e-ident @26.17-26.18 (raw "a")))
								(branch @27.3-27.16
									(p-tag @27.3-27.11 (raw "Z2")
										(p-ident @27.6-27.7 (raw "a"))
										(p-ident @27.9-27.10 (raw "b")))
									(e-ident @27.15-27.16 (raw "a")))
								(branch @28.3-28.20
									(p-tag @28.3-28.15 (raw "Z3")
										(p-record @28.6-28.14
											(field @28.8-28.9 (name "a") (rest false))
											(field @28.11-28.12 (name "b") (rest false))))
									(e-ident @28.19-28.20 (raw "a")))
								(branch @29.3-29.18
									(p-tag @29.3-29.13 (raw "Z4")
										(p-list @29.6-29.12
											(p-ident @29.7-29.8 (raw "a"))
											(p-ident @29.10-29.11 (raw "b"))))
									(e-ident @29.17-29.18 (raw "a")))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @18.1-18.2 (ident "h"))
		(e-closure @18.5-31.2
			(captures
				(capture @28.8-28.9 (ident "a"))
				(capture @26.7-26.8 (ident "a"))
				(capture @18.1-18.2 (ident "h"))
				(capture @29.7-29.8 (ident "a"))
				(capture @27.6-27.7 (ident "a")))
			(e-lambda @18.5-31.2
				(args
					(p-assign @18.6-18.7 (ident "x"))
					(p-assign @18.9-18.10 (ident "y")))
				(e-block @18.12-31.2
					(s-let @19.2-19.52
						(p-assign @19.2-19.4 (ident "h1"))
						(e-record @19.7-19.52
							(fields
								(field (name "h11")
									(e-lookup-local @19.14-19.15
										(p-assign @18.6-18.7 (ident "x"))))
								(field (name "h12")
									(e-lookup-local @19.22-19.23
										(p-assign @18.6-18.7 (ident "x"))))
								(field (name "h13")
									(e-record @19.30-19.50
										(fields
											(field (name "h131")
												(e-lookup-local @19.38-19.39
													(p-assign @18.6-18.7 (ident "x"))))
											(field (name "h132")
												(e-lookup-local @19.47-19.48
													(p-assign @18.9-18.10 (ident "y"))))))))))
					(s-let @20.2-20.14
						(p-assign @20.2-20.4 (ident "h2"))
						(e-call @20.7-20.14
							(e-lookup-local @20.7-20.8
								(p-assign @18.1-18.2 (ident "h")))
							(e-lookup-local @20.9-20.10
								(p-assign @18.6-18.7 (ident "x")))
							(e-lookup-local @20.12-20.13
								(p-assign @18.9-18.10 (ident "y")))))
					(s-let @21.2-21.14
						(p-assign @21.2-21.4 (ident "h3"))
						(e-tag @21.7-21.8 (name "A")
							(args
								(e-lookup-local @21.9-21.10
									(p-assign @18.6-18.7 (ident "x")))
								(e-lookup-local @21.12-21.13
									(p-assign @18.9-18.10 (ident "y"))))))
					(s-let @22.2-22.13
						(p-assign @22.2-22.4 (ident "h4"))
						(e-list @22.7-22.13
							(elems
								(e-lookup-local @22.8-22.9
									(p-assign @18.6-18.7 (ident "x")))
								(e-lookup-local @22.11-22.12
									(p-assign @18.9-18.10 (ident "y"))))))
					(s-let @23.2-23.13
						(p-assign @23.2-23.4 (ident "h5"))
						(e-tuple @23.7-23.13
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
											(p-applied-tag @26.3-26.13)))
									(value
										(e-lookup-local @26.17-26.18
											(p-assign @26.7-26.8 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @27.3-27.11)))
									(value
										(e-lookup-local @27.15-27.16
											(p-assign @27.6-27.7 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @28.3-28.15)))
									(value
										(e-lookup-local @28.19-28.20
											(p-assign @28.8-28.9 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @29.3-29.13)))
									(value
										(e-lookup-local @29.17-29.18
											(p-assign @29.7-29.8 (ident "a"))))))))))))
	(s-alias-decl @8.1-8.74
		(ty-header @8.1-8.5 (name "A")
			(ty-args
				(ty-var @8.3-8.4 (name "a"))))
		(ty-var @8.8-8.9 (name "a")))
	(s-alias-decl @9.1-9.74
		(ty-header @9.1-9.5 (name "B")
			(ty-args
				(ty-var @9.3-9.4 (name "b"))))
		(ty-var @9.8-9.9 (name "b")))
	(s-alias-decl @11.1-11.17
		(ty-header @11.1-11.8 (name "C")
			(ty-args
				(ty-var @11.3-11.4 (name "a"))
				(ty-var @11.6-11.7 (name "b"))))
		(ty-tuple @11.11-11.17
			(ty-var @11.12-11.13 (name "a"))
			(ty-var @11.15-11.16 (name "b"))))
	(s-alias-decl @12.1-12.18
		(ty-header @12.1-12.8 (name "D")
			(ty-args
				(ty-var @12.3-12.4 (name "a"))
				(ty-var @12.6-12.7 (name "b"))))
		(ty-apply @12.11-12.18 (symbol "C")
			(ty-var @12.13-12.14 (name "a"))
			(ty-var @12.16-12.17 (name "b"))))
	(s-alias-decl @13.1-13.25
		(ty-header @13.1-13.2 (name "E"))
		(ty-record @13.5-13.25
			(field (field "a")
				(ty @13.11-13.14 (name "Str")))
			(field (field "b")
				(ty @13.20-13.23 (name "Str")))))
	(s-alias-decl @14.1-14.11
		(ty-header @14.1-14.2 (name "F"))
		(ty-tag-union @14.5-14.11
			(ty @14.6-14.7 (name "A"))
			(ty @14.9-14.10 (name "B"))))
	(s-import @4.1-4.30 (module "I1")
		(exposes
			(exposed (name "I11") (wildcard false))
			(exposed (name "I12") (wildcard false))))
	(s-import @5.1-5.46 (module "I2")
		(exposes
			(exposed (name "I21") (alias "Ias1") (wildcard false))
			(exposed (name "I22") (alias "Ias2") (wildcard false))))
	(s-type-anno @16.1-16.42 (name "g")
		(ty-fn @16.5-16.11 (effectful false)
			(ty-var @16.5-16.6 (name "e"))
			(ty-var @16.10-16.11 (name "e")))
		(where
			(alias @16.18-16.29 (module-of "e") (ident "A"))
			(alias @16.31-16.42 (module-of "e") (ident "B"))))
	(ext-decl @16.18-16.29 (ident "module(e).A") (kind "type"))
	(ext-decl @16.31-16.42 (ident "module(e).B") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @18.1-18.2 (type "[Z1((field, field2)), Z2(c, d), Z3(f), Z4(List(elem))]others, [Z1((field3, field4)), Z2(i, j), Z3(k), Z4(List(elem2))]others2 -> _ret")))
	(type_decls
		(alias @8.1-8.74 (type "A(a)")
			(ty-header @8.1-8.5 (name "A")
				(ty-args
					(ty-var @8.3-8.4 (name "a")))))
		(alias @9.1-9.74 (type "B(b)")
			(ty-header @9.1-9.5 (name "B")
				(ty-args
					(ty-var @9.3-9.4 (name "b")))))
		(alias @11.1-11.17 (type "C(a, b)")
			(ty-header @11.1-11.8 (name "C")
				(ty-args
					(ty-var @11.3-11.4 (name "a"))
					(ty-var @11.6-11.7 (name "b")))))
		(alias @12.1-12.18 (type "D(a, b)")
			(ty-header @12.1-12.8 (name "D")
				(ty-args
					(ty-var @12.3-12.4 (name "a"))
					(ty-var @12.6-12.7 (name "b")))))
		(alias @13.1-13.25 (type "E")
			(ty-header @13.1-13.2 (name "E")))
		(alias @14.1-14.11 (type "F")
			(ty-header @14.1-14.2 (name "F"))))
	(expressions
		(expr @18.5-31.2 (type "[Z1((field, field2)), Z2(c, d), Z3(f), Z4(List(elem))]others, [Z1((field3, field4)), Z2(i, j), Z3(k), Z4(List(elem2))]others2 -> _ret"))))
~~~
