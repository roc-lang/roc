# META
~~~ini
description=Singleline formatting everything
type=snippet
~~~
# SOURCE
~~~roc
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
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:6:1:6:74
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - everything.md:7:1:7:74
MODULE NOT FOUND - everything.md:2:1:2:30
MODULE NOT FOUND - everything.md:3:1:3:46
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
**everything.md:6:1:6:74:**
```roc
A(a) : a where module(a).a1 : (a, a) -> Str, module(a).a2 : (a, a) -> Str
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**everything.md:7:1:7:74:**
```roc
B(b) : b where module(b).b1 : (b, b) -> Str, module(b).b2 : (b, b) -> Str
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `I1` was not found in this Roc project.

You're attempting to use this module here:
**everything.md:2:1:2:30:**
```roc
import I1 exposing [I11, I12]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `I2` was not found in this Roc project.

You're attempting to use this module here:
**everything.md:3:1:3:46:**
```roc
import I2 exposing [I21 as Ias1, I22 as Ias2]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:24:10:24:11:**
```roc
		Z1((a, b)) => a
```
		       ^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:25:9:25:10:**
```roc
		Z2(a, b) => a
```
		      ^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:26:11:26:12:**
```roc
		Z3({ a, b }) => a
```
		        ^


**UNUSED VARIABLE**
Variable `b` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_b` to suppress this warning.
The unused variable is declared here:
**everything.md:27:10:27:11:**
```roc
		Z4([a, b]) => a
```
		       ^


**UNUSED VARIABLE**
Variable `h1` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h1` to suppress this warning.
The unused variable is declared here:
**everything.md:17:2:17:4:**
```roc
	h1 = { h11: x, h12: x, h13: { h131: x, h132: y } }
```
	^^


**UNUSED VARIABLE**
Variable `h2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h2` to suppress this warning.
The unused variable is declared here:
**everything.md:18:2:18:4:**
```roc
	h2 = h(x, y)
```
	^^


**UNUSED VARIABLE**
Variable `h3` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h3` to suppress this warning.
The unused variable is declared here:
**everything.md:19:2:19:4:**
```roc
	h3 = A(x, y)
```
	^^


**UNUSED VARIABLE**
Variable `h4` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h4` to suppress this warning.
The unused variable is declared here:
**everything.md:20:2:20:4:**
```roc
	h4 = [x, y]
```
	^^


**UNUSED VARIABLE**
Variable `h5` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_h5` to suppress this warning.
The unused variable is declared here:
**everything.md:21:2:21:4:**
```roc
	h5 = (x, y)
```
	^^


# TOKENS
~~~zig
KwImport(2:1-2:7),UpperIdent(2:8-2:10),KwExposing(2:11-2:19),OpenSquare(2:20-2:21),UpperIdent(2:21-2:24),Comma(2:24-2:25),UpperIdent(2:26-2:29),CloseSquare(2:29-2:30),
KwImport(3:1-3:7),UpperIdent(3:8-3:10),KwExposing(3:11-3:19),OpenSquare(3:20-3:21),UpperIdent(3:21-3:24),KwAs(3:25-3:27),UpperIdent(3:28-3:32),Comma(3:32-3:33),UpperIdent(3:34-3:37),KwAs(3:38-3:40),UpperIdent(3:41-3:45),CloseSquare(3:45-3:46),
UpperIdent(6:1-6:2),NoSpaceOpenRound(6:2-6:3),LowerIdent(6:3-6:4),CloseRound(6:4-6:5),OpColon(6:6-6:7),LowerIdent(6:8-6:9),KwWhere(6:10-6:15),KwModule(6:16-6:22),NoSpaceOpenRound(6:22-6:23),LowerIdent(6:23-6:24),CloseRound(6:24-6:25),NoSpaceDotLowerIdent(6:25-6:28),OpColon(6:29-6:30),OpenRound(6:31-6:32),LowerIdent(6:32-6:33),Comma(6:33-6:34),LowerIdent(6:35-6:36),CloseRound(6:36-6:37),OpArrow(6:38-6:40),UpperIdent(6:41-6:44),Comma(6:44-6:45),KwModule(6:46-6:52),NoSpaceOpenRound(6:52-6:53),LowerIdent(6:53-6:54),CloseRound(6:54-6:55),NoSpaceDotLowerIdent(6:55-6:58),OpColon(6:59-6:60),OpenRound(6:61-6:62),LowerIdent(6:62-6:63),Comma(6:63-6:64),LowerIdent(6:65-6:66),CloseRound(6:66-6:67),OpArrow(6:68-6:70),UpperIdent(6:71-6:74),
UpperIdent(7:1-7:2),NoSpaceOpenRound(7:2-7:3),LowerIdent(7:3-7:4),CloseRound(7:4-7:5),OpColon(7:6-7:7),LowerIdent(7:8-7:9),KwWhere(7:10-7:15),KwModule(7:16-7:22),NoSpaceOpenRound(7:22-7:23),LowerIdent(7:23-7:24),CloseRound(7:24-7:25),NoSpaceDotLowerIdent(7:25-7:28),OpColon(7:29-7:30),OpenRound(7:31-7:32),LowerIdent(7:32-7:33),Comma(7:33-7:34),LowerIdent(7:35-7:36),CloseRound(7:36-7:37),OpArrow(7:38-7:40),UpperIdent(7:41-7:44),Comma(7:44-7:45),KwModule(7:46-7:52),NoSpaceOpenRound(7:52-7:53),LowerIdent(7:53-7:54),CloseRound(7:54-7:55),NoSpaceDotLowerIdent(7:55-7:58),OpColon(7:59-7:60),OpenRound(7:61-7:62),LowerIdent(7:62-7:63),Comma(7:63-7:64),LowerIdent(7:65-7:66),CloseRound(7:66-7:67),OpArrow(7:68-7:70),UpperIdent(7:71-7:74),
UpperIdent(9:1-9:2),NoSpaceOpenRound(9:2-9:3),LowerIdent(9:3-9:4),Comma(9:4-9:5),LowerIdent(9:6-9:7),CloseRound(9:7-9:8),OpColon(9:9-9:10),OpenRound(9:11-9:12),LowerIdent(9:12-9:13),Comma(9:13-9:14),LowerIdent(9:15-9:16),CloseRound(9:16-9:17),
UpperIdent(10:1-10:2),NoSpaceOpenRound(10:2-10:3),LowerIdent(10:3-10:4),Comma(10:4-10:5),LowerIdent(10:6-10:7),CloseRound(10:7-10:8),OpColon(10:9-10:10),UpperIdent(10:11-10:12),NoSpaceOpenRound(10:12-10:13),LowerIdent(10:13-10:14),Comma(10:14-10:15),LowerIdent(10:16-10:17),CloseRound(10:17-10:18),
UpperIdent(11:1-11:2),OpColon(11:3-11:4),OpenCurly(11:5-11:6),LowerIdent(11:7-11:8),OpColon(11:9-11:10),UpperIdent(11:11-11:14),Comma(11:14-11:15),LowerIdent(11:16-11:17),OpColon(11:18-11:19),UpperIdent(11:20-11:23),CloseCurly(11:24-11:25),
UpperIdent(12:1-12:2),OpColon(12:3-12:4),OpenSquare(12:5-12:6),UpperIdent(12:6-12:7),Comma(12:7-12:8),UpperIdent(12:9-12:10),CloseSquare(12:10-12:11),
LowerIdent(14:1-14:2),OpColon(14:3-14:4),LowerIdent(14:5-14:6),OpArrow(14:7-14:9),LowerIdent(14:10-14:11),KwWhere(14:12-14:17),KwModule(14:18-14:24),NoSpaceOpenRound(14:24-14:25),LowerIdent(14:25-14:26),CloseRound(14:26-14:27),NoSpaceDotUpperIdent(14:27-14:29),Comma(14:29-14:30),KwModule(14:31-14:37),NoSpaceOpenRound(14:37-14:38),LowerIdent(14:38-14:39),CloseRound(14:39-14:40),NoSpaceDotUpperIdent(14:40-14:42),
LowerIdent(16:1-16:2),OpAssign(16:3-16:4),OpBar(16:5-16:6),LowerIdent(16:6-16:7),Comma(16:7-16:8),LowerIdent(16:9-16:10),OpBar(16:10-16:11),OpenCurly(16:12-16:13),
LowerIdent(17:2-17:4),OpAssign(17:5-17:6),OpenCurly(17:7-17:8),LowerIdent(17:9-17:12),OpColon(17:12-17:13),LowerIdent(17:14-17:15),Comma(17:15-17:16),LowerIdent(17:17-17:20),OpColon(17:20-17:21),LowerIdent(17:22-17:23),Comma(17:23-17:24),LowerIdent(17:25-17:28),OpColon(17:28-17:29),OpenCurly(17:30-17:31),LowerIdent(17:32-17:36),OpColon(17:36-17:37),LowerIdent(17:38-17:39),Comma(17:39-17:40),LowerIdent(17:41-17:45),OpColon(17:45-17:46),LowerIdent(17:47-17:48),CloseCurly(17:49-17:50),CloseCurly(17:51-17:52),
LowerIdent(18:2-18:4),OpAssign(18:5-18:6),LowerIdent(18:7-18:8),NoSpaceOpenRound(18:8-18:9),LowerIdent(18:9-18:10),Comma(18:10-18:11),LowerIdent(18:12-18:13),CloseRound(18:13-18:14),
LowerIdent(19:2-19:4),OpAssign(19:5-19:6),UpperIdent(19:7-19:8),NoSpaceOpenRound(19:8-19:9),LowerIdent(19:9-19:10),Comma(19:10-19:11),LowerIdent(19:12-19:13),CloseRound(19:13-19:14),
LowerIdent(20:2-20:4),OpAssign(20:5-20:6),OpenSquare(20:7-20:8),LowerIdent(20:8-20:9),Comma(20:9-20:10),LowerIdent(20:11-20:12),CloseSquare(20:12-20:13),
LowerIdent(21:2-21:4),OpAssign(21:5-21:6),OpenRound(21:7-21:8),LowerIdent(21:8-21:9),Comma(21:9-21:10),LowerIdent(21:11-21:12),CloseRound(21:12-21:13),
KwMatch(23:2-23:7),LowerIdent(23:8-23:9),OpenCurly(23:10-23:11),
UpperIdent(24:3-24:5),NoSpaceOpenRound(24:5-24:6),NoSpaceOpenRound(24:6-24:7),LowerIdent(24:7-24:8),Comma(24:8-24:9),LowerIdent(24:10-24:11),CloseRound(24:11-24:12),CloseRound(24:12-24:13),OpFatArrow(24:14-24:16),LowerIdent(24:17-24:18),
UpperIdent(25:3-25:5),NoSpaceOpenRound(25:5-25:6),LowerIdent(25:6-25:7),Comma(25:7-25:8),LowerIdent(25:9-25:10),CloseRound(25:10-25:11),OpFatArrow(25:12-25:14),LowerIdent(25:15-25:16),
UpperIdent(26:3-26:5),NoSpaceOpenRound(26:5-26:6),OpenCurly(26:6-26:7),LowerIdent(26:8-26:9),Comma(26:9-26:10),LowerIdent(26:11-26:12),CloseCurly(26:13-26:14),CloseRound(26:14-26:15),OpFatArrow(26:16-26:18),LowerIdent(26:19-26:20),
UpperIdent(27:3-27:5),NoSpaceOpenRound(27:5-27:6),OpenSquare(27:6-27:7),LowerIdent(27:7-27:8),Comma(27:8-27:9),LowerIdent(27:10-27:11),CloseSquare(27:11-27:12),CloseRound(27:12-27:13),OpFatArrow(27:14-27:16),LowerIdent(27:17-27:18),
CloseCurly(28:2-28:3),
CloseCurly(29:1-29:2),
EndOfFile(30:1-30:1),
~~~
# PARSE
~~~clojure
(file @2.1-29.2
	(type-module @2.1-2.7)
	(statements
		(s-import @2.1-2.30 (raw "I1")
			(exposing
				(exposed-upper-ident @2.21-2.24 (text "I11"))
				(exposed-upper-ident @2.26-2.29 (text "I12"))))
		(s-import @3.1-3.46 (raw "I2")
			(exposing
				(exposed-upper-ident @3.21-3.32 (text "I21") (as "Ias1"))
				(exposed-upper-ident @3.34-3.45 (text "I22") (as "Ias2"))))
		(s-type-decl @6.1-6.74
			(header @6.1-6.5 (name "A")
				(args
					(ty-var @6.3-6.4 (raw "a"))))
			(ty-var @6.8-6.9 (raw "a")))
		(s-type-decl @7.1-7.74
			(header @7.1-7.5 (name "B")
				(args
					(ty-var @7.3-7.4 (raw "b"))))
			(ty-var @7.8-7.9 (raw "b")))
		(s-type-decl @9.1-9.17
			(header @9.1-9.8 (name "C")
				(args
					(ty-var @9.3-9.4 (raw "a"))
					(ty-var @9.6-9.7 (raw "b"))))
			(ty-tuple @9.11-9.17
				(ty-var @9.12-9.13 (raw "a"))
				(ty-var @9.15-9.16 (raw "b"))))
		(s-type-decl @10.1-10.18
			(header @10.1-10.8 (name "D")
				(args
					(ty-var @10.3-10.4 (raw "a"))
					(ty-var @10.6-10.7 (raw "b"))))
			(ty-apply @10.11-10.18
				(ty @10.11-10.12 (name "C"))
				(ty-var @10.13-10.14 (raw "a"))
				(ty-var @10.16-10.17 (raw "b"))))
		(s-type-decl @11.1-11.25
			(header @11.1-11.2 (name "E")
				(args))
			(ty-record @11.5-11.25
				(anno-record-field @11.7-11.14 (name "a")
					(ty @11.11-11.14 (name "Str")))
				(anno-record-field @11.16-11.23 (name "b")
					(ty @11.20-11.23 (name "Str")))))
		(s-type-decl @12.1-12.11
			(header @12.1-12.2 (name "F")
				(args))
			(ty-tag-union @12.5-12.11
				(tags
					(ty @12.6-12.7 (name "A"))
					(ty @12.9-12.10 (name "B")))))
		(s-type-anno @14.1-14.42 (name "g")
			(ty-fn @14.5-14.11
				(ty-var @14.5-14.6 (raw "e"))
				(ty-var @14.10-14.11 (raw "e")))
			(where
				(alias @14.18-14.29 (module-of "e") (name "A"))
				(alias @14.31-14.42 (module-of "e") (name "B"))))
		(s-decl @16.1-29.2
			(p-ident @16.1-16.2 (raw "h"))
			(e-lambda @16.5-29.2
				(args
					(p-ident @16.6-16.7 (raw "x"))
					(p-ident @16.9-16.10 (raw "y")))
				(e-block @16.12-29.2
					(statements
						(s-decl @17.2-17.52
							(p-ident @17.2-17.4 (raw "h1"))
							(e-record @17.7-17.52
								(field (field "h11")
									(e-ident @17.14-17.15 (raw "x")))
								(field (field "h12")
									(e-ident @17.22-17.23 (raw "x")))
								(field (field "h13")
									(e-record @17.30-17.50
										(field (field "h131")
											(e-ident @17.38-17.39 (raw "x")))
										(field (field "h132")
											(e-ident @17.47-17.48 (raw "y")))))))
						(s-decl @18.2-18.14
							(p-ident @18.2-18.4 (raw "h2"))
							(e-apply @18.7-18.14
								(e-ident @18.7-18.8 (raw "h"))
								(e-ident @18.9-18.10 (raw "x"))
								(e-ident @18.12-18.13 (raw "y"))))
						(s-decl @19.2-19.14
							(p-ident @19.2-19.4 (raw "h3"))
							(e-apply @19.7-19.14
								(e-tag @19.7-19.8 (raw "A"))
								(e-ident @19.9-19.10 (raw "x"))
								(e-ident @19.12-19.13 (raw "y"))))
						(s-decl @20.2-20.13
							(p-ident @20.2-20.4 (raw "h4"))
							(e-list @20.7-20.13
								(e-ident @20.8-20.9 (raw "x"))
								(e-ident @20.11-20.12 (raw "y"))))
						(s-decl @21.2-21.13
							(p-ident @21.2-21.4 (raw "h5"))
							(e-tuple @21.7-21.13
								(e-ident @21.8-21.9 (raw "x"))
								(e-ident @21.11-21.12 (raw "y"))))
						(e-match
							(e-ident @23.8-23.9 (raw "x"))
							(branches
								(branch @24.3-24.18
									(p-tag @24.3-24.13 (raw "Z1")
										(p-tuple @24.6-24.12
											(p-ident @24.7-24.8 (raw "a"))
											(p-ident @24.10-24.11 (raw "b"))))
									(e-ident @24.17-24.18 (raw "a")))
								(branch @25.3-25.16
									(p-tag @25.3-25.11 (raw "Z2")
										(p-ident @25.6-25.7 (raw "a"))
										(p-ident @25.9-25.10 (raw "b")))
									(e-ident @25.15-25.16 (raw "a")))
								(branch @26.3-26.20
									(p-tag @26.3-26.15 (raw "Z3")
										(p-record @26.6-26.14
											(field @26.8-26.9 (name "a") (rest false))
											(field @26.11-26.12 (name "b") (rest false))))
									(e-ident @26.19-26.20 (raw "a")))
								(branch @27.3-27.18
									(p-tag @27.3-27.13 (raw "Z4")
										(p-list @27.6-27.12
											(p-ident @27.7-27.8 (raw "a"))
											(p-ident @27.10-27.11 (raw "b"))))
									(e-ident @27.17-27.18 (raw "a")))))))))))
~~~
# FORMATTED
~~~roc
# Import exposing
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
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @16.1-16.2 (ident "h"))
		(e-closure @16.5-29.2
			(captures
				(capture @25.6-25.7 (ident "a"))
				(capture @27.7-27.8 (ident "a"))
				(capture @16.1-16.2 (ident "h"))
				(capture @26.8-26.9 (ident "a"))
				(capture @24.7-24.8 (ident "a")))
			(e-lambda @16.5-29.2
				(args
					(p-assign @16.6-16.7 (ident "x"))
					(p-assign @16.9-16.10 (ident "y")))
				(e-block @16.12-29.2
					(s-let @17.2-17.52
						(p-assign @17.2-17.4 (ident "h1"))
						(e-record @17.7-17.52
							(fields
								(field (name "h11")
									(e-lookup-local @17.14-17.15
										(p-assign @16.6-16.7 (ident "x"))))
								(field (name "h12")
									(e-lookup-local @17.22-17.23
										(p-assign @16.6-16.7 (ident "x"))))
								(field (name "h13")
									(e-record @17.30-17.50
										(fields
											(field (name "h131")
												(e-lookup-local @17.38-17.39
													(p-assign @16.6-16.7 (ident "x"))))
											(field (name "h132")
												(e-lookup-local @17.47-17.48
													(p-assign @16.9-16.10 (ident "y"))))))))))
					(s-let @18.2-18.14
						(p-assign @18.2-18.4 (ident "h2"))
						(e-call @18.7-18.14
							(e-lookup-local @18.7-18.8
								(p-assign @16.1-16.2 (ident "h")))
							(e-lookup-local @18.9-18.10
								(p-assign @16.6-16.7 (ident "x")))
							(e-lookup-local @18.12-18.13
								(p-assign @16.9-16.10 (ident "y")))))
					(s-let @19.2-19.14
						(p-assign @19.2-19.4 (ident "h3"))
						(e-tag @19.7-19.14 (name "A")
							(args
								(e-lookup-local @19.9-19.10
									(p-assign @16.6-16.7 (ident "x")))
								(e-lookup-local @19.12-19.13
									(p-assign @16.9-16.10 (ident "y"))))))
					(s-let @20.2-20.13
						(p-assign @20.2-20.4 (ident "h4"))
						(e-list @20.7-20.13
							(elems
								(e-lookup-local @20.8-20.9
									(p-assign @16.6-16.7 (ident "x")))
								(e-lookup-local @20.11-20.12
									(p-assign @16.9-16.10 (ident "y"))))))
					(s-let @21.2-21.13
						(p-assign @21.2-21.4 (ident "h5"))
						(e-tuple @21.7-21.13
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
											(p-applied-tag @24.3-24.13)))
									(value
										(e-lookup-local @24.17-24.18
											(p-assign @24.7-24.8 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @25.3-25.11)))
									(value
										(e-lookup-local @25.15-25.16
											(p-assign @25.6-25.7 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @26.3-26.15)))
									(value
										(e-lookup-local @26.19-26.20
											(p-assign @26.8-26.9 (ident "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @27.3-27.13)))
									(value
										(e-lookup-local @27.17-27.18
											(p-assign @27.7-27.8 (ident "a"))))))))))))
	(s-alias-decl @6.1-6.74
		(ty-header @6.1-6.5 (name "A")
			(ty-args
				(ty-rigid-var @6.3-6.4 (name "a"))))
		(ty-rigid-var-lookup (ty-rigid-var @6.3-6.4 (name "a"))))
	(s-alias-decl @7.1-7.74
		(ty-header @7.1-7.5 (name "B")
			(ty-args
				(ty-rigid-var @7.3-7.4 (name "b"))))
		(ty-rigid-var-lookup (ty-rigid-var @7.3-7.4 (name "b"))))
	(s-alias-decl @9.1-9.17
		(ty-header @9.1-9.8 (name "C")
			(ty-args
				(ty-rigid-var @9.3-9.4 (name "a"))
				(ty-rigid-var @9.6-9.7 (name "b"))))
		(ty-tuple @9.11-9.17
			(ty-rigid-var-lookup (ty-rigid-var @9.3-9.4 (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var @9.6-9.7 (name "b")))))
	(s-alias-decl @10.1-10.18
		(ty-header @10.1-10.8 (name "D")
			(ty-args
				(ty-rigid-var @10.3-10.4 (name "a"))
				(ty-rigid-var @10.6-10.7 (name "b"))))
		(ty-apply @10.11-10.18 (name "C") (local)
			(ty-rigid-var-lookup (ty-rigid-var @10.3-10.4 (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var @10.6-10.7 (name "b")))))
	(s-alias-decl @11.1-11.25
		(ty-header @11.1-11.2 (name "E"))
		(ty-record @11.5-11.25
			(field (field "a")
				(ty-lookup @11.11-11.14 (name "Str") (builtin)))
			(field (field "b")
				(ty-lookup @11.20-11.23 (name "Str") (builtin)))))
	(s-alias-decl @12.1-12.11
		(ty-header @12.1-12.2 (name "F"))
		(ty-tag-union @12.5-12.11
			(ty-tag-name @12.6-12.7 (name "A"))
			(ty-tag-name @12.9-12.10 (name "B"))))
	(s-import @2.1-2.30 (module "I1")
		(exposes
			(exposed (name "I11") (wildcard false))
			(exposed (name "I12") (wildcard false))))
	(s-import @3.1-3.46 (module "I2")
		(exposes
			(exposed (name "I21") (alias "Ias1") (wildcard false))
			(exposed (name "I22") (alias "Ias2") (wildcard false))))
	(s-type-anno @14.1-14.42 (name "g")
		(ty-fn @14.5-14.11 (effectful false)
			(ty-rigid-var @14.5-14.6 (name "e"))
			(ty-rigid-var-lookup (ty-rigid-var @14.5-14.6 (name "e"))))
		(where
			(alias @14.18-14.29 (module-of "e") (ident "A"))
			(alias @14.31-14.42 (module-of "e") (ident "B"))))
	(ext-decl @14.18-14.29 (ident "module(e).A") (kind "type"))
	(ext-decl @14.31-14.42 (ident "module(e).B") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @16.1-16.2 (type "[Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j, [Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j -> c")))
	(type_decls
		(alias @6.1-6.74 (type "A(a)")
			(ty-header @6.1-6.5 (name "A")
				(ty-args
					(ty-rigid-var @6.3-6.4 (name "a")))))
		(alias @7.1-7.74 (type "B(b)")
			(ty-header @7.1-7.5 (name "B")
				(ty-args
					(ty-rigid-var @7.3-7.4 (name "b")))))
		(alias @9.1-9.17 (type "C(a, b)")
			(ty-header @9.1-9.8 (name "C")
				(ty-args
					(ty-rigid-var @9.3-9.4 (name "a"))
					(ty-rigid-var @9.6-9.7 (name "b")))))
		(alias @10.1-10.18 (type "D(a, b)")
			(ty-header @10.1-10.8 (name "D")
				(ty-args
					(ty-rigid-var @10.3-10.4 (name "a"))
					(ty-rigid-var @10.6-10.7 (name "b")))))
		(alias @11.1-11.25 (type "E")
			(ty-header @11.1-11.2 (name "E")))
		(alias @12.1-12.11 (type "F")
			(ty-header @12.1-12.2 (name "F"))))
	(expressions
		(expr @16.5-29.2 (type "[Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j, [Z1((c, d)), Z2(c, f), Z3({ a: c, b: i }), Z4(List(c))]j -> c"))))
~~~
