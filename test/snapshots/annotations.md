# META
~~~ini
description=Example of a nominal tag union with a payload
type=file
~~~
# SOURCE
~~~roc
module []

Pair(a) := [Pair(a, a)]

pairU64 : Pair(U64)
pairU64 = Pair.Pair(1, 2)

pairStr : Pair(Str)
pairStr = Pair.Pair("hello", "world")

mkPair : a, a -> Pair(a)
mkPair = |x, y| Pair.Pair(x, y)

succeedPairSameType : Pair(U8)
succeedPairSameType = mkPair(1, 2)

failPairDiffTypes : Pair(U8)
failPairDiffTypes = mkPair("1", 2)

failPairDiffTypes2 : Pair(U64)
failPairDiffTypes2 = Pair.Pair(1, "str")

mkPairInvalid : a, b -> Pair(a)
mkPairInvalid = |x, y| Pair.Pair(x, y)
~~~
# EXPECTED
TYPE MISMATCH - annotations.md:18:28:18:28
INVALID NOMINAL TAG - annotations.md:21:22:21:41
INVALID NOMINAL TAG - annotations.md:24:24:24:39
# PROBLEMS
**TYPE MISMATCH**
The first and second arguments to `mkPair` must have compatible types, but they are incompatible in this call:
**annotations.md:18:28:**
```roc
failPairDiffTypes = mkPair("1", 2)
```
                           ^^^  ^

The first argument has the type:
    _Str_

But the second argument has the type:
    _Num(_size)_

`mkPair` needs these arguments to have compatible types.

**INVALID NOMINAL TAG**
I'm having trouble with this nominal tag:
**annotations.md:21:22:21:41:**
```roc
failPairDiffTypes2 = Pair.Pair(1, "str")
```
                     ^^^^^^^^^^^^^^^^^^^

The tag is:
    _Pair(Num(_size), Str)_

But the nominal type needs it to be:
    _Pair(Num(_size), Num(_size2))_

**INVALID NOMINAL TAG**
I'm having trouble with this nominal tag:
**annotations.md:24:24:24:39:**
```roc
mkPairInvalid = |x, y| Pair.Pair(x, y)
```
                       ^^^^^^^^^^^^^^^

The tag is:
    _Pair(a, b)_

But the nominal type needs it to be:
    _Pair(a, a)_

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(3:1-3:5),NoSpaceOpenRound(3:5-3:6),LowerIdent(3:6-3:7),CloseRound(3:7-3:8),OpColonEqual(3:9-3:11),OpenSquare(3:12-3:13),UpperIdent(3:13-3:17),NoSpaceOpenRound(3:17-3:18),LowerIdent(3:18-3:19),Comma(3:19-3:20),LowerIdent(3:21-3:22),CloseRound(3:22-3:23),CloseSquare(3:23-3:24),
LowerIdent(5:1-5:8),OpColon(5:9-5:10),UpperIdent(5:11-5:15),NoSpaceOpenRound(5:15-5:16),UpperIdent(5:16-5:19),CloseRound(5:19-5:20),
LowerIdent(6:1-6:8),OpAssign(6:9-6:10),UpperIdent(6:11-6:15),NoSpaceDotUpperIdent(6:15-6:20),NoSpaceOpenRound(6:20-6:21),Int(6:21-6:22),Comma(6:22-6:23),Int(6:24-6:25),CloseRound(6:25-6:26),
LowerIdent(8:1-8:8),OpColon(8:9-8:10),UpperIdent(8:11-8:15),NoSpaceOpenRound(8:15-8:16),UpperIdent(8:16-8:19),CloseRound(8:19-8:20),
LowerIdent(9:1-9:8),OpAssign(9:9-9:10),UpperIdent(9:11-9:15),NoSpaceDotUpperIdent(9:15-9:20),NoSpaceOpenRound(9:20-9:21),StringStart(9:21-9:22),StringPart(9:22-9:27),StringEnd(9:27-9:28),Comma(9:28-9:29),StringStart(9:30-9:31),StringPart(9:31-9:36),StringEnd(9:36-9:37),CloseRound(9:37-9:38),
LowerIdent(11:1-11:7),OpColon(11:8-11:9),LowerIdent(11:10-11:11),Comma(11:11-11:12),LowerIdent(11:13-11:14),OpArrow(11:15-11:17),UpperIdent(11:18-11:22),NoSpaceOpenRound(11:22-11:23),LowerIdent(11:23-11:24),CloseRound(11:24-11:25),
LowerIdent(12:1-12:7),OpAssign(12:8-12:9),OpBar(12:10-12:11),LowerIdent(12:11-12:12),Comma(12:12-12:13),LowerIdent(12:14-12:15),OpBar(12:15-12:16),UpperIdent(12:17-12:21),NoSpaceDotUpperIdent(12:21-12:26),NoSpaceOpenRound(12:26-12:27),LowerIdent(12:27-12:28),Comma(12:28-12:29),LowerIdent(12:30-12:31),CloseRound(12:31-12:32),
LowerIdent(14:1-14:20),OpColon(14:21-14:22),UpperIdent(14:23-14:27),NoSpaceOpenRound(14:27-14:28),UpperIdent(14:28-14:30),CloseRound(14:30-14:31),
LowerIdent(15:1-15:20),OpAssign(15:21-15:22),LowerIdent(15:23-15:29),NoSpaceOpenRound(15:29-15:30),Int(15:30-15:31),Comma(15:31-15:32),Int(15:33-15:34),CloseRound(15:34-15:35),
LowerIdent(17:1-17:18),OpColon(17:19-17:20),UpperIdent(17:21-17:25),NoSpaceOpenRound(17:25-17:26),UpperIdent(17:26-17:28),CloseRound(17:28-17:29),
LowerIdent(18:1-18:18),OpAssign(18:19-18:20),LowerIdent(18:21-18:27),NoSpaceOpenRound(18:27-18:28),StringStart(18:28-18:29),StringPart(18:29-18:30),StringEnd(18:30-18:31),Comma(18:31-18:32),Int(18:33-18:34),CloseRound(18:34-18:35),
LowerIdent(20:1-20:19),OpColon(20:20-20:21),UpperIdent(20:22-20:26),NoSpaceOpenRound(20:26-20:27),UpperIdent(20:27-20:30),CloseRound(20:30-20:31),
LowerIdent(21:1-21:19),OpAssign(21:20-21:21),UpperIdent(21:22-21:26),NoSpaceDotUpperIdent(21:26-21:31),NoSpaceOpenRound(21:31-21:32),Int(21:32-21:33),Comma(21:33-21:34),StringStart(21:35-21:36),StringPart(21:36-21:39),StringEnd(21:39-21:40),CloseRound(21:40-21:41),
LowerIdent(23:1-23:14),OpColon(23:15-23:16),LowerIdent(23:17-23:18),Comma(23:18-23:19),LowerIdent(23:20-23:21),OpArrow(23:22-23:24),UpperIdent(23:25-23:29),NoSpaceOpenRound(23:29-23:30),LowerIdent(23:30-23:31),CloseRound(23:31-23:32),
LowerIdent(24:1-24:14),OpAssign(24:15-24:16),OpBar(24:17-24:18),LowerIdent(24:18-24:19),Comma(24:19-24:20),LowerIdent(24:21-24:22),OpBar(24:22-24:23),UpperIdent(24:24-24:28),NoSpaceDotUpperIdent(24:28-24:33),NoSpaceOpenRound(24:33-24:34),LowerIdent(24:34-24:35),Comma(24:35-24:36),LowerIdent(24:37-24:38),CloseRound(24:38-24:39),
EndOfFile(25:1-25:1),
~~~
# PARSE
~~~clojure
(file @1.1-24.39
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-3.24
			(header @3.1-3.8 (name "Pair")
				(args
					(ty-var @3.6-3.7 (raw "a"))))
			(ty-tag-union @3.12-3.24
				(tags
					(ty-apply @3.13-3.23
						(ty @3.13-3.17 (name "Pair"))
						(ty-var @3.18-3.19 (raw "a"))
						(ty-var @3.21-3.22 (raw "a"))))))
		(s-type-anno @5.1-5.20 (name "pairU64")
			(ty-apply @5.11-5.20
				(ty @5.11-5.15 (name "Pair"))
				(ty @5.16-5.19 (name "U64"))))
		(s-decl @6.1-6.26
			(p-ident @6.1-6.8 (raw "pairU64"))
			(e-apply @6.11-6.26
				(e-tag @6.11-6.20 (raw "Pair.Pair"))
				(e-int @6.21-6.22 (raw "1"))
				(e-int @6.24-6.25 (raw "2"))))
		(s-type-anno @8.1-8.20 (name "pairStr")
			(ty-apply @8.11-8.20
				(ty @8.11-8.15 (name "Pair"))
				(ty @8.16-8.19 (name "Str"))))
		(s-decl @9.1-9.38
			(p-ident @9.1-9.8 (raw "pairStr"))
			(e-apply @9.11-9.38
				(e-tag @9.11-9.20 (raw "Pair.Pair"))
				(e-string @9.21-9.28
					(e-string-part @9.22-9.27 (raw "hello")))
				(e-string @9.30-9.37
					(e-string-part @9.31-9.36 (raw "world")))))
		(s-type-anno @11.1-11.25 (name "mkPair")
			(ty-fn @11.10-11.25
				(ty-var @11.10-11.11 (raw "a"))
				(ty-var @11.13-11.14 (raw "a"))
				(ty-apply @11.18-11.25
					(ty @11.18-11.22 (name "Pair"))
					(ty-var @11.23-11.24 (raw "a")))))
		(s-decl @12.1-12.32
			(p-ident @12.1-12.7 (raw "mkPair"))
			(e-lambda @12.10-12.32
				(args
					(p-ident @12.11-12.12 (raw "x"))
					(p-ident @12.14-12.15 (raw "y")))
				(e-apply @12.17-12.32
					(e-tag @12.17-12.26 (raw "Pair.Pair"))
					(e-ident @12.27-12.28 (raw "x"))
					(e-ident @12.30-12.31 (raw "y")))))
		(s-type-anno @14.1-14.31 (name "succeedPairSameType")
			(ty-apply @14.23-14.31
				(ty @14.23-14.27 (name "Pair"))
				(ty @14.28-14.30 (name "U8"))))
		(s-decl @15.1-15.35
			(p-ident @15.1-15.20 (raw "succeedPairSameType"))
			(e-apply @15.23-15.35
				(e-ident @15.23-15.29 (raw "mkPair"))
				(e-int @15.30-15.31 (raw "1"))
				(e-int @15.33-15.34 (raw "2"))))
		(s-type-anno @17.1-17.29 (name "failPairDiffTypes")
			(ty-apply @17.21-17.29
				(ty @17.21-17.25 (name "Pair"))
				(ty @17.26-17.28 (name "U8"))))
		(s-decl @18.1-18.35
			(p-ident @18.1-18.18 (raw "failPairDiffTypes"))
			(e-apply @18.21-18.35
				(e-ident @18.21-18.27 (raw "mkPair"))
				(e-string @18.28-18.31
					(e-string-part @18.29-18.30 (raw "1")))
				(e-int @18.33-18.34 (raw "2"))))
		(s-type-anno @20.1-20.31 (name "failPairDiffTypes2")
			(ty-apply @20.22-20.31
				(ty @20.22-20.26 (name "Pair"))
				(ty @20.27-20.30 (name "U64"))))
		(s-decl @21.1-21.41
			(p-ident @21.1-21.19 (raw "failPairDiffTypes2"))
			(e-apply @21.22-21.41
				(e-tag @21.22-21.31 (raw "Pair.Pair"))
				(e-int @21.32-21.33 (raw "1"))
				(e-string @21.35-21.40
					(e-string-part @21.36-21.39 (raw "str")))))
		(s-type-anno @23.1-23.32 (name "mkPairInvalid")
			(ty-fn @23.17-23.32
				(ty-var @23.17-23.18 (raw "a"))
				(ty-var @23.20-23.21 (raw "b"))
				(ty-apply @23.25-23.32
					(ty @23.25-23.29 (name "Pair"))
					(ty-var @23.30-23.31 (raw "a")))))
		(s-decl @24.1-24.39
			(p-ident @24.1-24.14 (raw "mkPairInvalid"))
			(e-lambda @24.17-24.39
				(args
					(p-ident @24.18-24.19 (raw "x"))
					(p-ident @24.21-24.22 (raw "y")))
				(e-apply @24.24-24.39
					(e-tag @24.24-24.33 (raw "Pair.Pair"))
					(e-ident @24.34-24.35 (raw "x"))
					(e-ident @24.37-24.38 (raw "y")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.8 (ident "pairU64"))
		(e-nominal @6.11-6.26 (nominal "Pair")
			(e-tag @6.11-6.26 (name "Pair")
				(args
					(e-num @6.21-6.22 (value "1"))
					(e-num @6.24-6.25 (value "2")))))
		(annotation @6.1-6.8
			(declared-type
				(ty-apply @5.11-5.20 (name "Pair") (local)
					(ty-lookup @5.11-5.20 (name "U64") (builtin))))))
	(d-let
		(p-assign @9.1-9.8 (ident "pairStr"))
		(e-nominal @9.11-9.38 (nominal "Pair")
			(e-tag @9.11-9.38 (name "Pair")
				(args
					(e-string @9.21-9.28
						(e-literal @9.22-9.27 (string "hello")))
					(e-string @9.30-9.37
						(e-literal @9.31-9.36 (string "world"))))))
		(annotation @9.1-9.8
			(declared-type
				(ty-apply @8.11-8.20 (name "Pair") (local)
					(ty-lookup @8.11-8.20 (name "Str") (builtin))))))
	(d-let
		(p-assign @12.1-12.7 (ident "mkPair"))
		(e-lambda @12.10-12.32
			(args
				(p-assign @12.11-12.12 (ident "x"))
				(p-assign @12.14-12.15 (ident "y")))
			(e-nominal @12.17-12.32 (nominal "Pair")
				(e-tag @12.17-12.32 (name "Pair")
					(args
						(e-lookup-local @12.27-12.28
							(p-assign @12.11-12.12 (ident "x")))
						(e-lookup-local @12.30-12.31
							(p-assign @12.14-12.15 (ident "y")))))))
		(annotation @12.1-12.7
			(declared-type
				(ty-fn @11.10-11.25 (effectful false)
					(ty-rigid-var @11.10-11.11 (name "a"))
					(ty-rigid-var @11.10-11.11 (name "a"))
					(ty-apply @11.18-11.25 (name "Pair") (local)
						(ty-rigid-var @11.10-11.11 (name "a")))))))
	(d-let
		(p-assign @15.1-15.20 (ident "succeedPairSameType"))
		(e-call @15.23-15.35
			(e-num @15.30-15.31 (value "1"))
			(e-num @15.33-15.34 (value "2")))
		(annotation @15.1-15.20
			(declared-type
				(ty-apply @14.23-14.31 (name "Pair") (local)
					(ty-lookup @14.23-14.31 (name "U8") (builtin))))))
	(d-let
		(p-assign @18.1-18.18 (ident "failPairDiffTypes"))
		(e-call @18.21-18.35
			(e-string @18.28-18.31
				(e-literal @18.29-18.30 (string "1")))
			(e-num @18.33-18.34 (value "2")))
		(annotation @18.1-18.18
			(declared-type
				(ty-apply @17.21-17.29 (name "Pair") (local)
					(ty-lookup @17.21-17.29 (name "U8") (builtin))))))
	(d-let
		(p-assign @21.1-21.19 (ident "failPairDiffTypes2"))
		(e-nominal @21.22-21.41 (nominal "Pair")
			(e-tag @21.22-21.41 (name "Pair")
				(args
					(e-num @21.32-21.33 (value "1"))
					(e-string @21.35-21.40
						(e-literal @21.36-21.39 (string "str"))))))
		(annotation @21.1-21.19
			(declared-type
				(ty-apply @20.22-20.31 (name "Pair") (local)
					(ty-lookup @20.22-20.31 (name "U64") (builtin))))))
	(d-let
		(p-assign @24.1-24.14 (ident "mkPairInvalid"))
		(e-lambda @24.17-24.39
			(args
				(p-assign @24.18-24.19 (ident "x"))
				(p-assign @24.21-24.22 (ident "y")))
			(e-nominal @24.24-24.39 (nominal "Pair")
				(e-tag @24.24-24.39 (name "Pair")
					(args
						(e-lookup-local @24.34-24.35
							(p-assign @24.18-24.19 (ident "x")))
						(e-lookup-local @24.37-24.38
							(p-assign @24.21-24.22 (ident "y")))))))
		(annotation @24.1-24.14
			(declared-type
				(ty-fn @23.17-23.32 (effectful false)
					(ty-rigid-var @23.17-23.18 (name "a"))
					(ty-rigid-var @23.20-23.21 (name "b"))
					(ty-apply @23.25-23.32 (name "Pair") (local)
						(ty-rigid-var @23.17-23.18 (name "a")))))))
	(s-nominal-decl @3.1-3.24
		(ty-header @3.1-3.8 (name "Pair")
			(ty-args
				(ty-rigid-var @3.6-3.7 (name "a"))))
		(ty-tag-union @3.12-3.24
			(tag_name @3.13-3.23 (name "Pair")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.8 (type "Pair(Num(Int(Unsigned64)))"))
		(patt @9.1-9.8 (type "Pair(Str)"))
		(patt @12.1-12.7 (type "a, a -> Pair(a)"))
		(patt @15.1-15.20 (type "Pair(Num(Int(Unsigned8)))"))
		(patt @18.1-18.18 (type "Error"))
		(patt @21.1-21.19 (type "Error"))
		(patt @24.1-24.14 (type "a, b -> Error")))
	(type_decls
		(nominal @3.1-3.24 (type "Pair(a)")
			(ty-header @3.1-3.8 (name "Pair")
				(ty-args
					(ty-rigid-var @3.6-3.7 (name "a"))))))
	(expressions
		(expr @6.11-6.26 (type "Pair(Num(Int(Unsigned64)))"))
		(expr @9.11-9.38 (type "Pair(Str)"))
		(expr @12.10-12.32 (type "a, a -> Pair(a)"))
		(expr @15.23-15.35 (type "Pair(Num(Int(Unsigned8)))"))
		(expr @18.21-18.35 (type "Error"))
		(expr @21.22-21.41 (type "Error"))
		(expr @24.17-24.39 (type "a, b -> Error"))))
~~~
