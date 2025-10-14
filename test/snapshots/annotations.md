# META
~~~ini
description=Example of a nominal tag union with a payload
type=snippet
~~~
# SOURCE
~~~roc
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
TYPE MISMATCH - annotations.md:16:28:16:28
INVALID NOMINAL TAG - annotations.md:19:22:19:41
INVALID NOMINAL TAG - annotations.md:22:24:22:39
# PROBLEMS
**TYPE MISMATCH**
The first and second arguments to `mkPair` must have compatible types, but they are incompatible in this call:
**annotations.md:16:28:**
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
**annotations.md:19:22:19:41:**
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
**annotations.md:22:24:22:39:**
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
UpperIdent(1:1-1:5),NoSpaceOpenRound(1:5-1:6),LowerIdent(1:6-1:7),CloseRound(1:7-1:8),OpColonEqual(1:9-1:11),OpenSquare(1:12-1:13),UpperIdent(1:13-1:17),NoSpaceOpenRound(1:17-1:18),LowerIdent(1:18-1:19),Comma(1:19-1:20),LowerIdent(1:21-1:22),CloseRound(1:22-1:23),CloseSquare(1:23-1:24),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),UpperIdent(3:11-3:15),NoSpaceOpenRound(3:15-3:16),UpperIdent(3:16-3:19),CloseRound(3:19-3:20),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),UpperIdent(4:11-4:15),NoSpaceDotUpperIdent(4:15-4:20),NoSpaceOpenRound(4:20-4:21),Int(4:21-4:22),Comma(4:22-4:23),Int(4:24-4:25),CloseRound(4:25-4:26),
LowerIdent(6:1-6:8),OpColon(6:9-6:10),UpperIdent(6:11-6:15),NoSpaceOpenRound(6:15-6:16),UpperIdent(6:16-6:19),CloseRound(6:19-6:20),
LowerIdent(7:1-7:8),OpAssign(7:9-7:10),UpperIdent(7:11-7:15),NoSpaceDotUpperIdent(7:15-7:20),NoSpaceOpenRound(7:20-7:21),StringStart(7:21-7:22),StringPart(7:22-7:27),StringEnd(7:27-7:28),Comma(7:28-7:29),StringStart(7:30-7:31),StringPart(7:31-7:36),StringEnd(7:36-7:37),CloseRound(7:37-7:38),
LowerIdent(9:1-9:7),OpColon(9:8-9:9),LowerIdent(9:10-9:11),Comma(9:11-9:12),LowerIdent(9:13-9:14),OpArrow(9:15-9:17),UpperIdent(9:18-9:22),NoSpaceOpenRound(9:22-9:23),LowerIdent(9:23-9:24),CloseRound(9:24-9:25),
LowerIdent(10:1-10:7),OpAssign(10:8-10:9),OpBar(10:10-10:11),LowerIdent(10:11-10:12),Comma(10:12-10:13),LowerIdent(10:14-10:15),OpBar(10:15-10:16),UpperIdent(10:17-10:21),NoSpaceDotUpperIdent(10:21-10:26),NoSpaceOpenRound(10:26-10:27),LowerIdent(10:27-10:28),Comma(10:28-10:29),LowerIdent(10:30-10:31),CloseRound(10:31-10:32),
LowerIdent(12:1-12:20),OpColon(12:21-12:22),UpperIdent(12:23-12:27),NoSpaceOpenRound(12:27-12:28),UpperIdent(12:28-12:30),CloseRound(12:30-12:31),
LowerIdent(13:1-13:20),OpAssign(13:21-13:22),LowerIdent(13:23-13:29),NoSpaceOpenRound(13:29-13:30),Int(13:30-13:31),Comma(13:31-13:32),Int(13:33-13:34),CloseRound(13:34-13:35),
LowerIdent(15:1-15:18),OpColon(15:19-15:20),UpperIdent(15:21-15:25),NoSpaceOpenRound(15:25-15:26),UpperIdent(15:26-15:28),CloseRound(15:28-15:29),
LowerIdent(16:1-16:18),OpAssign(16:19-16:20),LowerIdent(16:21-16:27),NoSpaceOpenRound(16:27-16:28),StringStart(16:28-16:29),StringPart(16:29-16:30),StringEnd(16:30-16:31),Comma(16:31-16:32),Int(16:33-16:34),CloseRound(16:34-16:35),
LowerIdent(18:1-18:19),OpColon(18:20-18:21),UpperIdent(18:22-18:26),NoSpaceOpenRound(18:26-18:27),UpperIdent(18:27-18:30),CloseRound(18:30-18:31),
LowerIdent(19:1-19:19),OpAssign(19:20-19:21),UpperIdent(19:22-19:26),NoSpaceDotUpperIdent(19:26-19:31),NoSpaceOpenRound(19:31-19:32),Int(19:32-19:33),Comma(19:33-19:34),StringStart(19:35-19:36),StringPart(19:36-19:39),StringEnd(19:39-19:40),CloseRound(19:40-19:41),
LowerIdent(21:1-21:14),OpColon(21:15-21:16),LowerIdent(21:17-21:18),Comma(21:18-21:19),LowerIdent(21:20-21:21),OpArrow(21:22-21:24),UpperIdent(21:25-21:29),NoSpaceOpenRound(21:29-21:30),LowerIdent(21:30-21:31),CloseRound(21:31-21:32),
LowerIdent(22:1-22:14),OpAssign(22:15-22:16),OpBar(22:17-22:18),LowerIdent(22:18-22:19),Comma(22:19-22:20),LowerIdent(22:21-22:22),OpBar(22:22-22:23),UpperIdent(22:24-22:28),NoSpaceDotUpperIdent(22:28-22:33),NoSpaceOpenRound(22:33-22:34),LowerIdent(22:34-22:35),Comma(22:35-22:36),LowerIdent(22:37-22:38),CloseRound(22:38-22:39),
EndOfFile(23:1-23:1),
~~~
# PARSE
~~~clojure
(file @1.1-22.39
	(type-module @1.1-1.5)
	(statements
		(s-type-decl @1.1-1.24
			(header @1.1-1.8 (name "Pair")
				(args
					(ty-var @1.6-1.7 (raw "a"))))
			(ty-tag-union @1.12-1.24
				(tags
					(ty-apply @1.13-1.23
						(ty @1.13-1.17 (name "Pair"))
						(ty-var @1.18-1.19 (raw "a"))
						(ty-var @1.21-1.22 (raw "a"))))))
		(s-type-anno @3.1-3.20 (name "pairU64")
			(ty-apply @3.11-3.20
				(ty @3.11-3.15 (name "Pair"))
				(ty @3.16-3.19 (name "U64"))))
		(s-decl @4.1-4.26
			(p-ident @4.1-4.8 (raw "pairU64"))
			(e-apply @4.11-4.26
				(e-tag @4.11-4.20 (raw "Pair.Pair"))
				(e-int @4.21-4.22 (raw "1"))
				(e-int @4.24-4.25 (raw "2"))))
		(s-type-anno @6.1-6.20 (name "pairStr")
			(ty-apply @6.11-6.20
				(ty @6.11-6.15 (name "Pair"))
				(ty @6.16-6.19 (name "Str"))))
		(s-decl @7.1-7.38
			(p-ident @7.1-7.8 (raw "pairStr"))
			(e-apply @7.11-7.38
				(e-tag @7.11-7.20 (raw "Pair.Pair"))
				(e-string @7.21-7.28
					(e-string-part @7.22-7.27 (raw "hello")))
				(e-string @7.30-7.37
					(e-string-part @7.31-7.36 (raw "world")))))
		(s-type-anno @9.1-9.25 (name "mkPair")
			(ty-fn @9.10-9.25
				(ty-var @9.10-9.11 (raw "a"))
				(ty-var @9.13-9.14 (raw "a"))
				(ty-apply @9.18-9.25
					(ty @9.18-9.22 (name "Pair"))
					(ty-var @9.23-9.24 (raw "a")))))
		(s-decl @10.1-10.32
			(p-ident @10.1-10.7 (raw "mkPair"))
			(e-lambda @10.10-10.32
				(args
					(p-ident @10.11-10.12 (raw "x"))
					(p-ident @10.14-10.15 (raw "y")))
				(e-apply @10.17-10.32
					(e-tag @10.17-10.26 (raw "Pair.Pair"))
					(e-ident @10.27-10.28 (raw "x"))
					(e-ident @10.30-10.31 (raw "y")))))
		(s-type-anno @12.1-12.31 (name "succeedPairSameType")
			(ty-apply @12.23-12.31
				(ty @12.23-12.27 (name "Pair"))
				(ty @12.28-12.30 (name "U8"))))
		(s-decl @13.1-13.35
			(p-ident @13.1-13.20 (raw "succeedPairSameType"))
			(e-apply @13.23-13.35
				(e-ident @13.23-13.29 (raw "mkPair"))
				(e-int @13.30-13.31 (raw "1"))
				(e-int @13.33-13.34 (raw "2"))))
		(s-type-anno @15.1-15.29 (name "failPairDiffTypes")
			(ty-apply @15.21-15.29
				(ty @15.21-15.25 (name "Pair"))
				(ty @15.26-15.28 (name "U8"))))
		(s-decl @16.1-16.35
			(p-ident @16.1-16.18 (raw "failPairDiffTypes"))
			(e-apply @16.21-16.35
				(e-ident @16.21-16.27 (raw "mkPair"))
				(e-string @16.28-16.31
					(e-string-part @16.29-16.30 (raw "1")))
				(e-int @16.33-16.34 (raw "2"))))
		(s-type-anno @18.1-18.31 (name "failPairDiffTypes2")
			(ty-apply @18.22-18.31
				(ty @18.22-18.26 (name "Pair"))
				(ty @18.27-18.30 (name "U64"))))
		(s-decl @19.1-19.41
			(p-ident @19.1-19.19 (raw "failPairDiffTypes2"))
			(e-apply @19.22-19.41
				(e-tag @19.22-19.31 (raw "Pair.Pair"))
				(e-int @19.32-19.33 (raw "1"))
				(e-string @19.35-19.40
					(e-string-part @19.36-19.39 (raw "str")))))
		(s-type-anno @21.1-21.32 (name "mkPairInvalid")
			(ty-fn @21.17-21.32
				(ty-var @21.17-21.18 (raw "a"))
				(ty-var @21.20-21.21 (raw "b"))
				(ty-apply @21.25-21.32
					(ty @21.25-21.29 (name "Pair"))
					(ty-var @21.30-21.31 (raw "a")))))
		(s-decl @22.1-22.39
			(p-ident @22.1-22.14 (raw "mkPairInvalid"))
			(e-lambda @22.17-22.39
				(args
					(p-ident @22.18-22.19 (raw "x"))
					(p-ident @22.21-22.22 (raw "y")))
				(e-apply @22.24-22.39
					(e-tag @22.24-22.33 (raw "Pair.Pair"))
					(e-ident @22.34-22.35 (raw "x"))
					(e-ident @22.37-22.38 (raw "y")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.8 (ident "pairU64"))
		(e-nominal @4.11-4.26 (nominal "Pair")
			(e-tag @4.11-4.26 (name "Pair")
				(args
					(e-num @4.21-4.22 (value "1"))
					(e-num @4.24-4.25 (value "2")))))
		(annotation @4.1-4.8
			(declared-type
				(ty-apply @3.11-3.20 (name "Pair") (local)
					(ty-lookup @3.11-3.20 (name "U64") (builtin))))))
	(d-let
		(p-assign @7.1-7.8 (ident "pairStr"))
		(e-nominal @7.11-7.38 (nominal "Pair")
			(e-tag @7.11-7.38 (name "Pair")
				(args
					(e-string @7.21-7.28
						(e-literal @7.22-7.27 (string "hello")))
					(e-string @7.30-7.37
						(e-literal @7.31-7.36 (string "world"))))))
		(annotation @7.1-7.8
			(declared-type
				(ty-apply @6.11-6.20 (name "Pair") (local)
					(ty-lookup @6.11-6.20 (name "Str") (builtin))))))
	(d-let
		(p-assign @10.1-10.7 (ident "mkPair"))
		(e-lambda @10.10-10.32
			(args
				(p-assign @10.11-10.12 (ident "x"))
				(p-assign @10.14-10.15 (ident "y")))
			(e-nominal @10.17-10.32 (nominal "Pair")
				(e-tag @10.17-10.32 (name "Pair")
					(args
						(e-lookup-local @10.27-10.28
							(p-assign @10.11-10.12 (ident "x")))
						(e-lookup-local @10.30-10.31
							(p-assign @10.14-10.15 (ident "y")))))))
		(annotation @10.1-10.7
			(declared-type
				(ty-fn @9.10-9.25 (effectful false)
					(ty-rigid-var @9.10-9.11 (name "a"))
					(ty-rigid-var-lookup (ty-rigid-var @9.10-9.11 (name "a")))
					(ty-apply @9.18-9.25 (name "Pair") (local)
						(ty-rigid-var-lookup (ty-rigid-var @9.10-9.11 (name "a"))))))))
	(d-let
		(p-assign @13.1-13.20 (ident "succeedPairSameType"))
		(e-call @13.23-13.35
			(e-lookup-local @13.23-13.29
				(p-assign @10.1-10.7 (ident "mkPair")))
			(e-num @13.30-13.31 (value "1"))
			(e-num @13.33-13.34 (value "2")))
		(annotation @13.1-13.20
			(declared-type
				(ty-apply @12.23-12.31 (name "Pair") (local)
					(ty-lookup @12.23-12.31 (name "U8") (builtin))))))
	(d-let
		(p-assign @16.1-16.18 (ident "failPairDiffTypes"))
		(e-call @16.21-16.35
			(e-lookup-local @16.21-16.27
				(p-assign @10.1-10.7 (ident "mkPair")))
			(e-string @16.28-16.31
				(e-literal @16.29-16.30 (string "1")))
			(e-num @16.33-16.34 (value "2")))
		(annotation @16.1-16.18
			(declared-type
				(ty-apply @15.21-15.29 (name "Pair") (local)
					(ty-lookup @15.21-15.29 (name "U8") (builtin))))))
	(d-let
		(p-assign @19.1-19.19 (ident "failPairDiffTypes2"))
		(e-nominal @19.22-19.41 (nominal "Pair")
			(e-tag @19.22-19.41 (name "Pair")
				(args
					(e-num @19.32-19.33 (value "1"))
					(e-string @19.35-19.40
						(e-literal @19.36-19.39 (string "str"))))))
		(annotation @19.1-19.19
			(declared-type
				(ty-apply @18.22-18.31 (name "Pair") (local)
					(ty-lookup @18.22-18.31 (name "U64") (builtin))))))
	(d-let
		(p-assign @22.1-22.14 (ident "mkPairInvalid"))
		(e-lambda @22.17-22.39
			(args
				(p-assign @22.18-22.19 (ident "x"))
				(p-assign @22.21-22.22 (ident "y")))
			(e-nominal @22.24-22.39 (nominal "Pair")
				(e-tag @22.24-22.39 (name "Pair")
					(args
						(e-lookup-local @22.34-22.35
							(p-assign @22.18-22.19 (ident "x")))
						(e-lookup-local @22.37-22.38
							(p-assign @22.21-22.22 (ident "y")))))))
		(annotation @22.1-22.14
			(declared-type
				(ty-fn @21.17-21.32 (effectful false)
					(ty-rigid-var @21.17-21.18 (name "a"))
					(ty-rigid-var @21.20-21.21 (name "b"))
					(ty-apply @21.25-21.32 (name "Pair") (local)
						(ty-rigid-var-lookup (ty-rigid-var @21.17-21.18 (name "a"))))))))
	(s-nominal-decl @1.1-1.24
		(ty-header @1.1-1.8 (name "Pair")
			(ty-args
				(ty-rigid-var @1.6-1.7 (name "a"))))
		(ty-tag-union @1.12-1.24
			(ty-tag-name @1.13-1.23 (name "Pair")
				(ty-rigid-var-lookup (ty-rigid-var @1.6-1.7 (name "a")))
				(ty-rigid-var-lookup (ty-rigid-var @1.6-1.7 (name "a")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.8 (type "Pair(Num(Int(Unsigned64)))"))
		(patt @7.1-7.8 (type "Pair(Str)"))
		(patt @10.1-10.7 (type "a, a -> Pair(a)"))
		(patt @13.1-13.20 (type "Pair(Num(Int(Unsigned8)))"))
		(patt @16.1-16.18 (type "Error"))
		(patt @19.1-19.19 (type "Error"))
		(patt @22.1-22.14 (type "a, b -> Error")))
	(type_decls
		(nominal @1.1-1.24 (type "Pair(a)")
			(ty-header @1.1-1.8 (name "Pair")
				(ty-args
					(ty-rigid-var @1.6-1.7 (name "a"))))))
	(expressions
		(expr @4.11-4.26 (type "Pair(Num(Int(Unsigned64)))"))
		(expr @7.11-7.38 (type "Pair(Str)"))
		(expr @10.10-10.32 (type "a, a -> Pair(a)"))
		(expr @13.23-13.35 (type "Pair(Num(Int(Unsigned8)))"))
		(expr @16.21-16.35 (type "Error"))
		(expr @19.22-19.41 (type "Error"))
		(expr @22.17-22.39 (type "a, b -> Error"))))
~~~
