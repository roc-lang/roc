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

mk_pair : a, a -> Pair(a)
mk_pair = |x, y| Pair.Pair(x, y)

succeed_pair_same_type : Pair(U8)
succeed_pair_same_type = mk_pair(1, 2)

fail_pair_diff_types : Pair(U8)
fail_pair_diff_types = mk_pair("1", 2)

fail_pair_diff_types2 : Pair(U64)
fail_pair_diff_types2 = Pair.Pair(1, "str")

mk_pair_invalid : a, b -> Pair(a)
mk_pair_invalid = |x, y| Pair.Pair(x, y)

mk_pair_inferred = |x, y| Pair.Pair(x, y)

fail_with_implicit = mk_pair_inferred("str", 2)
~~~
# EXPECTED
TYPE MISMATCH - annotations.md:18:32:18:32
INVALID NOMINAL TAG - annotations.md:21:25:21:44
INVALID NOMINAL TAG - annotations.md:24:26:24:41
TYPE MISMATCH - annotations.md:28:39:28:39
# PROBLEMS
**TYPE MISMATCH**
The first and second arguments to `mk_pair` must have compatible types, but they are incompatible in this call:
**annotations.md:18:32:**
```roc
fail_pair_diff_types = mk_pair("1", 2)
```
                               ^^^  ^

The first argument is of type:
    _Str_

But the second argument is of type:
    _Num(_size)_

`mk_pair` needs these arguments to have compatible types.

**INVALID NOMINAL TAG**
I'm having trouble with this nominal tag:
**annotations.md:21:25:21:44:**
```roc
fail_pair_diff_types2 = Pair.Pair(1, "str")
```
                        ^^^^^^^^^^^^^^^^^^^

The tag is:
    _Pair(Num(_size), Str)_

But it should be:
    _Pair(Num(_size2), Num(_size3))_

**INVALID NOMINAL TAG**
I'm having trouble with this nominal tag:
**annotations.md:24:26:24:41:**
```roc
mk_pair_invalid = |x, y| Pair.Pair(x, y)
```
                         ^^^^^^^^^^^^^^^

The tag is:
    _Pair(a, b)_

But it should be:
    _Pair(a, a)_

**TYPE MISMATCH**
The first and second arguments to `mk_pair_inferred` must have compatible types, but they are incompatible in this call:
**annotations.md:28:39:**
```roc
fail_with_implicit = mk_pair_inferred("str", 2)
```
                                      ^^^^^  ^

The first argument is of type:
    _Str_

But the second argument is of type:
    _Num(_size)_

`mk_pair_inferred` needs these arguments to have compatible types.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(3:1-3:5),NoSpaceOpenRound(3:5-3:6),LowerIdent(3:6-3:7),CloseRound(3:7-3:8),OpColonEqual(3:9-3:11),OpenSquare(3:12-3:13),UpperIdent(3:13-3:17),NoSpaceOpenRound(3:17-3:18),LowerIdent(3:18-3:19),Comma(3:19-3:20),LowerIdent(3:21-3:22),CloseRound(3:22-3:23),CloseSquare(3:23-3:24),
LowerIdent(5:1-5:8),OpColon(5:9-5:10),UpperIdent(5:11-5:15),NoSpaceOpenRound(5:15-5:16),UpperIdent(5:16-5:19),CloseRound(5:19-5:20),
LowerIdent(6:1-6:8),OpAssign(6:9-6:10),UpperIdent(6:11-6:15),NoSpaceDotUpperIdent(6:15-6:20),NoSpaceOpenRound(6:20-6:21),Int(6:21-6:22),Comma(6:22-6:23),Int(6:24-6:25),CloseRound(6:25-6:26),
LowerIdent(8:1-8:8),OpColon(8:9-8:10),UpperIdent(8:11-8:15),NoSpaceOpenRound(8:15-8:16),UpperIdent(8:16-8:19),CloseRound(8:19-8:20),
LowerIdent(9:1-9:8),OpAssign(9:9-9:10),UpperIdent(9:11-9:15),NoSpaceDotUpperIdent(9:15-9:20),NoSpaceOpenRound(9:20-9:21),StringStart(9:21-9:22),StringPart(9:22-9:27),StringEnd(9:27-9:28),Comma(9:28-9:29),StringStart(9:30-9:31),StringPart(9:31-9:36),StringEnd(9:36-9:37),CloseRound(9:37-9:38),
LowerIdent(11:1-11:8),OpColon(11:9-11:10),LowerIdent(11:11-11:12),Comma(11:12-11:13),LowerIdent(11:14-11:15),OpArrow(11:16-11:18),UpperIdent(11:19-11:23),NoSpaceOpenRound(11:23-11:24),LowerIdent(11:24-11:25),CloseRound(11:25-11:26),
LowerIdent(12:1-12:8),OpAssign(12:9-12:10),OpBar(12:11-12:12),LowerIdent(12:12-12:13),Comma(12:13-12:14),LowerIdent(12:15-12:16),OpBar(12:16-12:17),UpperIdent(12:18-12:22),NoSpaceDotUpperIdent(12:22-12:27),NoSpaceOpenRound(12:27-12:28),LowerIdent(12:28-12:29),Comma(12:29-12:30),LowerIdent(12:31-12:32),CloseRound(12:32-12:33),
LowerIdent(14:1-14:23),OpColon(14:24-14:25),UpperIdent(14:26-14:30),NoSpaceOpenRound(14:30-14:31),UpperIdent(14:31-14:33),CloseRound(14:33-14:34),
LowerIdent(15:1-15:23),OpAssign(15:24-15:25),LowerIdent(15:26-15:33),NoSpaceOpenRound(15:33-15:34),Int(15:34-15:35),Comma(15:35-15:36),Int(15:37-15:38),CloseRound(15:38-15:39),
LowerIdent(17:1-17:21),OpColon(17:22-17:23),UpperIdent(17:24-17:28),NoSpaceOpenRound(17:28-17:29),UpperIdent(17:29-17:31),CloseRound(17:31-17:32),
LowerIdent(18:1-18:21),OpAssign(18:22-18:23),LowerIdent(18:24-18:31),NoSpaceOpenRound(18:31-18:32),StringStart(18:32-18:33),StringPart(18:33-18:34),StringEnd(18:34-18:35),Comma(18:35-18:36),Int(18:37-18:38),CloseRound(18:38-18:39),
LowerIdent(20:1-20:22),OpColon(20:23-20:24),UpperIdent(20:25-20:29),NoSpaceOpenRound(20:29-20:30),UpperIdent(20:30-20:33),CloseRound(20:33-20:34),
LowerIdent(21:1-21:22),OpAssign(21:23-21:24),UpperIdent(21:25-21:29),NoSpaceDotUpperIdent(21:29-21:34),NoSpaceOpenRound(21:34-21:35),Int(21:35-21:36),Comma(21:36-21:37),StringStart(21:38-21:39),StringPart(21:39-21:42),StringEnd(21:42-21:43),CloseRound(21:43-21:44),
LowerIdent(23:1-23:16),OpColon(23:17-23:18),LowerIdent(23:19-23:20),Comma(23:20-23:21),LowerIdent(23:22-23:23),OpArrow(23:24-23:26),UpperIdent(23:27-23:31),NoSpaceOpenRound(23:31-23:32),LowerIdent(23:32-23:33),CloseRound(23:33-23:34),
LowerIdent(24:1-24:16),OpAssign(24:17-24:18),OpBar(24:19-24:20),LowerIdent(24:20-24:21),Comma(24:21-24:22),LowerIdent(24:23-24:24),OpBar(24:24-24:25),UpperIdent(24:26-24:30),NoSpaceDotUpperIdent(24:30-24:35),NoSpaceOpenRound(24:35-24:36),LowerIdent(24:36-24:37),Comma(24:37-24:38),LowerIdent(24:39-24:40),CloseRound(24:40-24:41),
LowerIdent(26:1-26:17),OpAssign(26:18-26:19),OpBar(26:20-26:21),LowerIdent(26:21-26:22),Comma(26:22-26:23),LowerIdent(26:24-26:25),OpBar(26:25-26:26),UpperIdent(26:27-26:31),NoSpaceDotUpperIdent(26:31-26:36),NoSpaceOpenRound(26:36-26:37),LowerIdent(26:37-26:38),Comma(26:38-26:39),LowerIdent(26:40-26:41),CloseRound(26:41-26:42),
LowerIdent(28:1-28:19),OpAssign(28:20-28:21),LowerIdent(28:22-28:38),NoSpaceOpenRound(28:38-28:39),StringStart(28:39-28:40),StringPart(28:40-28:43),StringEnd(28:43-28:44),Comma(28:44-28:45),Int(28:46-28:47),CloseRound(28:47-28:48),EndOfFile(28:48-28:48),
~~~
# PARSE
~~~clojure
(file @1.1-28.48
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
		(s-type-anno @11.1-11.26 (name "mk_pair")
			(ty-fn @11.11-11.26
				(ty-var @11.11-11.12 (raw "a"))
				(ty-var @11.14-11.15 (raw "a"))
				(ty-apply @11.19-11.26
					(ty @11.19-11.23 (name "Pair"))
					(ty-var @11.24-11.25 (raw "a")))))
		(s-decl @12.1-12.33
			(p-ident @12.1-12.8 (raw "mk_pair"))
			(e-lambda @12.11-12.33
				(args
					(p-ident @12.12-12.13 (raw "x"))
					(p-ident @12.15-12.16 (raw "y")))
				(e-apply @12.18-12.33
					(e-tag @12.18-12.27 (raw "Pair.Pair"))
					(e-ident @12.28-12.29 (raw "x"))
					(e-ident @12.31-12.32 (raw "y")))))
		(s-type-anno @14.1-14.34 (name "succeed_pair_same_type")
			(ty-apply @14.26-14.34
				(ty @14.26-14.30 (name "Pair"))
				(ty @14.31-14.33 (name "U8"))))
		(s-decl @15.1-15.39
			(p-ident @15.1-15.23 (raw "succeed_pair_same_type"))
			(e-apply @15.26-15.39
				(e-ident @15.26-15.33 (raw "mk_pair"))
				(e-int @15.34-15.35 (raw "1"))
				(e-int @15.37-15.38 (raw "2"))))
		(s-type-anno @17.1-17.32 (name "fail_pair_diff_types")
			(ty-apply @17.24-17.32
				(ty @17.24-17.28 (name "Pair"))
				(ty @17.29-17.31 (name "U8"))))
		(s-decl @18.1-18.39
			(p-ident @18.1-18.21 (raw "fail_pair_diff_types"))
			(e-apply @18.24-18.39
				(e-ident @18.24-18.31 (raw "mk_pair"))
				(e-string @18.32-18.35
					(e-string-part @18.33-18.34 (raw "1")))
				(e-int @18.37-18.38 (raw "2"))))
		(s-type-anno @20.1-20.34 (name "fail_pair_diff_types2")
			(ty-apply @20.25-20.34
				(ty @20.25-20.29 (name "Pair"))
				(ty @20.30-20.33 (name "U64"))))
		(s-decl @21.1-21.44
			(p-ident @21.1-21.22 (raw "fail_pair_diff_types2"))
			(e-apply @21.25-21.44
				(e-tag @21.25-21.34 (raw "Pair.Pair"))
				(e-int @21.35-21.36 (raw "1"))
				(e-string @21.38-21.43
					(e-string-part @21.39-21.42 (raw "str")))))
		(s-type-anno @23.1-23.34 (name "mk_pair_invalid")
			(ty-fn @23.19-23.34
				(ty-var @23.19-23.20 (raw "a"))
				(ty-var @23.22-23.23 (raw "b"))
				(ty-apply @23.27-23.34
					(ty @23.27-23.31 (name "Pair"))
					(ty-var @23.32-23.33 (raw "a")))))
		(s-decl @24.1-24.41
			(p-ident @24.1-24.16 (raw "mk_pair_invalid"))
			(e-lambda @24.19-24.41
				(args
					(p-ident @24.20-24.21 (raw "x"))
					(p-ident @24.23-24.24 (raw "y")))
				(e-apply @24.26-24.41
					(e-tag @24.26-24.35 (raw "Pair.Pair"))
					(e-ident @24.36-24.37 (raw "x"))
					(e-ident @24.39-24.40 (raw "y")))))
		(s-decl @26.1-26.42
			(p-ident @26.1-26.17 (raw "mk_pair_inferred"))
			(e-lambda @26.20-26.42
				(args
					(p-ident @26.21-26.22 (raw "x"))
					(p-ident @26.24-26.25 (raw "y")))
				(e-apply @26.27-26.42
					(e-tag @26.27-26.36 (raw "Pair.Pair"))
					(e-ident @26.37-26.38 (raw "x"))
					(e-ident @26.40-26.41 (raw "y")))))
		(s-decl @28.1-28.48
			(p-ident @28.1-28.19 (raw "fail_with_implicit"))
			(e-apply @28.22-28.48
				(e-ident @28.22-28.38 (raw "mk_pair_inferred"))
				(e-string @28.39-28.44
					(e-string-part @28.40-28.43 (raw "str")))
				(e-int @28.46-28.47 (raw "2"))))))
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
					(e-int @6.21-6.22 (value "1"))
					(e-int @6.24-6.25 (value "2")))))
		(annotation @6.1-6.8
			(declared-type
				(ty-apply @5.11-5.20 (symbol "Pair")
					(ty @5.16-5.19 (name "U64"))))))
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
				(ty-apply @8.11-8.20 (symbol "Pair")
					(ty @8.16-8.19 (name "Str"))))))
	(d-let
		(p-assign @12.1-12.8 (ident "mk_pair"))
		(e-lambda @12.11-12.33
			(args
				(p-assign @12.12-12.13 (ident "x"))
				(p-assign @12.15-12.16 (ident "y")))
			(e-nominal @12.18-12.33 (nominal "Pair")
				(e-tag @12.18-12.33 (name "Pair")
					(args
						(e-lookup-local @12.28-12.29
							(p-assign @12.12-12.13 (ident "x")))
						(e-lookup-local @12.31-12.32
							(p-assign @12.15-12.16 (ident "y")))))))
		(annotation @12.1-12.8
			(declared-type
				(ty-fn @11.11-11.26 (effectful false)
					(ty-var @11.11-11.12 (name "a"))
					(ty-var @11.14-11.15 (name "a"))
					(ty-apply @11.19-11.26 (symbol "Pair")
						(ty-var @11.24-11.25 (name "a")))))))
	(d-let
		(p-assign @15.1-15.23 (ident "succeed_pair_same_type"))
		(e-call @15.26-15.39
			(e-lookup-local @15.26-15.33
				(p-assign @12.1-12.8 (ident "mk_pair")))
			(e-int @15.34-15.35 (value "1"))
			(e-int @15.37-15.38 (value "2")))
		(annotation @15.1-15.23
			(declared-type
				(ty-apply @14.26-14.34 (symbol "Pair")
					(ty @14.31-14.33 (name "U8"))))))
	(d-let
		(p-assign @18.1-18.21 (ident "fail_pair_diff_types"))
		(e-call @18.24-18.39
			(e-lookup-local @18.24-18.31
				(p-assign @12.1-12.8 (ident "mk_pair")))
			(e-string @18.32-18.35
				(e-literal @18.33-18.34 (string "1")))
			(e-int @18.37-18.38 (value "2")))
		(annotation @18.1-18.21
			(declared-type
				(ty-apply @17.24-17.32 (symbol "Pair")
					(ty @17.29-17.31 (name "U8"))))))
	(d-let
		(p-assign @21.1-21.22 (ident "fail_pair_diff_types2"))
		(e-nominal @21.25-21.44 (nominal "Pair")
			(e-tag @21.25-21.44 (name "Pair")
				(args
					(e-int @21.35-21.36 (value "1"))
					(e-string @21.38-21.43
						(e-literal @21.39-21.42 (string "str"))))))
		(annotation @21.1-21.22
			(declared-type
				(ty-apply @20.25-20.34 (symbol "Pair")
					(ty @20.30-20.33 (name "U64"))))))
	(d-let
		(p-assign @24.1-24.16 (ident "mk_pair_invalid"))
		(e-lambda @24.19-24.41
			(args
				(p-assign @24.20-24.21 (ident "x"))
				(p-assign @24.23-24.24 (ident "y")))
			(e-nominal @24.26-24.41 (nominal "Pair")
				(e-tag @24.26-24.41 (name "Pair")
					(args
						(e-lookup-local @24.36-24.37
							(p-assign @24.20-24.21 (ident "x")))
						(e-lookup-local @24.39-24.40
							(p-assign @24.23-24.24 (ident "y")))))))
		(annotation @24.1-24.16
			(declared-type
				(ty-fn @23.19-23.34 (effectful false)
					(ty-var @23.19-23.20 (name "a"))
					(ty-var @23.22-23.23 (name "b"))
					(ty-apply @23.27-23.34 (symbol "Pair")
						(ty-var @23.32-23.33 (name "a")))))))
	(d-let
		(p-assign @26.1-26.17 (ident "mk_pair_inferred"))
		(e-lambda @26.20-26.42
			(args
				(p-assign @26.21-26.22 (ident "x"))
				(p-assign @26.24-26.25 (ident "y")))
			(e-nominal @26.27-26.42 (nominal "Pair")
				(e-tag @26.27-26.42 (name "Pair")
					(args
						(e-lookup-local @26.37-26.38
							(p-assign @26.21-26.22 (ident "x")))
						(e-lookup-local @26.40-26.41
							(p-assign @26.24-26.25 (ident "y"))))))))
	(d-let
		(p-assign @28.1-28.19 (ident "fail_with_implicit"))
		(e-call @28.22-28.48
			(e-lookup-local @28.22-28.38
				(p-assign @26.1-26.17 (ident "mk_pair_inferred")))
			(e-string @28.39-28.44
				(e-literal @28.40-28.43 (string "str")))
			(e-int @28.46-28.47 (value "2"))))
	(s-nominal-decl @3.1-3.24
		(ty-header @3.1-3.8 (name "Pair")
			(ty-args
				(ty-var @3.6-3.7 (name "a"))))
		(ty-tag-union @3.12-3.24
			(ty-apply @3.13-3.23 (symbol "Pair")
				(ty-var @3.18-3.19 (name "a"))
				(ty-var @3.21-3.22 (name "a"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.8 (type "Pair(U64)"))
		(patt @9.1-9.8 (type "Pair(Str)"))
		(patt @12.1-12.8 (type "a, a -> Pair(a)"))
		(patt @15.1-15.23 (type "Pair(U8)"))
		(patt @18.1-18.21 (type "Pair(U8)"))
		(patt @21.1-21.22 (type "Error"))
		(patt @24.1-24.16 (type "a, b -> Error"))
		(patt @26.1-26.17 (type "a, a -> Pair(a)"))
		(patt @28.1-28.19 (type "_c")))
	(type_decls
		(nominal @3.1-3.24 (type "Pair(a)")
			(ty-header @3.1-3.8 (name "Pair")
				(ty-args
					(ty-var @3.6-3.7 (name "a"))))))
	(expressions
		(expr @6.11-6.26 (type "Pair(U64)"))
		(expr @9.11-9.38 (type "Pair(Str)"))
		(expr @12.11-12.33 (type "a, a -> Pair(a)"))
		(expr @15.26-15.39 (type "Pair(U8)"))
		(expr @18.24-18.39 (type "Pair(U8)"))
		(expr @21.25-21.44 (type "Error"))
		(expr @24.19-24.41 (type "a, b -> Error"))
		(expr @26.20-26.42 (type "a, a -> Pair(a)"))
		(expr @28.22-28.48 (type "_c"))))
~~~
