# META
~~~ini
description=Nested instantiation with record field access causing type mismatch
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }
# TODO: if you do this whole thing as an expr block, with `composed` at
# the end instead of `answer =`, it triggers a parser bug!

make_record : a -> { value: a, tag: Str }
make_record = |x| { value: x, tag: "data" }

get_value : { value: a, tag: Str } -> a
get_value = |r| r.value

composed : List(a) -> Str
composed = |n| get_value(make_record(n))

answer = composed([42])
~~~
# EXPECTED
TYPE MISMATCH - test_nested_instantiation_crash.md:11:12:11:26
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**test_nested_instantiation_crash.md:11:12:11:26:**
```roc
composed : List(a) -> Str
```
           ^^^^^^^^^^^^^^

It is of type:
    _List(a) -> Str_

But you are trying to use it as:
    _List(a) -> List(a)_

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:53),StringEnd(1:53-1:54),CloseCurly(1:55-1:56),
LowerIdent(5:1-5:12),OpColon(5:13-5:14),LowerIdent(5:15-5:16),OpArrow(5:17-5:19),OpenCurly(5:20-5:21),LowerIdent(5:22-5:27),OpColon(5:27-5:28),LowerIdent(5:29-5:30),Comma(5:30-5:31),LowerIdent(5:32-5:35),OpColon(5:35-5:36),UpperIdent(5:37-5:40),CloseCurly(5:41-5:42),
LowerIdent(6:1-6:12),OpAssign(6:13-6:14),OpBar(6:15-6:16),LowerIdent(6:16-6:17),OpBar(6:17-6:18),OpenCurly(6:19-6:20),LowerIdent(6:21-6:26),OpColon(6:26-6:27),LowerIdent(6:28-6:29),Comma(6:29-6:30),LowerIdent(6:31-6:34),OpColon(6:34-6:35),StringStart(6:36-6:37),StringPart(6:37-6:41),StringEnd(6:41-6:42),CloseCurly(6:43-6:44),
LowerIdent(8:1-8:10),OpColon(8:11-8:12),OpenCurly(8:13-8:14),LowerIdent(8:15-8:20),OpColon(8:20-8:21),LowerIdent(8:22-8:23),Comma(8:23-8:24),LowerIdent(8:25-8:28),OpColon(8:28-8:29),UpperIdent(8:30-8:33),CloseCurly(8:34-8:35),OpArrow(8:36-8:38),LowerIdent(8:39-8:40),
LowerIdent(9:1-9:10),OpAssign(9:11-9:12),OpBar(9:13-9:14),LowerIdent(9:14-9:15),OpBar(9:15-9:16),LowerIdent(9:17-9:18),NoSpaceDotLowerIdent(9:18-9:24),
LowerIdent(11:1-11:9),OpColon(11:10-11:11),UpperIdent(11:12-11:16),NoSpaceOpenRound(11:16-11:17),LowerIdent(11:17-11:18),CloseRound(11:18-11:19),OpArrow(11:20-11:22),UpperIdent(11:23-11:26),
LowerIdent(12:1-12:9),OpAssign(12:10-12:11),OpBar(12:12-12:13),LowerIdent(12:13-12:14),OpBar(12:14-12:15),LowerIdent(12:16-12:25),NoSpaceOpenRound(12:25-12:26),LowerIdent(12:26-12:37),NoSpaceOpenRound(12:37-12:38),LowerIdent(12:38-12:39),CloseRound(12:39-12:40),CloseRound(12:40-12:41),
LowerIdent(14:1-14:7),OpAssign(14:8-14:9),LowerIdent(14:10-14:18),NoSpaceOpenRound(14:18-14:19),OpenSquare(14:19-14:20),Int(14:20-14:22),CloseSquare(14:22-14:23),CloseRound(14:23-14:24),EndOfFile(14:24-14:24),
~~~
# PARSE
~~~clojure
(file @1.1-14.24
	(app @1.1-1.56
		(provides @1.5-1.11
			(exposed-lower-ident @1.6-1.10
				(text "main")))
		(record-field @1.14-1.54 (name "pf")
			(e-string @1.27-1.54
				(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))
		(packages @1.12-1.56
			(record-field @1.14-1.54 (name "pf")
				(e-string @1.27-1.54
					(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno @5.1-5.42 (name "make_record")
			(ty-fn @5.15-5.42
				(ty-var @5.15-5.16 (raw "a"))
				(ty-record @5.20-5.42
					(anno-record-field @5.22-5.30 (name "value")
						(ty-var @5.29-5.30 (raw "a")))
					(anno-record-field @5.32-5.40 (name "tag")
						(ty @5.37-5.40 (name "Str"))))))
		(s-decl @6.1-6.44
			(p-ident @6.1-6.12 (raw "make_record"))
			(e-lambda @6.15-6.44
				(args
					(p-ident @6.16-6.17 (raw "x")))
				(e-record @6.19-6.44
					(field (field "value")
						(e-ident @6.28-6.29 (raw "x")))
					(field (field "tag")
						(e-string @6.36-6.42
							(e-string-part @6.37-6.41 (raw "data")))))))
		(s-type-anno @8.1-8.40 (name "get_value")
			(ty-fn @8.13-8.40
				(ty-record @8.13-8.35
					(anno-record-field @8.15-8.23 (name "value")
						(ty-var @8.22-8.23 (raw "a")))
					(anno-record-field @8.25-8.33 (name "tag")
						(ty @8.30-8.33 (name "Str"))))
				(ty-var @8.39-8.40 (raw "a"))))
		(s-decl @9.1-9.24
			(p-ident @9.1-9.10 (raw "get_value"))
			(e-lambda @9.13-9.24
				(args
					(p-ident @9.14-9.15 (raw "r")))
				(e-field-access @9.17-9.24
					(e-ident @9.17-9.18 (raw "r"))
					(e-ident @9.18-9.24 (raw "value")))))
		(s-type-anno @11.1-11.26 (name "composed")
			(ty-fn @11.12-11.26
				(ty-apply @11.12-11.19
					(ty @11.12-11.16 (name "List"))
					(ty-var @11.17-11.18 (raw "a")))
				(ty @11.23-11.26 (name "Str"))))
		(s-decl @12.1-12.41
			(p-ident @12.1-12.9 (raw "composed"))
			(e-lambda @12.12-12.41
				(args
					(p-ident @12.13-12.14 (raw "n")))
				(e-apply @12.16-12.41
					(e-ident @12.16-12.25 (raw "get_value"))
					(e-apply @12.26-12.40
						(e-ident @12.26-12.37 (raw "make_record"))
						(e-ident @12.38-12.39 (raw "n"))))))
		(s-decl @14.1-14.24
			(p-ident @14.1-14.7 (raw "answer"))
			(e-apply @14.10-14.24
				(e-ident @14.10-14.18 (raw "composed"))
				(e-list @14.19-14.23
					(e-int @14.20-14.22 (raw "42")))))))
~~~
# FORMATTED
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }
# TODO: if you do this whole thing as an expr block, with `composed` at
# the end instead of `answer =`, it triggers a parser bug!

make_record : a -> { value : a, tag : Str }
make_record = |x| { value: x, tag: "data" }

get_value : { value : a, tag : Str } -> a
get_value = |r| r.value

composed : List(a) -> Str
composed = |n| get_value(make_record(n))

answer = composed([42])
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.12 (ident "make_record"))
		(e-lambda @6.15-6.44
			(args
				(p-assign @6.16-6.17 (ident "x")))
			(e-record @6.19-6.44
				(fields
					(field (name "value")
						(e-lookup-local @6.28-6.29
							(p-assign @6.16-6.17 (ident "x"))))
					(field (name "tag")
						(e-string @6.36-6.42
							(e-literal @6.37-6.41 (string "data")))))))
		(annotation @6.1-6.12
			(declared-type
				(ty-fn @5.15-5.42 (effectful false)
					(ty-var @5.15-5.16 (name "a"))
					(ty-record @5.20-5.42
						(field (field "value")
							(ty-var @5.29-5.30 (name "a")))
						(field (field "tag")
							(ty @5.37-5.40 (name "Str"))))))))
	(d-let
		(p-assign @9.1-9.10 (ident "get_value"))
		(e-lambda @9.13-9.24
			(args
				(p-assign @9.14-9.15 (ident "r")))
			(e-dot-access @9.17-9.24 (field "value")
				(receiver
					(e-lookup-local @9.17-9.18
						(p-assign @9.14-9.15 (ident "r"))))))
		(annotation @9.1-9.10
			(declared-type
				(ty-fn @8.13-8.40 (effectful false)
					(ty-record @8.13-8.35
						(field (field "value")
							(ty-var @8.22-8.23 (name "a")))
						(field (field "tag")
							(ty @8.30-8.33 (name "Str"))))
					(ty-var @8.39-8.40 (name "a"))))))
	(d-let
		(p-assign @12.1-12.9 (ident "composed"))
		(e-closure @12.12-12.41
			(captures
				(capture @6.1-6.12 (ident "make_record"))
				(capture @9.1-9.10 (ident "get_value")))
			(e-lambda @12.12-12.41
				(args
					(p-assign @12.13-12.14 (ident "n")))
				(e-call @12.16-12.41
					(e-lookup-local @12.16-12.25
						(p-assign @9.1-9.10 (ident "get_value")))
					(e-call @12.26-12.40
						(e-lookup-local @12.26-12.37
							(p-assign @6.1-6.12 (ident "make_record")))
						(e-lookup-local @12.38-12.39
							(p-assign @12.13-12.14 (ident "n")))))))
		(annotation @12.1-12.9
			(declared-type
				(ty-fn @11.12-11.26 (effectful false)
					(ty-apply @11.12-11.19 (symbol "List")
						(ty-var @11.17-11.18 (name "a")))
					(ty @11.23-11.26 (name "Str"))))))
	(d-let
		(p-assign @14.1-14.7 (ident "answer"))
		(e-call @14.10-14.24
			(e-lookup-local @14.10-14.18
				(p-assign @12.1-12.9 (ident "composed")))
			(e-list @14.19-14.23
				(elems
					(e-int @14.20-14.22 (value "42")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.12 (type "a -> { tag: Str, value: a }"))
		(patt @9.1-9.10 (type "{ tag: Str, value: a } -> a"))
		(patt @12.1-12.9 (type "Error"))
		(patt @14.1-14.7 (type "_b")))
	(expressions
		(expr @6.15-6.44 (type "a -> { tag: Str, value: a }"))
		(expr @9.13-9.24 (type "{ tag: Str, value: a } -> a"))
		(expr @12.12-12.41 (type "Error"))
		(expr @14.10-14.24 (type "_b"))))
~~~
