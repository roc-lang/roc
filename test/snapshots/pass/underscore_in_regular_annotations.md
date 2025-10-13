# META
~~~ini
description=Underscores are allowed in regular type annotations (not in type declarations)
type=snippet
~~~
# SOURCE
~~~roc
main : _ -> _
main = |x| x

identity : a -> a
identity = |x| x

# Function with underscore in annotation
process : List(_) -> Str
process = |list| "processed"

# Record with underscore
get_data : { field: _, other: U32 } -> U32
get_data = |record| record.other

# Pattern matching with underscore type annotation
handle_result : Result(_, Str) -> Str
handle_result = |result|
    match result {
        Ok(_) => "success",
        Err(msg) => msg,
    }

# Underscore in function arguments
map : (a -> b), List(a) -> List(b)
map = |_, _| []

# Named underscore type variables
transform : _a -> _b -> _b
transform = |_, b| b
~~~
# EXPECTED
PARSE ERROR - underscore_in_regular_annotations.md:28:22:28:24
PARSE ERROR - underscore_in_regular_annotations.md:28:25:28:27
UNUSED VARIABLE - underscore_in_regular_annotations.md:9:12:9:16
# PROBLEMS
**PARSE ERROR**
Function types with multiple arrows need parentheses.

Instead of writing **a -> b -> c**, use parentheses to clarify which you mean:
        a -> (b -> c) for a **curried** function (a function that **returns** another function)
        (a -> b) -> c for a **higher-order** function (a function that **takes** another function)

**underscore_in_regular_annotations.md:28:22:28:24:**
```roc
transform : _a -> _b -> _b
```
                     ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_in_regular_annotations.md:28:25:28:27:**
```roc
transform : _a -> _b -> _b
```
                        ^^


**UNUSED VARIABLE**
Variable `list` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_list` to suppress this warning.
The unused variable is declared here:
**underscore_in_regular_annotations.md:9:12:9:16:**
```roc
process = |list| "processed"
```
           ^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpColon(1:6-1:7),Underscore(1:8-1:9),OpArrow(1:10-1:12),Underscore(1:13-1:14),
LowerIdent(2:1-2:5),OpAssign(2:6-2:7),OpBar(2:8-2:9),LowerIdent(2:9-2:10),OpBar(2:10-2:11),LowerIdent(2:12-2:13),
LowerIdent(4:1-4:9),OpColon(4:10-4:11),LowerIdent(4:12-4:13),OpArrow(4:14-4:16),LowerIdent(4:17-4:18),
LowerIdent(5:1-5:9),OpAssign(5:10-5:11),OpBar(5:12-5:13),LowerIdent(5:13-5:14),OpBar(5:14-5:15),LowerIdent(5:16-5:17),
LowerIdent(8:1-8:8),OpColon(8:9-8:10),UpperIdent(8:11-8:15),NoSpaceOpenRound(8:15-8:16),Underscore(8:16-8:17),CloseRound(8:17-8:18),OpArrow(8:19-8:21),UpperIdent(8:22-8:25),
LowerIdent(9:1-9:8),OpAssign(9:9-9:10),OpBar(9:11-9:12),LowerIdent(9:12-9:16),OpBar(9:16-9:17),StringStart(9:18-9:19),StringPart(9:19-9:28),StringEnd(9:28-9:29),
LowerIdent(12:1-12:9),OpColon(12:10-12:11),OpenCurly(12:12-12:13),LowerIdent(12:14-12:19),OpColon(12:19-12:20),Underscore(12:21-12:22),Comma(12:22-12:23),LowerIdent(12:24-12:29),OpColon(12:29-12:30),UpperIdent(12:31-12:34),CloseCurly(12:35-12:36),OpArrow(12:37-12:39),UpperIdent(12:40-12:43),
LowerIdent(13:1-13:9),OpAssign(13:10-13:11),OpBar(13:12-13:13),LowerIdent(13:13-13:19),OpBar(13:19-13:20),LowerIdent(13:21-13:27),NoSpaceDotLowerIdent(13:27-13:33),
LowerIdent(16:1-16:14),OpColon(16:15-16:16),UpperIdent(16:17-16:23),NoSpaceOpenRound(16:23-16:24),Underscore(16:24-16:25),Comma(16:25-16:26),UpperIdent(16:27-16:30),CloseRound(16:30-16:31),OpArrow(16:32-16:34),UpperIdent(16:35-16:38),
LowerIdent(17:1-17:14),OpAssign(17:15-17:16),OpBar(17:17-17:18),LowerIdent(17:18-17:24),OpBar(17:24-17:25),
KwMatch(18:5-18:10),LowerIdent(18:11-18:17),OpenCurly(18:18-18:19),
UpperIdent(19:9-19:11),NoSpaceOpenRound(19:11-19:12),Underscore(19:12-19:13),CloseRound(19:13-19:14),OpFatArrow(19:15-19:17),StringStart(19:18-19:19),StringPart(19:19-19:26),StringEnd(19:26-19:27),Comma(19:27-19:28),
UpperIdent(20:9-20:12),NoSpaceOpenRound(20:12-20:13),LowerIdent(20:13-20:16),CloseRound(20:16-20:17),OpFatArrow(20:18-20:20),LowerIdent(20:21-20:24),Comma(20:24-20:25),
CloseCurly(21:5-21:6),
LowerIdent(24:1-24:4),OpColon(24:5-24:6),OpenRound(24:7-24:8),LowerIdent(24:8-24:9),OpArrow(24:10-24:12),LowerIdent(24:13-24:14),CloseRound(24:14-24:15),Comma(24:15-24:16),UpperIdent(24:17-24:21),NoSpaceOpenRound(24:21-24:22),LowerIdent(24:22-24:23),CloseRound(24:23-24:24),OpArrow(24:25-24:27),UpperIdent(24:28-24:32),NoSpaceOpenRound(24:32-24:33),LowerIdent(24:33-24:34),CloseRound(24:34-24:35),
LowerIdent(25:1-25:4),OpAssign(25:5-25:6),OpBar(25:7-25:8),Underscore(25:8-25:9),Comma(25:9-25:10),Underscore(25:11-25:12),OpBar(25:12-25:13),OpenSquare(25:14-25:15),CloseSquare(25:15-25:16),
LowerIdent(28:1-28:10),OpColon(28:11-28:12),NamedUnderscore(28:13-28:15),OpArrow(28:16-28:18),NamedUnderscore(28:19-28:21),OpArrow(28:22-28:24),NamedUnderscore(28:25-28:27),
LowerIdent(29:1-29:10),OpAssign(29:11-29:12),OpBar(29:13-29:14),Underscore(29:14-29:15),Comma(29:15-29:16),LowerIdent(29:17-29:18),OpBar(29:18-29:19),LowerIdent(29:20-29:21),
EndOfFile(30:1-30:1),
~~~
# PARSE
~~~clojure
(file @1.1-29.21
	(type-module @1.1-1.5)
	(statements
		(s-type-anno @1.1-1.14 (name "main")
			(ty-fn @1.8-1.14
				(_)
				(_)))
		(s-decl @2.1-2.13
			(p-ident @2.1-2.5 (raw "main"))
			(e-lambda @2.8-2.13
				(args
					(p-ident @2.9-2.10 (raw "x")))
				(e-ident @2.12-2.13 (raw "x"))))
		(s-type-anno @4.1-4.18 (name "identity")
			(ty-fn @4.12-4.18
				(ty-var @4.12-4.13 (raw "a"))
				(ty-var @4.17-4.18 (raw "a"))))
		(s-decl @5.1-5.17
			(p-ident @5.1-5.9 (raw "identity"))
			(e-lambda @5.12-5.17
				(args
					(p-ident @5.13-5.14 (raw "x")))
				(e-ident @5.16-5.17 (raw "x"))))
		(s-type-anno @8.1-8.25 (name "process")
			(ty-fn @8.11-8.25
				(ty-apply @8.11-8.18
					(ty @8.11-8.15 (name "List"))
					(_))
				(ty @8.22-8.25 (name "Str"))))
		(s-decl @9.1-9.29
			(p-ident @9.1-9.8 (raw "process"))
			(e-lambda @9.11-9.29
				(args
					(p-ident @9.12-9.16 (raw "list")))
				(e-string @9.18-9.29
					(e-string-part @9.19-9.28 (raw "processed")))))
		(s-type-anno @12.1-12.43 (name "get_data")
			(ty-fn @12.12-12.43
				(ty-record @12.12-12.36
					(anno-record-field @12.14-12.22 (name "field")
						(_))
					(anno-record-field @12.24-12.34 (name "other")
						(ty @12.31-12.34 (name "U32"))))
				(ty @12.40-12.43 (name "U32"))))
		(s-decl @13.1-13.33
			(p-ident @13.1-13.9 (raw "get_data"))
			(e-lambda @13.12-13.33
				(args
					(p-ident @13.13-13.19 (raw "record")))
				(e-field-access @13.21-13.33
					(e-ident @13.21-13.27 (raw "record"))
					(e-ident @13.27-13.33 (raw "other")))))
		(s-type-anno @16.1-16.38 (name "handle_result")
			(ty-fn @16.17-16.38
				(ty-apply @16.17-16.31
					(ty @16.17-16.23 (name "Result"))
					(_)
					(ty @16.27-16.30 (name "Str")))
				(ty @16.35-16.38 (name "Str"))))
		(s-decl @17.1-21.6
			(p-ident @17.1-17.14 (raw "handle_result"))
			(e-lambda @17.17-21.6
				(args
					(p-ident @17.18-17.24 (raw "result")))
				(e-match
					(e-ident @18.11-18.17 (raw "result"))
					(branches
						(branch @19.9-19.27
							(p-tag @19.9-19.14 (raw "Ok")
								(p-underscore))
							(e-string @19.18-19.27
								(e-string-part @19.19-19.26 (raw "success"))))
						(branch @20.9-20.24
							(p-tag @20.9-20.17 (raw "Err")
								(p-ident @20.13-20.16 (raw "msg")))
							(e-ident @20.21-20.24 (raw "msg")))))))
		(s-type-anno @24.1-24.35 (name "map")
			(ty-fn @24.7-24.35
				(ty-fn @24.8-24.14
					(ty-var @24.8-24.9 (raw "a"))
					(ty-var @24.13-24.14 (raw "b")))
				(ty-apply @24.17-24.24
					(ty @24.17-24.21 (name "List"))
					(ty-var @24.22-24.23 (raw "a")))
				(ty-apply @24.28-24.35
					(ty @24.28-24.32 (name "List"))
					(ty-var @24.33-24.34 (raw "b")))))
		(s-decl @25.1-25.16
			(p-ident @25.1-25.4 (raw "map"))
			(e-lambda @25.7-25.16
				(args
					(p-underscore)
					(p-underscore))
				(e-list @25.14-25.16)))
		(s-type-anno @28.1-28.21 (name "transform")
			(ty-fn @28.13-28.21
				(underscore-ty-var @28.13-28.15 (raw "_a"))
				(underscore-ty-var @28.19-28.21 (raw "_b"))))
		(s-malformed @28.22-28.24 (tag "multi_arrow_needs_parens"))
		(s-malformed @28.25-28.27 (tag "statement_unexpected_token"))
		(s-decl @29.1-29.21
			(p-ident @29.1-29.10 (raw "transform"))
			(e-lambda @29.13-29.21
				(args
					(p-underscore)
					(p-ident @29.17-29.18 (raw "b")))
				(e-ident @29.20-29.21 (raw "b"))))))
~~~
# FORMATTED
~~~roc
main : _ -> _
main = |x| x

identity : a -> a
identity = |x| x

# Function with underscore in annotation
process : List(_) -> Str
process = |list| "processed"

# Record with underscore
get_data : { field : _, other : U32 } -> U32
get_data = |record| record.other

# Pattern matching with underscore type annotation
handle_result : Result(_, Str) -> Str
handle_result = |result|
	match result {
		Ok(_) => "success"
		Err(msg) => msg
	}

# Underscore in function arguments
map : (a -> b), List(a) -> List(b)
map = |_, _| []

# Named underscore type variables
transform : _a -> _b

transform = |_, b| b
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.5 (ident "main"))
		(e-lambda @2.8-2.13
			(args
				(p-assign @2.9-2.10 (ident "x")))
			(e-lookup-local @2.12-2.13
				(p-assign @2.9-2.10 (ident "x"))))
		(annotation @2.1-2.5
			(declared-type
				(ty-fn @1.8-1.14 (effectful false)
					(ty-underscore @1.1-1.1)
					(ty-underscore @1.1-1.1)))))
	(d-let
		(p-assign @5.1-5.9 (ident "identity"))
		(e-lambda @5.12-5.17
			(args
				(p-assign @5.13-5.14 (ident "x")))
			(e-lookup-local @5.16-5.17
				(p-assign @5.13-5.14 (ident "x"))))
		(annotation @5.1-5.9
			(declared-type
				(ty-fn @4.12-4.18 (effectful false)
					(ty-rigid-var @4.12-4.13 (name "a"))
					(ty-rigid-var-lookup (ty-rigid-var @4.12-4.13 (name "a")))))))
	(d-let
		(p-assign @9.1-9.8 (ident "process"))
		(e-lambda @9.11-9.29
			(args
				(p-assign @9.12-9.16 (ident "list")))
			(e-string @9.18-9.29
				(e-literal @9.19-9.28 (string "processed"))))
		(annotation @9.1-9.8
			(declared-type
				(ty-fn @8.11-8.25 (effectful false)
					(ty-apply @8.11-8.18 (name "List") (builtin)
						(ty-underscore @8.16-8.16))
					(ty-lookup @8.22-8.25 (name "Str") (builtin))))))
	(d-let
		(p-assign @13.1-13.9 (ident "get_data"))
		(e-lambda @13.12-13.33
			(args
				(p-assign @13.13-13.19 (ident "record")))
			(e-dot-access @13.21-13.33 (field "other")
				(receiver
					(e-lookup-local @13.21-13.27
						(p-assign @13.13-13.19 (ident "record"))))))
		(annotation @13.1-13.9
			(declared-type
				(ty-fn @12.12-12.43 (effectful false)
					(ty-record @12.12-12.36
						(field (field "field")
							(ty-underscore @1.1-1.1))
						(field (field "other")
							(ty-lookup @12.31-12.34 (name "U32") (builtin))))
					(ty-lookup @12.40-12.43 (name "U32") (builtin))))))
	(d-let
		(p-assign @17.1-17.14 (ident "handle_result"))
		(e-closure @17.17-21.6
			(captures
				(capture @20.13-20.16 (ident "msg")))
			(e-lambda @17.17-21.6
				(args
					(p-assign @17.18-17.24 (ident "result")))
				(e-match @18.5-21.6
					(match @18.5-21.6
						(cond
							(e-lookup-local @18.11-18.17
								(p-assign @17.18-17.24 (ident "result"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @19.9-19.14)))
								(value
									(e-string @19.18-19.27
										(e-literal @19.19-19.26 (string "success")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @20.9-20.17)))
								(value
									(e-lookup-local @20.21-20.24
										(p-assign @20.13-20.16 (ident "msg"))))))))))
		(annotation @17.1-17.14
			(declared-type
				(ty-fn @16.17-16.38 (effectful false)
					(ty-apply @16.17-16.31 (name "Result") (external (module-idx "3") (target-node-idx "3"))
						(ty-underscore @16.24-16.24)
						(ty-lookup @16.27-16.30 (name "Str") (builtin)))
					(ty-lookup @16.35-16.38 (name "Str") (builtin))))))
	(d-let
		(p-assign @25.1-25.4 (ident "map"))
		(e-lambda @25.7-25.16
			(args
				(p-underscore @25.8-25.9)
				(p-underscore @25.11-25.12))
			(e-empty_list @25.14-25.16))
		(annotation @25.1-25.4
			(declared-type
				(ty-fn @24.7-24.35 (effectful false)
					(ty-parens @24.7-24.15
						(ty-fn @24.8-24.14 (effectful false)
							(ty-rigid-var @24.8-24.9 (name "a"))
							(ty-rigid-var @24.13-24.14 (name "b"))))
					(ty-apply @24.17-24.24 (name "List") (builtin)
						(ty-rigid-var-lookup (ty-rigid-var @24.8-24.9 (name "a"))))
					(ty-apply @24.28-24.35 (name "List") (builtin)
						(ty-rigid-var-lookup (ty-rigid-var @24.13-24.14 (name "b"))))))))
	(d-let
		(p-assign @29.1-29.10 (ident "transform"))
		(e-lambda @29.13-29.21
			(args
				(p-underscore @29.14-29.15)
				(p-assign @29.17-29.18 (ident "b")))
			(e-lookup-local @29.20-29.21
				(p-assign @29.17-29.18 (ident "b"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.5 (type "_arg -> _ret"))
		(patt @5.1-5.9 (type "a -> a"))
		(patt @9.1-9.8 (type "List(_elem) -> Str"))
		(patt @13.1-13.9 (type "{ field: _field2, other: Num(Int(Unsigned32)) } -> Num(Int(Unsigned32))"))
		(patt @17.1-17.14 (type "Error -> Str"))
		(patt @25.1-25.4 (type "a -> b, List(a) -> List(b)"))
		(patt @29.1-29.10 (type "_arg, c -> c")))
	(expressions
		(expr @2.8-2.13 (type "_arg -> _ret"))
		(expr @5.12-5.17 (type "a -> a"))
		(expr @9.11-9.29 (type "List(_elem) -> Str"))
		(expr @13.12-13.33 (type "{ field: _field2, other: Num(Int(Unsigned32)) } -> Num(Int(Unsigned32))"))
		(expr @17.17-21.6 (type "Error -> Str"))
		(expr @25.7-25.16 (type "a -> b, List(a) -> List(b)"))
		(expr @29.13-29.21 (type "_arg, c -> c"))))
~~~
