# META
~~~ini
description=Underscores are allowed in regular type annotations (not in type declarations)
type=file
~~~
# SOURCE
~~~roc
module []

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
PARSE ERROR - underscore_in_regular_annotations.md:30:22:30:24
PARSE ERROR - underscore_in_regular_annotations.md:30:25:30:27
UNUSED VARIABLE - underscore_in_regular_annotations.md:11:12:11:16
# PROBLEMS
**PARSE ERROR**
Function types with multiple arrows need parentheses.

Instead of writing **a -> b -> c**, use parentheses to clarify which you mean:
        a -> (b -> c) for a **curried** function (a function that **returns** another function)
        (a -> b) -> c for a **higher-order** function (a function that **takes** another function)

**underscore_in_regular_annotations.md:30:22:30:24:**
```roc
transform : _a -> _b -> _b
```
                     ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_in_regular_annotations.md:30:25:30:27:**
```roc
transform : _a -> _b -> _b
```
                        ^^


**UNUSED VARIABLE**
Variable `list` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_list` to suppress this warning.
The unused variable is declared here:
**underscore_in_regular_annotations.md:11:12:11:16:**
```roc
process = |list| "processed"
```
           ^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(3:1-3:5),OpColon(3:6-3:7),Underscore(3:8-3:9),OpArrow(3:10-3:12),Underscore(3:13-3:14),
LowerIdent(4:1-4:5),OpAssign(4:6-4:7),OpBar(4:8-4:9),LowerIdent(4:9-4:10),OpBar(4:10-4:11),LowerIdent(4:12-4:13),
LowerIdent(6:1-6:9),OpColon(6:10-6:11),LowerIdent(6:12-6:13),OpArrow(6:14-6:16),LowerIdent(6:17-6:18),
LowerIdent(7:1-7:9),OpAssign(7:10-7:11),OpBar(7:12-7:13),LowerIdent(7:13-7:14),OpBar(7:14-7:15),LowerIdent(7:16-7:17),
LowerIdent(10:1-10:8),OpColon(10:9-10:10),UpperIdent(10:11-10:15),NoSpaceOpenRound(10:15-10:16),Underscore(10:16-10:17),CloseRound(10:17-10:18),OpArrow(10:19-10:21),UpperIdent(10:22-10:25),
LowerIdent(11:1-11:8),OpAssign(11:9-11:10),OpBar(11:11-11:12),LowerIdent(11:12-11:16),OpBar(11:16-11:17),StringStart(11:18-11:19),StringPart(11:19-11:28),StringEnd(11:28-11:29),
LowerIdent(14:1-14:9),OpColon(14:10-14:11),OpenCurly(14:12-14:13),LowerIdent(14:14-14:19),OpColon(14:19-14:20),Underscore(14:21-14:22),Comma(14:22-14:23),LowerIdent(14:24-14:29),OpColon(14:29-14:30),UpperIdent(14:31-14:34),CloseCurly(14:35-14:36),OpArrow(14:37-14:39),UpperIdent(14:40-14:43),
LowerIdent(15:1-15:9),OpAssign(15:10-15:11),OpBar(15:12-15:13),LowerIdent(15:13-15:19),OpBar(15:19-15:20),LowerIdent(15:21-15:27),NoSpaceDotLowerIdent(15:27-15:33),
LowerIdent(18:1-18:14),OpColon(18:15-18:16),UpperIdent(18:17-18:23),NoSpaceOpenRound(18:23-18:24),Underscore(18:24-18:25),Comma(18:25-18:26),UpperIdent(18:27-18:30),CloseRound(18:30-18:31),OpArrow(18:32-18:34),UpperIdent(18:35-18:38),
LowerIdent(19:1-19:14),OpAssign(19:15-19:16),OpBar(19:17-19:18),LowerIdent(19:18-19:24),OpBar(19:24-19:25),
KwMatch(20:5-20:10),LowerIdent(20:11-20:17),OpenCurly(20:18-20:19),
UpperIdent(21:9-21:11),NoSpaceOpenRound(21:11-21:12),Underscore(21:12-21:13),CloseRound(21:13-21:14),OpFatArrow(21:15-21:17),StringStart(21:18-21:19),StringPart(21:19-21:26),StringEnd(21:26-21:27),Comma(21:27-21:28),
UpperIdent(22:9-22:12),NoSpaceOpenRound(22:12-22:13),LowerIdent(22:13-22:16),CloseRound(22:16-22:17),OpFatArrow(22:18-22:20),LowerIdent(22:21-22:24),Comma(22:24-22:25),
CloseCurly(23:5-23:6),
LowerIdent(26:1-26:4),OpColon(26:5-26:6),OpenRound(26:7-26:8),LowerIdent(26:8-26:9),OpArrow(26:10-26:12),LowerIdent(26:13-26:14),CloseRound(26:14-26:15),Comma(26:15-26:16),UpperIdent(26:17-26:21),NoSpaceOpenRound(26:21-26:22),LowerIdent(26:22-26:23),CloseRound(26:23-26:24),OpArrow(26:25-26:27),UpperIdent(26:28-26:32),NoSpaceOpenRound(26:32-26:33),LowerIdent(26:33-26:34),CloseRound(26:34-26:35),
LowerIdent(27:1-27:4),OpAssign(27:5-27:6),OpBar(27:7-27:8),Underscore(27:8-27:9),Comma(27:9-27:10),Underscore(27:11-27:12),OpBar(27:12-27:13),OpenSquare(27:14-27:15),CloseSquare(27:15-27:16),
LowerIdent(30:1-30:10),OpColon(30:11-30:12),NamedUnderscore(30:13-30:15),OpArrow(30:16-30:18),NamedUnderscore(30:19-30:21),OpArrow(30:22-30:24),NamedUnderscore(30:25-30:27),
LowerIdent(31:1-31:10),OpAssign(31:11-31:12),OpBar(31:13-31:14),Underscore(31:14-31:15),Comma(31:15-31:16),LowerIdent(31:17-31:18),OpBar(31:18-31:19),LowerIdent(31:20-31:21),
EndOfFile(32:1-32:1),
~~~
# PARSE
~~~clojure
(file @1.1-31.21
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-anno @3.1-3.14 (name "main")
			(ty-fn @3.8-3.14
				(_)
				(_)))
		(s-decl @4.1-4.13
			(p-ident @4.1-4.5 (raw "main"))
			(e-lambda @4.8-4.13
				(args
					(p-ident @4.9-4.10 (raw "x")))
				(e-ident @4.12-4.13 (raw "x"))))
		(s-type-anno @6.1-6.18 (name "identity")
			(ty-fn @6.12-6.18
				(ty-var @6.12-6.13 (raw "a"))
				(ty-var @6.17-6.18 (raw "a"))))
		(s-decl @7.1-7.17
			(p-ident @7.1-7.9 (raw "identity"))
			(e-lambda @7.12-7.17
				(args
					(p-ident @7.13-7.14 (raw "x")))
				(e-ident @7.16-7.17 (raw "x"))))
		(s-type-anno @10.1-10.25 (name "process")
			(ty-fn @10.11-10.25
				(ty-apply @10.11-10.18
					(ty @10.11-10.15 (name "List"))
					(_))
				(ty @10.22-10.25 (name "Str"))))
		(s-decl @11.1-11.29
			(p-ident @11.1-11.8 (raw "process"))
			(e-lambda @11.11-11.29
				(args
					(p-ident @11.12-11.16 (raw "list")))
				(e-string @11.18-11.29
					(e-string-part @11.19-11.28 (raw "processed")))))
		(s-type-anno @14.1-14.43 (name "get_data")
			(ty-fn @14.12-14.43
				(ty-record @14.12-14.36
					(anno-record-field @14.14-14.22 (name "field")
						(_))
					(anno-record-field @14.24-14.34 (name "other")
						(ty @14.31-14.34 (name "U32"))))
				(ty @14.40-14.43 (name "U32"))))
		(s-decl @15.1-15.33
			(p-ident @15.1-15.9 (raw "get_data"))
			(e-lambda @15.12-15.33
				(args
					(p-ident @15.13-15.19 (raw "record")))
				(e-field-access @15.21-15.33
					(e-ident @15.21-15.27 (raw "record"))
					(e-ident @15.27-15.33 (raw "other")))))
		(s-type-anno @18.1-18.38 (name "handle_result")
			(ty-fn @18.17-18.38
				(ty-apply @18.17-18.31
					(ty @18.17-18.23 (name "Result"))
					(_)
					(ty @18.27-18.30 (name "Str")))
				(ty @18.35-18.38 (name "Str"))))
		(s-decl @19.1-23.6
			(p-ident @19.1-19.14 (raw "handle_result"))
			(e-lambda @19.17-23.6
				(args
					(p-ident @19.18-19.24 (raw "result")))
				(e-match
					(e-ident @20.11-20.17 (raw "result"))
					(branches
						(branch @21.9-21.27
							(p-tag @21.9-21.14 (raw "Ok")
								(p-underscore))
							(e-string @21.18-21.27
								(e-string-part @21.19-21.26 (raw "success"))))
						(branch @22.9-22.24
							(p-tag @22.9-22.17 (raw "Err")
								(p-ident @22.13-22.16 (raw "msg")))
							(e-ident @22.21-22.24 (raw "msg")))))))
		(s-type-anno @26.1-26.35 (name "map")
			(ty-fn @26.7-26.35
				(ty-fn @26.8-26.14
					(ty-var @26.8-26.9 (raw "a"))
					(ty-var @26.13-26.14 (raw "b")))
				(ty-apply @26.17-26.24
					(ty @26.17-26.21 (name "List"))
					(ty-var @26.22-26.23 (raw "a")))
				(ty-apply @26.28-26.35
					(ty @26.28-26.32 (name "List"))
					(ty-var @26.33-26.34 (raw "b")))))
		(s-decl @27.1-27.16
			(p-ident @27.1-27.4 (raw "map"))
			(e-lambda @27.7-27.16
				(args
					(p-underscore)
					(p-underscore))
				(e-list @27.14-27.16)))
		(s-type-anno @30.1-30.21 (name "transform")
			(ty-fn @30.13-30.21
				(underscore-ty-var @30.13-30.15 (raw "_a"))
				(underscore-ty-var @30.19-30.21 (raw "_b"))))
		(s-malformed @30.22-30.24 (tag "multi_arrow_needs_parens"))
		(s-malformed @30.25-30.27 (tag "statement_unexpected_token"))
		(s-decl @31.1-31.21
			(p-ident @31.1-31.10 (raw "transform"))
			(e-lambda @31.13-31.21
				(args
					(p-underscore)
					(p-ident @31.17-31.18 (raw "b")))
				(e-ident @31.20-31.21 (raw "b"))))))
~~~
# FORMATTED
~~~roc
module []

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
		(p-assign @4.1-4.5 (ident "main"))
		(e-lambda @4.8-4.13
			(args
				(p-assign @4.9-4.10 (ident "x")))
			(e-lookup-local @4.12-4.13
				(p-assign @4.9-4.10 (ident "x"))))
		(annotation @4.1-4.5
			(declared-type
				(ty-fn @3.8-3.14 (effectful false)
					(ty-underscore @1.1-1.1)
					(ty-underscore @1.1-1.1)))))
	(d-let
		(p-assign @7.1-7.9 (ident "identity"))
		(e-lambda @7.12-7.17
			(args
				(p-assign @7.13-7.14 (ident "x")))
			(e-lookup-local @7.16-7.17
				(p-assign @7.13-7.14 (ident "x"))))
		(annotation @7.1-7.9
			(declared-type
				(ty-fn @6.12-6.18 (effectful false)
					(ty-var @6.12-6.13 (name "a"))
					(ty-var @6.17-6.18 (name "a"))))))
	(d-let
		(p-assign @11.1-11.8 (ident "process"))
		(e-lambda @11.11-11.29
			(args
				(p-assign @11.12-11.16 (ident "list")))
			(e-string @11.18-11.29
				(e-literal @11.19-11.28 (string "processed"))))
		(annotation @11.1-11.8
			(declared-type
				(ty-fn @10.11-10.25 (effectful false)
					(ty-apply @10.11-10.18 (symbol "List")
						(ty-underscore @10.16-10.16))
					(ty @10.22-10.25 (name "Str"))))))
	(d-let
		(p-assign @15.1-15.9 (ident "get_data"))
		(e-lambda @15.12-15.33
			(args
				(p-assign @15.13-15.19 (ident "record")))
			(e-dot-access @15.21-15.33 (field "other")
				(receiver
					(e-lookup-local @15.21-15.27
						(p-assign @15.13-15.19 (ident "record"))))))
		(annotation @15.1-15.9
			(declared-type
				(ty-fn @14.12-14.43 (effectful false)
					(ty-record @14.12-14.36
						(field (field "field")
							(ty-underscore @1.1-1.1))
						(field (field "other")
							(ty @14.31-14.34 (name "U32"))))
					(ty @14.40-14.43 (name "U32"))))))
	(d-let
		(p-assign @19.1-19.14 (ident "handle_result"))
		(e-closure @19.17-23.6
			(captures
				(capture @22.13-22.16 (ident "msg")))
			(e-lambda @19.17-23.6
				(args
					(p-assign @19.18-19.24 (ident "result")))
				(e-match @20.5-23.6
					(match @20.5-23.6
						(cond
							(e-lookup-local @20.11-20.17
								(p-assign @19.18-19.24 (ident "result"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal @21.9-21.14
											(p-applied-tag @21.9-21.14))))
								(value
									(e-string @21.18-21.27
										(e-literal @21.19-21.26 (string "success")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal @22.9-22.17
											(p-applied-tag @22.9-22.17))))
								(value
									(e-lookup-local @22.21-22.24
										(p-assign @22.13-22.16 (ident "msg"))))))))))
		(annotation @19.1-19.14
			(declared-type
				(ty-fn @18.17-18.38 (effectful false)
					(ty-apply @18.17-18.31 (symbol "Result")
						(ty-underscore @18.24-18.24)
						(ty @18.27-18.30 (name "Str")))
					(ty @18.35-18.38 (name "Str"))))))
	(d-let
		(p-assign @27.1-27.4 (ident "map"))
		(e-lambda @27.7-27.16
			(args
				(p-underscore @27.8-27.9)
				(p-underscore @27.11-27.12))
			(e-empty_list @27.14-27.16))
		(annotation @27.1-27.4
			(declared-type
				(ty-fn @26.7-26.35 (effectful false)
					(ty-parens @26.7-26.15
						(ty-fn @26.8-26.14 (effectful false)
							(ty-var @26.8-26.9 (name "a"))
							(ty-var @26.13-26.14 (name "b"))))
					(ty-apply @26.17-26.24 (symbol "List")
						(ty-var @26.22-26.23 (name "a")))
					(ty-apply @26.28-26.35 (symbol "List")
						(ty-var @26.33-26.34 (name "b")))))))
	(d-let
		(p-assign @31.1-31.10 (ident "transform"))
		(e-lambda @31.13-31.21
			(args
				(p-underscore @31.14-31.15)
				(p-assign @31.17-31.18 (ident "b")))
			(e-lookup-local @31.20-31.21
				(p-assign @31.17-31.18 (ident "b"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.5 (type "_arg -> _ret"))
		(patt @7.1-7.9 (type "a -> a"))
		(patt @11.1-11.8 (type "List(_c) -> Str"))
		(patt @15.1-15.9 (type "{ field: _field2, other: U32 } -> U32"))
		(patt @19.1-19.14 (type "Result(ok, Str) -> Str"))
		(patt @27.1-27.4 (type "a -> b, List(a) -> List(b)"))
		(patt @31.1-31.10 (type "_arg, _arg2 -> _ret")))
	(expressions
		(expr @4.8-4.13 (type "_arg -> _ret"))
		(expr @7.12-7.17 (type "a -> a"))
		(expr @11.11-11.29 (type "List(_c) -> Str"))
		(expr @15.12-15.33 (type "{ field: _field2, other: U32 } -> U32"))
		(expr @19.17-23.6 (type "Result(ok, Str) -> Str"))
		(expr @27.7-27.16 (type "a -> b, List(a) -> List(b)"))
		(expr @31.13-31.21 (type "_arg, _arg2 -> _ret"))))
~~~
