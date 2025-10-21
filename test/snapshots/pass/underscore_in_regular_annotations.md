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
LowerIdent,OpColon,Underscore,OpArrow,Underscore,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,Underscore,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,Underscore,Comma,UpperIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,StringStart,StringPart,StringEnd,Comma,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,Comma,
CloseCurly,
LowerIdent,OpColon,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,Comma,Underscore,OpBar,OpenSquare,CloseSquare,
LowerIdent,OpColon,NamedUnderscore,OpArrow,NamedUnderscore,OpArrow,NamedUnderscore,
LowerIdent,OpAssign,OpBar,Underscore,Comma,LowerIdent,OpBar,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "main")
			(ty-fn
				(_)
				(_)))
		(s-decl
			(p-ident (raw "main"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(s-type-anno (name "identity")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "a"))))
		(s-decl
			(p-ident (raw "identity"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(s-type-anno (name "process")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(_))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "process"))
			(e-lambda
				(args
					(p-ident (raw "list")))
				(e-string
					(e-string-part (raw "processed")))))
		(s-type-anno (name "get_data")
			(ty-fn
				(ty-record
					(anno-record-field (name "field")
						(_))
					(anno-record-field (name "other")
						(ty (name "U32"))))
				(ty (name "U32"))))
		(s-decl
			(p-ident (raw "get_data"))
			(e-lambda
				(args
					(p-ident (raw "record")))
				(e-field-access
					(e-ident (raw "record"))
					(e-ident (raw "other")))))
		(s-type-anno (name "handle_result")
			(ty-fn
				(ty-apply
					(ty (name "Result"))
					(_)
					(ty (name "Str")))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "handle_result"))
			(e-lambda
				(args
					(p-ident (raw "result")))
				(e-match
					(e-ident (raw "result"))
					(branches
						(branch
							(p-tag (raw "Ok")
								(p-underscore))
							(e-string
								(e-string-part (raw "success"))))
						(branch
							(p-tag (raw "Err")
								(p-ident (raw "msg")))
							(e-ident (raw "msg")))))))
		(s-type-anno (name "map")
			(ty-fn
				(ty-fn
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "a")))
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "b")))))
		(s-decl
			(p-ident (raw "map"))
			(e-lambda
				(args
					(p-underscore)
					(p-underscore))
				(e-list)))
		(s-type-anno (name "transform")
			(ty-fn
				(underscore-ty-var (raw "_a"))
				(underscore-ty-var (raw "_b"))))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "transform"))
			(e-lambda
				(args
					(p-underscore)
					(p-ident (raw "b")))
				(e-ident (raw "b"))))))
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
		(p-assign (ident "main"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-underscore)
					(ty-underscore)))))
	(d-let
		(p-assign (ident "identity"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-rigid-var (name "a"))
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "process"))
		(e-lambda
			(args
				(p-assign (ident "list")))
			(e-string
				(e-literal (string "processed"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-apply (name "List") (builtin)
						(ty-underscore))
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "get_data"))
		(e-lambda
			(args
				(p-assign (ident "record")))
			(e-dot-access (field "other")
				(receiver
					(e-lookup-local
						(p-assign (ident "record"))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-record
						(field (field "field")
							(ty-underscore))
						(field (field "other")
							(ty-lookup (name "U32") (builtin))))
					(ty-lookup (name "U32") (builtin))))))
	(d-let
		(p-assign (ident "handle_result"))
		(e-closure
			(captures
				(capture (ident "msg")))
			(e-lambda
				(args
					(p-assign (ident "result")))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "result"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-string
										(e-literal (string "success")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-lookup-local
										(p-assign (ident "msg"))))))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-apply (name "Result") (module "Result")
						(ty-underscore)
						(ty-lookup (name "Str") (builtin)))
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "map"))
		(e-lambda
			(args
				(p-underscore)
				(p-underscore))
			(e-empty_list))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-parens
						(ty-fn (effectful false)
							(ty-rigid-var (name "a"))
							(ty-rigid-var (name "b"))))
					(ty-apply (name "List") (builtin)
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-apply (name "List") (builtin)
						(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))))
	(d-let
		(p-assign (ident "transform"))
		(e-lambda
			(args
				(p-underscore)
				(p-assign (ident "b")))
			(e-lookup-local
				(p-assign (ident "b"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> _ret"))
		(patt (type "a -> a"))
		(patt (type "List(_elem) -> Str"))
		(patt (type "{ field: _field2, other: Num(Int(Unsigned32)) } -> Num(Int(Unsigned32))"))
		(patt (type "Result(_c, Str) -> Str"))
		(patt (type "a -> b, List(a) -> List(b)"))
		(patt (type "_arg, c -> c")))
	(expressions
		(expr (type "_arg -> _ret"))
		(expr (type "a -> a"))
		(expr (type "List(_elem) -> Str"))
		(expr (type "{ field: _field2, other: Num(Int(Unsigned32)) } -> Num(Int(Unsigned32))"))
		(expr (type "Result(_c, Str) -> Str"))
		(expr (type "a -> b, List(a) -> List(b)"))
		(expr (type "_arg, c -> c"))))
~~~
