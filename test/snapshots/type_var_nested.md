# META
~~~ini
description=Type variables nested within complex type constructors
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "platform.roc" }

# Map over Result type
map_result : Result(a, e), (a -> b) -> Result(b, e)
map_result = |result, transform| {
    match result {
        Ok(value) => Ok(transform(value))
        Err(error) => Err(error)
    }
}

# Simple identity function with type variable
identity : a -> a
identity = |x| x

# Nested type variables in records
make_pair : a, b -> { first: a, second: b }
make_pair = |x, y| { first: x, second: y }

# Function that works with lists of any type
list_length : List(_a) -> U64
list_length = |_lst| 42

# Nested Result types
wrap_in_result : a -> Result(Result(a, Str), Str)
wrap_in_result = |value| Ok(Ok(value))

main = |_| "done"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,NamedUnderscore,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,Int,
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,CloseRound,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "platform.roc"))))))
	(statements
		(s-type-anno (name "map_result")
			(ty-fn
				(ty-apply
					(ty (name "Result"))
					(ty-var (raw "a"))
					(ty-var (raw "e")))
				(ty-fn
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-apply
					(ty (name "Result"))
					(ty-var (raw "b"))
					(ty-var (raw "e")))))
		(s-decl
			(p-ident (raw "map_result"))
			(e-lambda
				(args
					(p-ident (raw "result"))
					(p-ident (raw "transform")))
				(e-block
					(statements
						(e-match
							(e-ident (raw "result"))
							(branches
								(branch
									(p-tag (raw "Ok")
										(p-ident (raw "value")))
									(e-apply
										(e-tag (raw "Ok"))
										(e-apply
											(e-ident (raw "transform"))
											(e-ident (raw "value")))))
								(branch
									(p-tag (raw "Err")
										(p-ident (raw "error")))
									(e-apply
										(e-tag (raw "Err"))
										(e-ident (raw "error"))))))))))
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
		(s-type-anno (name "make_pair")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b"))
				(ty-record
					(anno-record-field (name "first")
						(ty-var (raw "a")))
					(anno-record-field (name "second")
						(ty-var (raw "b"))))))
		(s-decl
			(p-ident (raw "make_pair"))
			(e-lambda
				(args
					(p-ident (raw "x"))
					(p-ident (raw "y")))
				(e-record
					(field (field "first")
						(e-ident (raw "x")))
					(field (field "second")
						(e-ident (raw "y"))))))
		(s-type-anno (name "list_length")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(underscore-ty-var (raw "_a")))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "list_length"))
			(e-lambda
				(args
					(p-ident (raw "_lst")))
				(e-int (raw "42"))))
		(s-type-anno (name "wrap_in_result")
			(ty-fn
				(ty-var (raw "a"))
				(ty-apply
					(ty (name "Result"))
					(ty-apply
						(ty (name "Result"))
						(ty-var (raw "a"))
						(ty (name "Str")))
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "wrap_in_result"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-apply
					(e-tag (raw "Ok"))
					(e-apply
						(e-tag (raw "Ok"))
						(e-ident (raw "value"))))))
		(s-decl
			(p-ident (raw "main"))
			(e-lambda
				(args
					(p-underscore))
				(e-string
					(e-string-part (raw "done")))))))
~~~
# FORMATTED
~~~roc
app [main] { pf: platform "platform.roc" }

# Map over Result type
map_result : Result(a, e), (a -> b) -> Result(b, e)
map_result = |result, transform| {
	match result {
		Ok(value) => Ok(transform(value))
		Err(error) => Err(error)
	}
}

# Simple identity function with type variable
identity : a -> a
identity = |x| x

# Nested type variables in records
make_pair : a, b -> { first : a, second : b }
make_pair = |x, y| { first: x, second: y }

# Function that works with lists of any type
list_length : List(_a) -> U64
list_length = |_lst| 42

# Nested Result types
wrap_in_result : a -> Result(Result(a, Str), Str)
wrap_in_result = |value| Ok(Ok(value))

main = |_| "done"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "map_result"))
		(e-closure
			(captures
				(capture (ident "error"))
				(capture (ident "value")))
			(e-lambda
				(args
					(p-assign (ident "result"))
					(p-assign (ident "transform")))
				(e-block
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
										(e-tag (name "Ok")
											(args
												(e-call
													(e-lookup-local
														(p-assign (ident "transform")))
													(e-lookup-local
														(p-assign (ident "value"))))))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-tag (name "Err")
											(args
												(e-lookup-local
													(p-assign (ident "error")))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Result") (external-module "Result")
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "e")))
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-rigid-var (name "b"))))
				(ty-apply (name "Result") (external-module "Result")
					(ty-rigid-var-lookup (ty-rigid-var (name "b")))
					(ty-rigid-var-lookup (ty-rigid-var (name "e")))))))
	(d-let
		(p-assign (ident "identity"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x"))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
	(d-let
		(p-assign (ident "make_pair"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "y")))
			(e-record
				(fields
					(field (name "first")
						(e-lookup-local
							(p-assign (ident "x"))))
					(field (name "second")
						(e-lookup-local
							(p-assign (ident "y")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))
				(ty-record
					(field (field "first")
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(field (field "second")
						(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))))
	(d-let
		(p-assign (ident "list_length"))
		(e-lambda
			(args
				(p-assign (ident "_lst")))
			(e-num (value "42")))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-rigid-var (name "_a")))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "wrap_in_result"))
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-tag (name "Ok")
				(args
					(e-tag (name "Ok")
						(args
							(e-lookup-local
								(p-assign (ident "value"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-apply (name "Result") (external-module "Result")
					(ty-apply (name "Result") (external-module "Result")
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-lookup (name "Str") (external-module "Str")))
					(ty-lookup (name "Str") (external-module "Str"))))))
	(d-let
		(p-assign (ident "main"))
		(e-lambda
			(args
				(p-underscore))
			(e-string
				(e-literal (string "done"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Result(a, e), a -> b -> Result(b, e)"))
		(patt (type "a -> a"))
		(patt (type "a, b -> { first: a, second: b }"))
		(patt (type "List(_a) -> Num(Int(Unsigned64))"))
		(patt (type "a -> Result(Result(a, Error), Error)"))
		(patt (type "_arg -> Error")))
	(expressions
		(expr (type "Result(a, e), a -> b -> Result(b, e)"))
		(expr (type "a -> a"))
		(expr (type "a, b -> { first: a, second: b }"))
		(expr (type "List(_a) -> Num(Int(Unsigned64))"))
		(expr (type "a -> Result(Result(a, Error), Error)"))
		(expr (type "_arg -> Error"))))
~~~
