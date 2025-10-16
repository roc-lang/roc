# META
~~~ini
description=Basic type annotations with type variables and application
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Test generic identity function
identity : a -> a
identity = |x| x

# Test function with multiple type parameters
combine : a, b -> (a, b)
combine = |first, second| (first, second)

# Test type application with concrete types
addOne : U64 -> U64
addOne = |n| n + 1

main! = |_| {
    # Test identity with different types
    num = identity(42)
    text = identity("hello")

    # Test combine function
    pair = combine(num, text)

    # Test concrete function
    result = addOne(5)

    result
}
~~~
# EXPECTED
UNUSED VARIABLE - type_annotation_basic.md:21:5:21:9
# PROBLEMS
**UNUSED VARIABLE**
Variable `pair` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_pair` to suppress this warning.
The unused variable is declared here:
**type_annotation_basic.md:21:5:21:9:**
```roc
    pair = combine(num, text)
```
    ^^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
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
		(s-type-anno (name "combine")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b"))
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "b")))))
		(s-decl
			(p-ident (raw "combine"))
			(e-lambda
				(args
					(p-ident (raw "first"))
					(p-ident (raw "second")))
				(e-tuple
					(e-ident (raw "first"))
					(e-ident (raw "second")))))
		(s-type-anno (name "addOne")
			(ty-fn
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "addOne"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-binop (op "+")
					(e-ident (raw "n"))
					(e-int (raw "1")))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "num"))
							(e-apply
								(e-ident (raw "identity"))
								(e-int (raw "42"))))
						(s-decl
							(p-ident (raw "text"))
							(e-apply
								(e-ident (raw "identity"))
								(e-string
									(e-string-part (raw "hello")))))
						(s-decl
							(p-ident (raw "pair"))
							(e-apply
								(e-ident (raw "combine"))
								(e-ident (raw "num"))
								(e-ident (raw "text"))))
						(s-decl
							(p-ident (raw "result"))
							(e-apply
								(e-ident (raw "addOne"))
								(e-int (raw "5"))))
						(e-ident (raw "result"))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Test generic identity function
identity : a -> a
identity = |x| x

# Test function with multiple type parameters
combine : a, b -> (a, b)
combine = |first, second| (first, second)

# Test type application with concrete types
addOne : U64 -> U64
addOne = |n| n + 1

main! = |_| {
	# Test identity with different types
	num = identity(42)
	text = identity("hello")

	# Test combine function
	pair = combine(num, text)

	# Test concrete function
	result = addOne(5)

	result
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
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
		(p-assign (ident "combine"))
		(e-lambda
			(args
				(p-assign (ident "first"))
				(p-assign (ident "second")))
			(e-tuple
				(elems
					(e-lookup-local
						(p-assign (ident "first")))
					(e-lookup-local
						(p-assign (ident "second"))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b"))
					(ty-tuple
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))))
	(d-let
		(p-assign (ident "addOne"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "n")))
				(e-num (value "1"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "U64") (builtin))
					(ty-lookup (name "U64") (builtin))))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "combine"))
				(capture (ident "identity"))
				(capture (ident "addOne")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-let
						(p-assign (ident "num"))
						(e-call
							(e-lookup-local
								(p-assign (ident "identity")))
							(e-num (value "42"))))
					(s-let
						(p-assign (ident "text"))
						(e-call
							(e-lookup-local
								(p-assign (ident "identity")))
							(e-string
								(e-literal (string "hello")))))
					(s-let
						(p-assign (ident "pair"))
						(e-call
							(e-lookup-local
								(p-assign (ident "combine")))
							(e-lookup-local
								(p-assign (ident "num")))
							(e-lookup-local
								(p-assign (ident "text")))))
					(s-let
						(p-assign (ident "result"))
						(e-call
							(e-lookup-local
								(p-assign (ident "addOne")))
							(e-num (value "5"))))
					(e-lookup-local
						(p-assign (ident "result"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a"))
		(patt (type "a, b -> (a, b)"))
		(patt (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(patt (type "_arg -> Num(Int(Unsigned64))")))
	(expressions
		(expr (type "a -> a"))
		(expr (type "a, b -> (a, b)"))
		(expr (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(expr (type "_arg -> Num(Int(Unsigned64))"))))
~~~
