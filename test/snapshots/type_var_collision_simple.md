# META
~~~ini
description=Simple demonstration that type variable names avoid collision with existing identifiers
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Define some variables that would normally be used for type variables
a = 1
b = 2
c = 3

# This identity function should get type 'd -> d' since a, b, c are taken
identity = |x| x

# This function should get type 'e -> e' since d is now also taken
identity2 = |y| y

# This function with two parameters should get types 'f, g -> (f, g)'
pair = |first, second| (first, second)

main! = |_| {
    result1 = identity(42)
    result2 = identity2("hello")
    result3 = pair(result1, result2)
    
    a + b + c
}
~~~
# EXPECTED
UNUSED VARIABLE - type_var_collision_simple.md:20:5:20:12
# PROBLEMS
**UNUSED VARIABLE**
Variable `result3` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result3` to suppress this warning.
The unused variable is declared here:
**type_var_collision_simple.md:20:5:20:12:**
```roc
    result3 = pair(result1, result2)
```
    ^^^^^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
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
		(s-decl
			(p-ident (raw "a"))
			(e-int (raw "1")))
		(s-decl
			(p-ident (raw "b"))
			(e-int (raw "2")))
		(s-decl
			(p-ident (raw "c"))
			(e-int (raw "3")))
		(s-decl
			(p-ident (raw "identity"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(s-decl
			(p-ident (raw "identity2"))
			(e-lambda
				(args
					(p-ident (raw "y")))
				(e-ident (raw "y"))))
		(s-decl
			(p-ident (raw "pair"))
			(e-lambda
				(args
					(p-ident (raw "first"))
					(p-ident (raw "second")))
				(e-tuple
					(e-ident (raw "first"))
					(e-ident (raw "second")))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "result1"))
							(e-apply
								(e-ident (raw "identity"))
								(e-int (raw "42"))))
						(s-decl
							(p-ident (raw "result2"))
							(e-apply
								(e-ident (raw "identity2"))
								(e-string
									(e-string-part (raw "hello")))))
						(s-decl
							(p-ident (raw "result3"))
							(e-apply
								(e-ident (raw "pair"))
								(e-ident (raw "result1"))
								(e-ident (raw "result2"))))
						(e-binop (op "+")
							(e-binop (op "+")
								(e-ident (raw "a"))
								(e-ident (raw "b")))
							(e-ident (raw "c")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Define some variables that would normally be used for type variables
a = 1
b = 2
c = 3

# This identity function should get type 'd -> d' since a, b, c are taken
identity = |x| x

# This function should get type 'e -> e' since d is now also taken
identity2 = |y| y

# This function with two parameters should get types 'f, g -> (f, g)'
pair = |first, second| (first, second)

main! = |_| {
	result1 = identity(42)
	result2 = identity2("hello")
	result3 = pair(result1, result2)

	a + b + c
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "b"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "c"))
		(e-num (value "3")))
	(d-let
		(p-assign (ident "identity"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x")))))
	(d-let
		(p-assign (ident "identity2"))
		(e-lambda
			(args
				(p-assign (ident "y")))
			(e-lookup-local
				(p-assign (ident "y")))))
	(d-let
		(p-assign (ident "pair"))
		(e-lambda
			(args
				(p-assign (ident "first"))
				(p-assign (ident "second")))
			(e-tuple
				(elems
					(e-lookup-local
						(p-assign (ident "first")))
					(e-lookup-local
						(p-assign (ident "second")))))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "pair"))
				(capture (ident "b"))
				(capture (ident "c"))
				(capture (ident "identity2"))
				(capture (ident "a"))
				(capture (ident "identity")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-let
						(p-assign (ident "result1"))
						(e-call
							(e-lookup-local
								(p-assign (ident "identity")))
							(e-num (value "42"))))
					(s-let
						(p-assign (ident "result2"))
						(e-call
							(e-lookup-local
								(p-assign (ident "identity2")))
							(e-string
								(e-literal (string "hello")))))
					(s-let
						(p-assign (ident "result3"))
						(e-call
							(e-lookup-local
								(p-assign (ident "pair")))
							(e-lookup-local
								(p-assign (ident "result1")))
							(e-lookup-local
								(p-assign (ident "result2")))))
					(e-binop (op "add")
						(e-binop (op "add")
							(e-lookup-local
								(p-assign (ident "a")))
							(e-lookup-local
								(p-assign (ident "b"))))
						(e-lookup-local
							(p-assign (ident "c")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "d -> d"))
		(patt (type "d -> d"))
		(patt (type "_arg, _arg2 -> (_field, _field2)"))
		(patt (type "_arg -> Num(_size)")))
	(expressions
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "d -> d"))
		(expr (type "d -> d"))
		(expr (type "_arg, _arg2 -> (_field, _field2)"))
		(expr (type "_arg -> Num(_size)"))))
~~~
