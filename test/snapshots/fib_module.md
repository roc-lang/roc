# META
~~~ini
description=Fibonacci fn
type=file
~~~
# SOURCE
~~~roc
module [fib]

fib = |n| if n <= 1 n else fib(n - 1) + fib(n - 2)
~~~
# EXPECTED
NIL
# PROBLEMS
**INVALID IF CONDITION**
This `if` condition needs to be a _Bool_:
**fib_module.md:3:14:**
```roc
fib = |n| if n <= 1 n else fib(n - 1) + fib(n - 2)
```
             ^^^^^^

Right now, it has the type:
    _Num(_size)_

Every `if` condition must evaluate to a _Bool_–either `True` or `False`.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),OpBar(3:7-3:8),LowerIdent(3:8-3:9),OpBar(3:9-3:10),KwIf(3:11-3:13),LowerIdent(3:14-3:15),OpLessThanOrEq(3:16-3:18),Int(3:19-3:20),LowerIdent(3:21-3:22),KwElse(3:23-3:27),LowerIdent(3:28-3:31),NoSpaceOpenRound(3:31-3:32),LowerIdent(3:32-3:33),OpBinaryMinus(3:34-3:35),Int(3:36-3:37),CloseRound(3:37-3:38),OpPlus(3:39-3:40),LowerIdent(3:41-3:44),NoSpaceOpenRound(3:44-3:45),LowerIdent(3:45-3:46),OpBinaryMinus(3:47-3:48),Int(3:49-3:50),CloseRound(3:50-3:51),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.51
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident @1.9-1.12
				(text "fib"))))
	(statements
		(s-decl @3.1-3.51
			(p-ident @3.1-3.4 (raw "fib"))
			(e-lambda @3.7-3.51
				(args
					(p-ident @3.8-3.9 (raw "n")))
				(e-if-then-else @3.11-3.51
					(e-binop @3.14-3.20 (op "<=")
						(e-ident @3.14-3.15 (raw "n"))
						(e-int @3.19-3.20 (raw "1")))
					(e-ident @3.21-3.22 (raw "n"))
					(e-binop @3.28-3.51 (op "+")
						(e-apply @3.28-3.38
							(e-ident @3.28-3.31 (raw "fib"))
							(e-binop @3.32-3.37 (op "-")
								(e-ident @3.32-3.33 (raw "n"))
								(e-int @3.36-3.37 (raw "1"))))
						(e-apply @3.41-3.51
							(e-ident @3.41-3.44 (raw "fib"))
							(e-binop @3.45-3.50 (op "-")
								(e-ident @3.45-3.46 (raw "n"))
								(e-int @3.49-3.50 (raw "2"))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.4 (ident "fib"))
		(e-closure @3.7-3.51
			(captures
				(capture @3.1-3.4 (ident "fib")))
			(e-lambda @3.7-3.51
				(args
					(p-assign @3.8-3.9 (ident "n")))
				(e-if @3.11-3.51
					(if-branches
						(if-branch
							(e-binop @3.14-3.20 (op "le")
								(e-lookup-local @3.14-3.15
									(p-assign @3.8-3.9 (ident "n")))
								(e-num @3.19-3.20 (value "1")))
							(e-lookup-local @3.21-3.22
								(p-assign @3.8-3.9 (ident "n")))))
					(if-else
						(e-binop @3.28-3.51 (op "add")
							(e-call @3.28-3.38
								(e-binop @3.32-3.37 (op "sub")
									(e-lookup-local @3.32-3.33
										(p-assign @3.8-3.9 (ident "n")))
									(e-num @3.36-3.37 (value "1"))))
							(e-call @3.41-3.51
								(e-binop @3.45-3.50 (op "sub")
									(e-lookup-local @3.45-3.46
										(p-assign @3.8-3.9 (ident "n")))
									(e-num @3.49-3.50 (value "2"))))))))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Bool"))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "True"))
			(tag_name @1.1-1.1 (name "False"))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Result")
			(ty-args
				(ty-rigid-var @1.1-1.1 (name "ok"))
				(ty-rigid-var @1.1-1.1 (name "err"))))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "Ok"))
			(tag_name @1.1-1.1 (name "Err")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.4 (type "Error -> Error")))
	(type_decls
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err"))))))
	(expressions
		(expr @3.7-3.51 (type "Error -> Error"))))
~~~
