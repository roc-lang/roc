# META
~~~ini
description=Mono test: closure returns closure with captured variable, verifying lifted patterns
type=mono
skip=true
# TODO: cross-def closure evaluation — see TODO_REPL_FAILURES.md §1
~~~
# SOURCE
~~~roc
# A function that returns a closure capturing a variable
# This tests that lifted function patterns are properly created
make_adder = |x| |y| x + y

# Use the closure maker
add_five = make_adder(5.I64)
result = add_five(10.I64)
~~~
# MONO
~~~roc
make_adder : a -> (b -> a) where [a.plus : a, b -> a]
make_adder = |x| |y| x + y

add_five : I64 -> I64
add_five = make_adder(5.I64)

result : I64
result = add_five(10.I64)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
COMPTIME EVAL ERROR - mono_static_dispatch_closure.md:7:10:7:26
# PROBLEMS
**COMPTIME EVAL ERROR**
This definition could not be evaluated at compile time:
**mono_static_dispatch_closure.md:7:10:7:26:**
```roc
result = add_five(10.I64)
```
         ^^^^^^^^^^^^^^^^

The evaluation failed with error:

    RuntimeError

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,NoSpaceDotUpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,NoSpaceDotUpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "make_adder"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-lambda
					(args
						(p-ident (raw "y")))
					(e-binop (op "+")
						(e-ident (raw "x"))
						(e-ident (raw "y"))))))
		(s-decl
			(p-ident (raw "add_five"))
			(e-apply
				(e-ident (raw "make_adder"))
				(e-typed-int (raw "5") (type ".I64"))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "add_five"))
				(e-typed-int (raw "10") (type ".I64"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "make_adder"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-closure
				(captures
					(capture (ident "x")))
				(e-lambda
					(args
						(p-assign (ident "y")))
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "y"))))))))
	(d-let
		(p-assign (ident "add_five"))
		(e-call
			(e-lookup-local
				(p-assign (ident "make_adder")))
			(e-typed-int (value "5") (type "I64"))))
	(d-let
		(p-assign (ident "result"))
		(e-call
			(e-lookup-local
				(p-assign (ident "add_five")))
			(e-typed-int (value "10") (type "I64")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> (b -> a) where [a.plus : a, b -> a]"))
		(patt (type "I64 -> I64"))
		(patt (type "I64")))
	(expressions
		(expr (type "a -> (b -> a) where [a.plus : a, b -> a]"))
		(expr (type "I64 -> I64"))
		(expr (type "I64"))))
~~~
