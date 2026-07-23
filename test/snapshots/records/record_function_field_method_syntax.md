# META
~~~ini
description=Method call syntax is not reinterpreted as calling a record field
type=snippet
~~~
# SOURCE
~~~roc
r = { f: |x| x }

result = r.f(1)
~~~
# EXPECTED
MISSING METHOD - record_function_field_method_syntax.md:3:12:3:13
# PROBLEMS

┌────────────────┐
│ MISSING METHOD ├─ This `f` method is being called on a value whose type ────┐
└┬───────────────┘  doesn't have that method.                                 │
 │                                                                            │
 │  result = r.f(1)                                                           │
 │             ‾                                                              │
 └─────────────────────────────── record_function_field_method_syntax.md:3:12 ┘

    The value's type, which does not have a method named `f`, is:

        { f: a -> a }

# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,OpBar,LowerIdent,OpBar,LowerIdent,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "r"))
			(e-record
				(field (field "f")
					(e-lambda
						(args
							(p-ident (raw "x")))
						(e-ident (raw "x"))))))
		(s-decl
			(p-ident (raw "result"))
			(e-method-call (method ".f")
				(receiver
					(e-ident (raw "r")))
				(args
					(e-int (raw "1")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "r"))
		(e-record
			(fields
				(field (name "f")
					(e-lambda
						(args
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "x"))))))))
	(d-let
		(p-assign (ident "result"))
		(e-dispatch-call (method "f") (constraint-fn-var 196)
			(receiver
				(e-lookup-local
					(p-assign (ident "r"))))
			(args
				(e-num (value "1"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ f: a -> a }"))
		(patt (type "Error")))
	(expressions
		(expr (type "{ f: a -> a }"))
		(expr (type "Error"))))
~~~
