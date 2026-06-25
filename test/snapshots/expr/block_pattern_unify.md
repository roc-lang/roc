# META
~~~ini
description=Block with pattern unification testing
type=expr
~~~
# SOURCE
~~~roc
{
    x = 42
    str = "hello"
    result = x + 5
    result
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "42")))
		(s-decl
			(p-ident (raw "str"))
			(e-string
				(e-string-part (raw "hello"))))
		(s-decl
			(p-ident (raw "result"))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-int (raw "5"))))
		(e-ident (raw "result"))))
~~~
# FORMATTED
~~~roc
{
	x = 42
	str = "hello"
	result = x + 5
	result
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "42")))
	(s-let
		(p-assign (ident "str"))
		(e-string
			(e-literal (string "hello"))))
	(s-let
		(p-assign (ident "result"))
		(e-dispatch-call (method "plus") (constraint-fn-var 103)
			(receiver
				(e-lookup-local
					(p-assign (ident "x"))))
			(args
				(e-num (value "5")))))
	(e-lookup-local
		(p-assign (ident "result"))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
