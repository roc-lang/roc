# META
~~~ini
description=Names bound by a top-level destructure depend on their def like plain names do, so later defs may use them
type=file
~~~
# SOURCE
~~~roc
{x} = { x: 1 }
later = x + 1
{n} = { n: "s" }
f = |_| n
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,LowerIdent,CloseCurly,OpAssign,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
OpenCurly,LowerIdent,CloseCurly,OpAssign,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-record
				(field (name "x") (rest false)))
			(e-record
				(field (field "x")
					(e-int (raw "1")))))
		(s-decl
			(p-ident (raw "later"))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-int (raw "1"))))
		(s-decl
			(p-record
				(field (name "n") (rest false)))
			(e-record
				(field (field "n")
					(e-string
						(e-string-part (raw "s"))))))
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-underscore))
				(e-ident (raw "n"))))))
~~~
# FORMATTED
~~~roc
{ x } = { x: 1 }

later = x + 1

{ n } = { n: "s" }

f = |_| n
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-record-destructure
			(destructs
				(record-destruct (label "x") (ident "x")
					(required
						(p-assign (ident "x"))))))
		(e-record
			(fields
				(field (name "x")
					(e-num (value "1"))))))
	(d-let
		(p-assign (ident "later"))
		(e-dispatch-call (method "plus") (constraint-fn-var 244)
			(receiver
				(e-lookup-local
					(p-assign (ident "x"))))
			(args
				(e-num (value "1")))))
	(d-let
		(p-record-destructure
			(destructs
				(record-destruct (label "n") (ident "n")
					(required
						(p-assign (ident "n"))))))
		(e-record
			(fields
				(field (name "n")
					(e-string
						(e-literal (string "s")))))))
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-underscore))
			(e-lookup-local
				(p-assign (ident "n"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec"))
		(patt (type "_arg -> Str")))
	(expressions
		(expr (type "{ x: Dec }"))
		(expr (type "Dec"))
		(expr (type "{ n: Str }"))
		(expr (type "_arg -> Str"))))
~~~
