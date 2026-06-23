# META
~~~ini
description=Boolean closure type checking - should have no errors
type=expr
~~~
# SOURCE
~~~roc
(|x| !x)(True)
~~~
# EXPECTED
MISSING METHOD - bool_closure_type_check.md:1:6:1:8
# PROBLEMS

┌────────────────┐
│ MISSING METHOD ├─ This `not` method is being called on a value whose type ──┐
└┬───────────────┘  doesn't have that method.                                 │
 │                                                                            │
 │  (|x| !x)(True)                                                            │
 │       ‾‾                                                                   │
 └──────────────────────────────────────────── bool_closure_type_check.md:1:6 ┘

    The value's type, which does not have a method named `not`, is:

        [True, ..]

# TOKENS
~~~zig
OpenRound,OpBar,LowerIdent,OpBar,OpBang,LowerIdent,CloseRound,NoSpaceOpenRound,UpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-tuple
		(e-lambda
			(args
				(p-ident (raw "x")))
			(unary "!"
				(e-ident (raw "x")))))
	(e-tag (raw "True")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call (constraint-fn-var 15)
	(e-lambda
		(args
			(p-assign (ident "x")))
		(e-dispatch-call (method "not") (constraint-fn-var 12)
			(receiver
				(e-lookup-local
					(p-assign (ident "x"))))
			(args)))
	(e-tag (name "True")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
