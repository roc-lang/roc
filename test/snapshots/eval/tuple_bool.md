# META
~~~ini
description=Tuple containing variations on boolean values
type=expr
~~~
# SOURCE
~~~roc
(True, False, Bool.True, Bool.False, !True, !False, True and False, !True or !True)
~~~
# EXPECTED
MISSING METHOD - tuple_bool.md:1:38:1:43
MISSING METHOD - tuple_bool.md:1:45:1:51
MISSING METHOD - tuple_bool.md:1:69:1:74
MISSING METHOD - tuple_bool.md:1:78:1:83
# PROBLEMS

┌────────────────┐
│ MISSING METHOD ├─ This `not` method is being called on a value whose type ──┐
└┬───────────────┘  doesn't have that method.                                 │
 │                                                                            │
 │  ….False, !True, !False, True and False, !True or !True)                   │
 │           ‾‾‾‾‾                                                            │
 └──────────────────────────────────────────────────────── tuple_bool.md:1:38 ┘

    The value's type, which does not have a method named `not`, is:

        [True, ..]


┌────────────────┐
│ MISSING METHOD ├─ This `not` method is being called on a value whose type ──┐
└┬───────────────┘  doesn't have that method.                                 │
 │                                                                            │
 │  … !True, !False, True and False, !True or !True)                          │
 │           ‾‾‾‾‾‾                                                           │
 └──────────────────────────────────────────────────────── tuple_bool.md:1:45 ┘

    The value's type, which does not have a method named `not`, is:

        [False, ..]


┌────────────────┐
│ MISSING METHOD ├─ This `not` method is being called on a value whose type ──┐
└┬───────────────┘  doesn't have that method.                                 │
 │                                                                            │
 │  … False, !True or !True)                                                  │
 │           ‾‾‾‾‾                                                            │
 └──────────────────────────────────────────────────────── tuple_bool.md:1:69 ┘

    The value's type, which does not have a method named `not`, is:

        [True, ..]


┌────────────────┐
│ MISSING METHOD ├─ This `not` method is being called on a value whose type ──┐
└┬───────────────┘  doesn't have that method.                                 │
 │                                                                            │
 │  …True or !True)                                                           │
 │           ‾‾‾‾‾                                                            │
 └──────────────────────────────────────────────────────── tuple_bool.md:1:78 ┘

    The value's type, which does not have a method named `not`, is:

        [True, ..]

# TOKENS
~~~zig
OpenRound,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,Comma,OpBang,UpperIdent,Comma,OpBang,UpperIdent,Comma,UpperIdent,OpAnd,UpperIdent,Comma,OpBang,UpperIdent,OpOr,OpBang,UpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple
	(e-tag (raw "True"))
	(e-tag (raw "False"))
	(e-tag (raw "Bool.True"))
	(e-tag (raw "Bool.False"))
	(unary "!"
		(e-tag (raw "True")))
	(unary "!"
		(e-tag (raw "False")))
	(e-binop (op "and")
		(e-tag (raw "True"))
		(e-tag (raw "False")))
	(e-binop (op "or")
		(unary "!"
			(e-tag (raw "True")))
		(unary "!"
			(e-tag (raw "True")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple
	(elems
		(e-tag (name "True"))
		(e-tag (name "False"))
		(e-nominal-external
			(builtin)
			(e-tag (name "True")))
		(e-nominal-external
			(builtin)
			(e-tag (name "False")))
		(e-dispatch-call (method "not") (constraint-fn-var 47)
			(receiver
				(e-tag (name "True")))
			(args))
		(e-dispatch-call (method "not") (constraint-fn-var 51)
			(receiver
				(e-tag (name "False")))
			(args))
		(e-if
			(if-branches
				(if-branch
					(e-tag (name "True"))
					(e-tag (name "False"))))
			(if-else
				(e-nominal-external
					(builtin)
					(e-tag (name "False")))))
		(e-if
			(if-branches
				(if-branch
					(e-dispatch-call (method "not") (constraint-fn-var 67)
						(receiver
							(e-tag (name "True")))
						(args))
					(e-nominal-external
						(builtin)
						(e-tag (name "True")))))
			(if-else
				(e-dispatch-call (method "not") (constraint-fn-var 79)
					(receiver
						(e-tag (name "True")))
					(args))))))
~~~
# TYPES
~~~clojure
(expr (type "([True, ..], [False, ..], Bool, Bool, Error, Error, Bool, Error)"))
~~~
