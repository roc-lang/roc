# META
~~~ini
description=Comprehensive test for match branch scoping with variable isolation
type=expr
~~~
# SOURCE
~~~roc
match result {
    Ok(value) => value + 1
    Err(value) => value - 1
    Ok(different) => different * 2
    Err(different) => different / 2
}
~~~
# EXPECTED
POLYMORPHIC VALUE - branch_scoping.md:1:1:6:2
# PROBLEMS

┌───────────────────┐
│ POLYMORPHIC VALUE ├─ This top-level value still has an unresolved ──────────┐
└┬──────────────────┘  polymorphic type.                                      │
 │                                                                            │
 │  match result {                                                            │
 │      Ok(value) => value + 1                                                │
 │      Err(value) => value - 1                                               │
 │      Ok(different) => different * 2                                        │
 │      Err(different) => different / 2                                       │
 │  }                                                                         │
 │                                                                            │
 └───────────────────────────────────────────────────── branch_scoping.md:1:1 ┘

    Its type is:
    a
      where [
        a.div_by : a, Dec -> a,
        a.minus : a, Dec -> a,
        a.plus : a, Dec -> a,
        a.times : a, Dec -> a,
      ]
    Add an annotation or use this value in a way that fixes its concrete type.

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpPlus,Int,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpBinaryMinus,Int,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpStar,Int,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpSlash,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "result"))
	(branches
		(branch
			(p-tag (raw "Ok")
				(p-ident (raw "value")))
			(e-binop (op "+")
				(e-ident (raw "value"))
				(e-int (raw "1"))))
		(branch
			(p-tag (raw "Err")
				(p-ident (raw "value")))
			(e-binop (op "-")
				(e-ident (raw "value"))
				(e-int (raw "1"))))
		(branch
			(p-tag (raw "Ok")
				(p-ident (raw "different")))
			(e-binop (op "*")
				(e-ident (raw "different"))
				(e-int (raw "2"))))
		(branch
			(p-tag (raw "Err")
				(p-ident (raw "different")))
			(e-binop (op "/")
				(e-ident (raw "different"))
				(e-int (raw "2"))))))
~~~
# FORMATTED
~~~roc
match result {
	Ok(value) => value + 1
	Err(value) => value - 1
	Ok(different) => different * 2
	Err(different) => different / 2
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-dispatch-call (method "plus") (constraint-fn-var 71)
						(receiver
							(e-lookup-local
								(p-assign (ident "value"))))
						(args
							(e-num (value "1"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-dispatch-call (method "minus") (constraint-fn-var 107)
						(receiver
							(e-lookup-local
								(p-assign (ident "value"))))
						(args
							(e-num (value "1"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-dispatch-call (method "times") (constraint-fn-var 143)
						(receiver
							(e-lookup-local
								(p-assign (ident "different"))))
						(args
							(e-num (value "2"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-dispatch-call (method "div_by") (constraint-fn-var 179)
						(receiver
							(e-lookup-local
								(p-assign (ident "different"))))
						(args
							(e-num (value "2")))))))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.div_by : a, Dec -> a, a.minus : a, Dec -> a, a.plus : a, Dec -> a, a.times : a, Dec -> a]"))
~~~
