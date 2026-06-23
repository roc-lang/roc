# META
~~~ini
description=Match expression with mixed tag and list patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match data {
    Ok([x, y]) => x + y
    Err(x) => x - 1
    Ok([x]) => x * 2
    Err(y) => y / 2
}
~~~
# EXPECTED
POLYMORPHIC VALUE - mixed_pattern_scoping.md:1:1:6:2
# PROBLEMS

┌───────────────────┐
│ POLYMORPHIC VALUE ├─ This top-level value still has an unresolved ──────────┐
└┬──────────────────┘  polymorphic type.                                      │
 │                                                                            │
 │  match data {                                                              │
 │      Ok([x, y]) => x + y                                                   │
 │      Err(x) => x - 1                                                       │
 │      Ok([x]) => x * 2                                                      │
 │      Err(y) => y / 2                                                       │
 │  }                                                                         │
 │                                                                            │
 └────────────────────────────────────────────── mixed_pattern_scoping.md:1:1 ┘

    Its type is:
    a
      where [
        a.div_by : a, Dec -> a,
        a.minus : a, Dec -> a,
        a.plus : a, a -> a,
        a.times : a, Dec -> a,
      ]
    Add an annotation or use this value in a way that fixes its concrete type.

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,CloseRound,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpBinaryMinus,Int,
UpperIdent,NoSpaceOpenRound,OpenSquare,LowerIdent,CloseSquare,CloseRound,OpFatArrow,LowerIdent,OpStar,Int,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpSlash,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "data"))
	(branches
		(branch
			(p-tag (raw "Ok")
				(p-list
					(p-ident (raw "x"))
					(p-ident (raw "y"))))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-ident (raw "y"))))
		(branch
			(p-tag (raw "Err")
				(p-ident (raw "x")))
			(e-binop (op "-")
				(e-ident (raw "x"))
				(e-int (raw "1"))))
		(branch
			(p-tag (raw "Ok")
				(p-list
					(p-ident (raw "x"))))
			(e-binop (op "*")
				(e-ident (raw "x"))
				(e-int (raw "2"))))
		(branch
			(p-tag (raw "Err")
				(p-ident (raw "y")))
			(e-binop (op "/")
				(e-ident (raw "y"))
				(e-int (raw "2"))))))
~~~
# FORMATTED
~~~roc
match data {
	Ok([x, y]) => x + y
	Err(x) => x - 1
	Ok([x]) => x * 2
	Err(y) => y / 2
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
					(e-dispatch-call (method "plus") (constraint-fn-var 43)
						(receiver
							(e-lookup-local
								(p-assign (ident "x"))))
						(args
							(e-lookup-local
								(p-assign (ident "y")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-dispatch-call (method "minus") (constraint-fn-var 79)
						(receiver
							(e-lookup-local
								(p-assign (ident "x"))))
						(args
							(e-num (value "1"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-dispatch-call (method "times") (constraint-fn-var 117)
						(receiver
							(e-lookup-local
								(p-assign (ident "x"))))
						(args
							(e-num (value "2"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-dispatch-call (method "div_by") (constraint-fn-var 153)
						(receiver
							(e-lookup-local
								(p-assign (ident "y"))))
						(args
							(e-num (value "2")))))))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.div_by : a, Dec -> a, a.minus : a, Dec -> a, a.plus : a, a -> a, a.times : a, Dec -> a]"))
~~~
