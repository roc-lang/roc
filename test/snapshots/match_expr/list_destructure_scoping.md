# META
~~~ini
description=Match expression with various list destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match list {
    [first] => first
    [first, second] => first + second
}
~~~
# EXPECTED
POLYMORPHIC VALUE - list_destructure_scoping.md:1:1:4:2
# PROBLEMS

┌───────────────────┐
│ POLYMORPHIC VALUE ├─ This top-level value still has an unresolved ──────────┐
└┬──────────────────┘  polymorphic type.                                      │
 │                                                                            │
 │  match list {                                                              │
 │      [first] => first                                                      │
 │      [first, second] => first + second                                     │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────────────────────── list_destructure_scoping.md:1:1 ┘

    Its type is:
    a where [a.plus : a, a -> a]
    Add an annotation or use this value in a way that fixes its concrete type.

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "list"))
	(branches
		(branch
			(p-list
				(p-ident (raw "first")))
			(e-ident (raw "first")))
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-ident (raw "second")))
			(e-binop (op "+")
				(e-ident (raw "first"))
				(e-ident (raw "second"))))))
~~~
# FORMATTED
~~~roc
match list {
	[first] => first
	[first, second] => first + second
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
						(p-list
							(patterns
								(p-assign (ident "first"))))))
				(value
					(e-lookup-local
						(p-assign (ident "first")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "first"))
								(p-assign (ident "second"))))))
				(value
					(e-dispatch-call (method "plus") (constraint-fn-var 26)
						(receiver
							(e-lookup-local
								(p-assign (ident "first"))))
						(args
							(e-lookup-local
								(p-assign (ident "second"))))))))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.plus : a, a -> a]"))
~~~
