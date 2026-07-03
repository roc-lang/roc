# META
~~~ini
description=Match expression with list rest patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match items {
    [first, ..rest] => first + 1
    [..rest, last] => last + 2
    [x, ..rest, y] => x + y
}
~~~
# EXPECTED
BAD LIST REST PATTERN SYNTAX - list_rest_scoping.md:2:13:2:19
BAD LIST REST PATTERN SYNTAX - list_rest_scoping.md:3:6:3:12
BAD LIST REST PATTERN SYNTAX - list_rest_scoping.md:4:9:4:15
# PROBLEMS

┌──────────────────────────────┐
│ BAD LIST REST PATTERN SYNTAX ├─ List rest patterns should use the `.. as ───┐
└┬─────────────────────────────┘  name` syntax, not `..name`.                 │
 │                                                                            │
 │  [first, ..rest] => first + 1                                              │
 │          ‾‾‾‾‾‾                                                            │
 └───────────────────────────────────────────────── list_rest_scoping.md:2:13 ┘

    For example, use `[first, .. as rest]` instead of `[first, ..rest]`.


┌──────────────────────────────┐
│ BAD LIST REST PATTERN SYNTAX ├─ List rest patterns should use the `.. as ───┐
└┬─────────────────────────────┘  name` syntax, not `..name`.                 │
 │                                                                            │
 │  [..rest, last] => last + 2                                                │
 │   ‾‾‾‾‾‾                                                                   │
 └────────────────────────────────────────────────── list_rest_scoping.md:3:6 ┘

    For example, use `[first, .. as rest]` instead of `[first, ..rest]`.


┌──────────────────────────────┐
│ BAD LIST REST PATTERN SYNTAX ├─ List rest patterns should use the `.. as ───┐
└┬─────────────────────────────┘  name` syntax, not `..name`.                 │
 │                                                                            │
 │  [x, ..rest, y] => x + y                                                   │
 │      ‾‾‾‾‾‾                                                                │
 └────────────────────────────────────────────────── list_rest_scoping.md:4:9 ┘

    For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,Comma,DoubleDot,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,Int,
OpenSquare,DoubleDot,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,Int,
OpenSquare,LowerIdent,Comma,DoubleDot,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "items"))
	(branches
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-list-rest (name "rest")))
			(e-binop (op "+")
				(e-ident (raw "first"))
				(e-int (raw "1"))))
		(branch
			(p-list
				(p-list-rest (name "rest"))
				(p-ident (raw "last")))
			(e-binop (op "+")
				(e-ident (raw "last"))
				(e-int (raw "2"))))
		(branch
			(p-list
				(p-ident (raw "x"))
				(p-list-rest (name "rest"))
				(p-ident (raw "y")))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-ident (raw "y"))))))
~~~
# FORMATTED
~~~roc
match items {
	[first, .. as rest] => first + 1
	[.. as rest, last] => last + 2
	[x, .. as rest, y] => x + y
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
								(p-assign (ident "first")))
							(rest-at (index 1)
								(p-assign (ident "rest"))))))
				(value
					(e-dispatch-call (method "plus") (constraint-fn-var 72)
						(receiver
							(e-lookup-local
								(p-assign (ident "first"))))
						(args
							(e-num (value "1"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "last")))
							(rest-at (index 0)
								(p-assign (ident "rest"))))))
				(value
					(e-dispatch-call (method "plus") (constraint-fn-var 109)
						(receiver
							(e-lookup-local
								(p-assign (ident "last"))))
						(args
							(e-num (value "2"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "x"))
								(p-assign (ident "y")))
							(rest-at (index 1)
								(p-assign (ident "rest"))))))
				(value
					(e-dispatch-call (method "plus") (constraint-fn-var 113)
						(receiver
							(e-lookup-local
								(p-assign (ident "x"))))
						(args
							(e-lookup-local
								(p-assign (ident "y"))))))))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
