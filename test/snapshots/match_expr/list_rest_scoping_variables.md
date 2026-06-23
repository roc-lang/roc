# META
~~~ini
description=List rest patterns with proper variable scoping across branches
type=expr
~~~
# SOURCE
~~~roc
match data {
    [..items] => 1
    [first, ..items] => first
    [..items, last] => last
    [first, ..items, last] => first + last
}
~~~
# EXPECTED
BAD LIST REST PATTERN SYNTAX - list_rest_scoping_variables.md:2:6:2:13
BAD LIST REST PATTERN SYNTAX - list_rest_scoping_variables.md:3:13:3:20
BAD LIST REST PATTERN SYNTAX - list_rest_scoping_variables.md:4:6:4:13
BAD LIST REST PATTERN SYNTAX - list_rest_scoping_variables.md:5:13:5:20
# PROBLEMS

┌──────────────────────────────┐
│ BAD LIST REST PATTERN SYNTAX ├─ List rest patterns should use the `.. as ───┐
└┬─────────────────────────────┘  name` syntax, not `..name`.                 │
 │                                                                            │
 │  [..items] => 1                                                            │
 │   ‾‾‾‾‾‾‾                                                                  │
 └──────────────────────────────────────── list_rest_scoping_variables.md:2:6 ┘

    For example, use `[first, .. as rest]` instead of `[first, ..rest]`.


┌──────────────────────────────┐
│ BAD LIST REST PATTERN SYNTAX ├─ List rest patterns should use the `.. as ───┐
└┬─────────────────────────────┘  name` syntax, not `..name`.                 │
 │                                                                            │
 │  [first, ..items] => first                                                 │
 │          ‾‾‾‾‾‾‾                                                           │
 └─────────────────────────────────────── list_rest_scoping_variables.md:3:13 ┘

    For example, use `[first, .. as rest]` instead of `[first, ..rest]`.


┌──────────────────────────────┐
│ BAD LIST REST PATTERN SYNTAX ├─ List rest patterns should use the `.. as ───┐
└┬─────────────────────────────┘  name` syntax, not `..name`.                 │
 │                                                                            │
 │  [..items, last] => last                                                   │
 │   ‾‾‾‾‾‾‾                                                                  │
 └──────────────────────────────────────── list_rest_scoping_variables.md:4:6 ┘

    For example, use `[first, .. as rest]` instead of `[first, ..rest]`.


┌──────────────────────────────┐
│ BAD LIST REST PATTERN SYNTAX ├─ List rest patterns should use the `.. as ───┐
└┬─────────────────────────────┘  name` syntax, not `..name`.                 │
 │                                                                            │
 │  [first, ..items, last] => first + last                                    │
 │          ‾‾‾‾‾‾‾                                                           │
 └─────────────────────────────────────── list_rest_scoping_variables.md:5:13 ┘

    For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,DoubleDot,LowerIdent,CloseSquare,OpFatArrow,Int,
OpenSquare,LowerIdent,Comma,DoubleDot,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,DoubleDot,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,LowerIdent,Comma,DoubleDot,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "data"))
	(branches
		(branch
			(p-list
				(p-list-rest (name "items")))
			(e-int (raw "1")))
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-list-rest (name "items")))
			(e-ident (raw "first")))
		(branch
			(p-list
				(p-list-rest (name "items"))
				(p-ident (raw "last")))
			(e-ident (raw "last")))
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-list-rest (name "items"))
				(p-ident (raw "last")))
			(e-binop (op "+")
				(e-ident (raw "first"))
				(e-ident (raw "last"))))))
~~~
# FORMATTED
~~~roc
match data {
	[.. as items] => 1
	[first, .. as items] => first
	[.. as items, last] => last
	[first, .. as items, last] => first + last
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
							(patterns)
							(rest-at (index 0)
								(p-assign (ident "items"))))))
				(value
					(e-num (value "1"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "first")))
							(rest-at (index 1)
								(p-assign (ident "items"))))))
				(value
					(e-lookup-local
						(p-assign (ident "first")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "last")))
							(rest-at (index 0)
								(p-assign (ident "items"))))))
				(value
					(e-lookup-local
						(p-assign (ident "last")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "first"))
								(p-assign (ident "last")))
							(rest-at (index 1)
								(p-assign (ident "items"))))))
				(value
					(e-dispatch-call (method "plus") (constraint-fn-var 81)
						(receiver
							(e-lookup-local
								(p-assign (ident "first"))))
						(args
							(e-lookup-local
								(p-assign (ident "last"))))))))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
