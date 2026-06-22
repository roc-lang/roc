# META
~~~ini
description=Match expression with list patterns including invalid rest pattern
type=expr
canonicalize_diagnostics=true
~~~
# SOURCE
~~~roc
match numbers {
    [] => acc
    [first, ..rest] => 0 # invalid rest pattern should error
}
~~~
# EXPECTED
BAD LIST REST PATTERN SYNTAX - list_patterns.md:3:13:3:19
UNDEFINED VARIABLE - list_patterns.md:1:7:1:14
UNDEFINED VARIABLE - list_patterns.md:2:11:2:14
UNUSED VARIABLE - list_patterns.md:3:6:3:11
UNUSED VARIABLE - list_patterns.md:3:15:3:15
# PROBLEMS
                                                ┌──────────────────────────────┐
┌─ List rest patterns should use the `.. as ────┤ BAD LIST REST PATTERN SYNTAX │
│  name` syntax, not `..name`.                  └─────────────────────────────┬┘
│                                                                             │
│      [first, ..rest] => 0 # invalid rest pattern should error               │
│              ‾‾‾‾‾‾                                                         │
└────────────────────────────────────────────────────── list_patterns.md:3:13 ┘

    For example, use `[first, .. as rest]` instead of `[first, ..rest]`.
                                                          ┌────────────────────┐
┌─ Nothing is named numbers in this scope. ───────────────┤ UNDEFINED VARIABLE │
│                                                         └───────────────────┬┘
│                                                                             │
│  match numbers {                                                            │
│        ‾‾‾‾‾‾‾                                                              │
└─────────────────────────────────────────────────────── list_patterns.md:1:7 ┘

    Is there an import or exposing missing up-top?
                                                          ┌────────────────────┐
┌─ Nothing is named acc in this scope. ───────────────────┤ UNDEFINED VARIABLE │
│                                                         └───────────────────┬┘
│                                                                             │
│      [] => acc                                                              │
│            ‾‾‾                                                              │
└────────────────────────────────────────────────────── list_patterns.md:2:11 ┘

    Is there an import or exposing missing up-top?
                                                             ┌─────────────────┐
┌─ Variable first is not used anywhere in your code. ────────┤ UNUSED VARIABLE │
│                                                            └────────────────┬┘
│                                                                             │
│      [first, ..rest] => 0 # invalid rest pattern should error               │
│       ‾‾‾‾‾                                                                 │
└─────────────────────────────────────────────────────── list_patterns.md:3:6 ┘

    If you don't need this variable, prefix it with an underscore like _first to suppress this warning.
    The unused variable is declared here:
                                                             ┌─────────────────┐
┌─ Variable rest is not used anywhere in your code. ─────────┤ UNUSED VARIABLE │
│                                                            └────────────────┬┘
│                                                                             │
│      [first, ..rest] => 0 # invalid rest pattern should error               │
│                ‾                                                            │
└────────────────────────────────────────────────────── list_patterns.md:3:15 ┘

    If you don't need this variable, prefix it with an underscore like _rest to suppress this warning.
    The unused variable is declared here:
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,LowerIdent,Comma,DoubleDot,LowerIdent,CloseSquare,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "numbers"))
	(branches
		(branch
			(p-list)
			(e-ident (raw "acc")))
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-list-rest (name "rest")))
			(e-int (raw "0")))))
~~~
# FORMATTED
~~~roc
match numbers {
	[] => acc
	[first, .. as rest] => 0
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
							(patterns))))
				(value
					(e-runtime-error (tag "ident_not_in_scope"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "first")))
							(rest-at (index 1)
								(p-assign (ident "rest"))))))
				(value
					(e-num (value "0")))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
