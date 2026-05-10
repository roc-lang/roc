# META
~~~ini
description=Match expression with list patterns including invalid rest pattern
type=expr
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
# PROBLEMS
**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

**list_patterns.md:3:13:3:19:**
```roc
    [first, ..rest] => 0 # invalid rest pattern should error
```
            ^^^^^^


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
