# META
~~~ini
description=Match expression with single branch (simple variable pattern)
type=expr
~~~
# SOURCE
~~~roc
match value {
    x => x + 1
}
~~~
# EXPECTED
UNDEFINED VARIABLE - single_branch.md:1:7:1:12
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `value` in this scope.
Is there an `import` or `exposing` missing up-top?

**single_branch.md:1:7:1:12:**
```roc
match value {
```
      ^^^^^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
LowerIdent,OpFatArrow,LowerIdent,OpPlus,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "value"))
	(branches
		(branch
			(p-ident (raw "x"))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-int (raw "1"))))))
~~~
# FORMATTED
~~~roc
match value {
	x => x + 1
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
						(p-assign (ident "x"))))
				(value
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-num (value "1"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
