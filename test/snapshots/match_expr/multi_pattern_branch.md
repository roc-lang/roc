# META
~~~ini
description=Match expression with multiple patterns in one branch
type=expr
~~~
# SOURCE
~~~roc
match color {
    Blue | Green | Red => 1
    Black => 2
    White => 3
}
~~~
# EXPECTED
UNDEFINED VARIABLE - multi_pattern_branch.md:1:7:1:12
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `color` in this scope.
Is there an `import` or `exposing` missing up-top?

**multi_pattern_branch.md:1:7:1:12:**
```roc
match color {
```
      ^^^^^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpBar,UpperIdent,OpBar,UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "color"))
	(branches
		(branch
			(p-alternatives
				(p-tag (raw "Blue"))
				(p-tag (raw "Green"))
				(p-tag (raw "Red")))
			(e-int (raw "1")))
		(branch
			(p-tag (raw "Black"))
			(e-int (raw "2")))
		(branch
			(p-tag (raw "White"))
			(e-int (raw "3")))))
~~~
# FORMATTED
~~~roc
match color {
	Blue | Green | Red => 1
	Black => 2
	White => 3
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
						(p-applied-tag))
					(pattern (degenerate false)
						(p-applied-tag))
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-num (value "1"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-num (value "2"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-num (value "3")))))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
