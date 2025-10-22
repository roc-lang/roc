# META
~~~ini
description=Basic tag union match with simple patterns
type=expr
~~~
# SOURCE
~~~roc
match color {
	Red => 1
	Blue => 2
	Green => "3"
}
~~~
# EXPECTED
UNDEFINED VARIABLE - basic_tag_union.md:1:7:1:12
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `color` in this scope.
Is there an `import` or `exposing` missing up-top?

**basic_tag_union.md:1:7:1:12:**
```roc
match color {
```
      ^^^^^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "color"))
	(branches
		(branch
			(p-tag (raw "Red"))
			(e-int (raw "1")))
		(branch
			(p-tag (raw "Blue"))
			(e-int (raw "2")))
		(branch
			(p-tag (raw "Green"))
			(e-string
				(e-string-part (raw "3"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
					(e-string
						(e-literal (string "3"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
