# META
~~~ini
description=Match expression with tag patterns and variable catch-all pattern
type=expr
~~~
# SOURCE
~~~roc
match value {
    Answer => "the answer"
    Zero => "zero"
    other => "something else"
}
~~~
# EXPECTED
UNDEFINED VARIABLE - wildcard_patterns.md:1:7:1:12
UNUSED VARIABLE - wildcard_patterns.md:4:5:4:10
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `value` in this scope.
Is there an `import` or `exposing` missing up-top?

**wildcard_patterns.md:1:7:1:12:**
```roc
match value {
```
      ^^^^^


**UNUSED VARIABLE**
Variable `other` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_other` to suppress this warning.
The unused variable is declared here:
**wildcard_patterns.md:4:5:4:10:**
```roc
    other => "something else"
```
    ^^^^^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
LowerIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "value"))
	(branches
		(branch
			(p-tag (raw "Answer"))
			(e-string
				(e-string-part (raw "the answer"))))
		(branch
			(p-tag (raw "Zero"))
			(e-string
				(e-string-part (raw "zero"))))
		(branch
			(p-ident (raw "other"))
			(e-string
				(e-string-part (raw "something else"))))))
~~~
# FORMATTED
~~~roc
match value {
	Answer => "the answer"
	Zero => "zero"
	other => "something else"
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
					(e-string
						(e-literal (string "the answer")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-string
						(e-literal (string "zero")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-assign (ident "other"))))
				(value
					(e-string
						(e-literal (string "something else"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
