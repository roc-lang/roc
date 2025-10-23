# META
~~~ini
description=Match expression with boolean-like tag patterns
type=expr
~~~
# SOURCE
~~~roc
match isReady {
	True => "ready to go!"
	False => "not ready yet"
}
~~~
# EXPECTED
UNDEFINED VARIABLE - boolean_patterns.md:1:7:1:14
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `isReady` in this scope.
Is there an `import` or `exposing` missing up-top?

**boolean_patterns.md:1:7:1:14:**
```roc
match isReady {
```
      ^^^^^^^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "isReady"))
	(branches
		(branch
			(p-tag (raw "True"))
			(e-string
				(e-string-part (raw "ready to go!"))))
		(branch
			(p-tag (raw "False"))
			(e-string
				(e-string-part (raw "not ready yet"))))))
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
					(e-string
						(e-literal (string "ready to go!")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-string
						(e-literal (string "not ready yet"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
