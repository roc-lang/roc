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
NIL
# PROBLEMS
NIL
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
