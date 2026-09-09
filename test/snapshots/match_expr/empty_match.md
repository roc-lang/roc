# META
~~~ini
description=Match expression with no branches should produce error
type=expr
~~~
# SOURCE
~~~roc
match 42 {}
~~~
# EXPECTED
EMPTY MATCH - empty_match.md:1:1:1:6
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Empty Match")
		(region (start 1 1) (end 1 6))
		(headline
			(reflow "I was parsing a match expression, but it has no branches."))
		(document
			(reflow "A match expression needs at least one branch inside the braces.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "match result {")
			(line-break)
			(indent 1)
			(text "    Ok(value) => value")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "match")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "empty_match.md") (start 1 1) (end 1 6) (annotation error) (line-text "match 42 {}")))))
~~~
# TOKENS
~~~zig
KwMatch,Int,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "match_has_no_branches"))
~~~
# FORMATTED
~~~roc

~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
