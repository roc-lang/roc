# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
F
~~~
# EXPECTED
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_004.md:2:1:2:1
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 2 1) (end 2 1))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_004.md") (start 2 1) (end 2 1) (annotation error) (line-text "")))))
~~~
# TOKENS
~~~zig
UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "expected_colon_after_type_annotation"))))
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
