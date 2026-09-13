# META
~~~ini
description=minimal reproduction of record parsing index out of bounds crash
type=expr
~~~
# SOURCE
~~~roc
{ i, Complete]
~~~
# EXPECTED
EXPECTED RECORD FIELD - fuzz_crash_033.md:1:6:1:14
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Record Field")
		(region (start 1 6) (end 1 14))
		(headline
			(reflow "I was parsing a record expression, and I expected a lowercase field name."))
		(document
			(reflow "Record fields start with lowercase names. After the name, either write ")
			(annotated code ": value")
			(reflow " or omit the value to use field punning.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ name: \"Ada\", age }")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "Complete")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_033.md") (start 1 6) (end 1 14) (annotation error) (line-text "{ i, Complete]")))))
~~~
# TOKENS
~~~zig
OpenCurly,LowerIdent,Comma,UpperIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "expected_expr_record_field_name"))
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
