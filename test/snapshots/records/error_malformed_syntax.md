# META
~~~ini
description=Malformed record syntax (error case)
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
~~~
# EXPECTED
EXPECTED RECORD FIELD - error_malformed_syntax.md:1:18:1:19
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Record Field")
		(region (start 1 18) (end 1 19))
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
			(annotated code ":")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "error_malformed_syntax.md") (start 1 18) (end 1 19) (annotation error) (line-text "{ name: \"Alice\", : 30, , email: , active Bool.true, \"invalid\": value, 42: \"number key\", : }")))))
~~~
# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,OpColon,Int,Comma,Comma,LowerIdent,OpColon,Comma,LowerIdent,UpperIdent,NoSpaceDotLowerIdent,Comma,StringStart,StringPart,StringEnd,OpColon,LowerIdent,Comma,Int,OpColon,StringStart,StringPart,StringEnd,Comma,OpColon,CloseCurly,
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
