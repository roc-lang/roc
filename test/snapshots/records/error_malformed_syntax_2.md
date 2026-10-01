# META
~~~ini
description=Malformed record syntax using equals instead of colon (error case)
type=expr
~~~
# SOURCE
~~~roc
{ age: 42, name = "Alice" }
~~~
# EXPECTED
UNEXPECTED TYPE SYNTAX - error_malformed_syntax_2.md:1:8:1:10
UNEXPECTED EXPRESSION SYNTAX - error_malformed_syntax_2.md:1:10:1:11
DECLARATION HAS NO VALUE - error_malformed_syntax_2.md:1:3:1:10
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Record Field Uses Assignment")
		(region (start 1 17) (end 1 18))
		(headline
			(reflow "I found `=` where a record field needs `:`."))
		(document
			(reflow "Use a colon between a record field name and its value. Run ")
			(annotated code "roc fmt")
			(reflow " to correct this separator.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ name: \"Ada\", age: 36 }")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "error_malformed_syntax_2.md") (start 1 17) (end 1 18) (annotation error) (line-text "{ age: 42, name = \"Alice\" }")))))
~~~
# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpAssign,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "age")
		(e-int (raw "42")))
	(field (field "name")
		(e-string
			(e-string-part (raw "Alice")))))
~~~
# FORMATTED
~~~roc
{ age: 42, name: "Alice" }
~~~
# CANONICALIZE
~~~clojure
(e-record
	(fields
		(field (name "age")
			(e-num (value "42")))
		(field (name "name")
			(e-string
				(e-literal (string "Alice"))))))
~~~
# TYPES
~~~clojure
(expr (type "{ age: Dec, name: Str }"))
~~~
