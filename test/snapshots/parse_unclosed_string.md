# META
~~~ini
description=Unclosed string literal
type=expr
~~~
# SOURCE
~~~roc
"hello
~~~
# EXPECTED
UNCLOSED STRING - parse_unclosed_string.md:1:1:1:7
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unclosed String")
		(region (start 1 1) (end 1 7))
		(headline
			(reflow "This string is missing a closing quote."))
		(document
			(source-region (file "parse_unclosed_string.md") (start 1 1) (end 1 7) (annotation error) (line-text "\"hello")))))
~~~
# TOKENS
~~~zig
StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-string
	(e-string-part (raw "hello")))
~~~
# FORMATTED
~~~roc
"hello"
~~~
# CANONICALIZE
~~~clojure
(e-string
	(e-literal (string "hello")))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
