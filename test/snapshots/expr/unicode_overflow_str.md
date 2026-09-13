# META
~~~ini
description=Unicode overflow (should error)
type=expr
~~~
# SOURCE
~~~roc
"\u(FFFFFF)"
~~~
# EXPECTED
INVALID UNICODE ESCAPE SEQUENCE - unicode_overflow_str.md:1:2:1:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Unicode Escape Sequence")
		(region (start 1 2) (end 1 12))
		(headline
			(reflow "This Unicode escape sequence is not valid."))
		(document
			(source-region (file "unicode_overflow_str.md") (start 1 2) (end 1 12) (annotation error) (line-text "\"\\u(FFFFFF)\"")))))
~~~
# TOKENS
~~~zig
StringStart,MalformedStringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-string)
~~~
# FORMATTED
~~~roc
""
~~~
# CANONICALIZE
~~~clojure
(e-string)
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
