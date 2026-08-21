# META
~~~ini
description=Unicode not hex (should error))
type=expr
~~~
# SOURCE
~~~roc
"abc\u(zzzz)def"
~~~
# EXPECTED
INVALID UNICODE ESCAPE SEQUENCE - unicode_not_hex.md:1:5:1:13
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Unicode Escape Sequence")
		(region (start 1 5) (end 1 13))
		(headline
			(reflow "This Unicode escape sequence is not valid."))
		(document
			(source-region (file "unicode_not_hex.md") (start 1 5) (end 1 13) (annotation error) (line-text "\"abc\\u(zzzz)def\"")))))
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
