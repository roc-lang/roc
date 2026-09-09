# META
~~~ini
description=Weird escape (should error)
type=expr
~~~
# SOURCE
~~~roc
"abc\qdef"
~~~
# EXPECTED
INVALID ESCAPE SEQUENCE - weird_escape.md:1:5:1:7
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Escape Sequence")
		(region (start 1 5) (end 1 7))
		(headline
			(reflow "This escape sequence is not recognized."))
		(document
			(source-region (file "weird_escape.md") (start 1 5) (end 1 7) (annotation error) (line-text "\"abc\\qdef\"")))))
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
