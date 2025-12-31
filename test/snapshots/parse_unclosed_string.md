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
NIL
# TOKENS
~~~zig
StringStart,StringPart,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-string
	(e-string-part (raw "hello\n")))
~~~
