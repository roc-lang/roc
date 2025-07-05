# META
~~~ini
description=single_no_end fail
type=expr
~~~
# SOURCE
~~~roc
"there is no end
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:17),StringEnd(1:17-1:17),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.17
	(e-string-part @1.2-1.17 (raw "there is no end")))
~~~
# FORMATTED
~~~roc
"there is no end"
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-1.17
	(e-literal @1.2-1.17 (string "there is no end")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.17 (type "Str"))
~~~
