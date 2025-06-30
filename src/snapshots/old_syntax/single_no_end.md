# META
~~~ini
description=single_no_end fail
type=expr
~~~
# SOURCE
~~~roc
"there is no end
~~~
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:17),EndOfFile(1:17-1:17),
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
(e-string @1.1-1.17 (id 74)
	(e-literal @1.2-1.17 (string "there is no end")))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Str"))
~~~
