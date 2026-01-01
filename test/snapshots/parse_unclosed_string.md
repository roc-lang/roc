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
**UNCLOSED STRING**
This string is missing a closing quote.

**parse_unclosed_string.md:1:1:1:7:**
```roc
"hello
```
^^^^^^


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
