# META
~~~ini
description=Invalid integer literal that exceeds i128 range
type=expr
~~~
# SOURCE
~~~roc
99999999999999999999999999999999999999999
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**INVALID NUMBER**
This number literal is not valid: 99999999999999999999999999999999999999999

# TOKENS
~~~zig
Int(1:1-1:42),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.42 (raw "99999999999999999999999999999999999999999"))
~~~
# FORMATTED
~~~roc
99999999999999999999999999999999999999999
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "invalid_num_literal"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.42 (type "Error"))
~~~
