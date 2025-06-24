# META
~~~ini
description=Hexadecimal integer literal
type=expr
~~~
# SOURCE
~~~roc
0xFF
~~~
# PROBLEMS
**INVALID NUMBER**
This number literal is not valid: 0xFF

# TOKENS
~~~zig
Int(1:1-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(e-int @1-1-1-5 (raw "0xFF"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "invalid_num_literal") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Error"))
~~~