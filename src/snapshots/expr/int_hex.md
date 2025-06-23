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
(int (1:1-1:5) "0xFF")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_runtime_error (1:1-1:5) "invalid_num_literal")
~~~
# TYPES
~~~clojure
(expr 13 (type "Error"))
~~~