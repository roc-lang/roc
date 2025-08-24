# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]import
S
0
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwImport UpperIdent Int ~~~
# PARSE
~~~clojure
(block
  (import
    (uc "S")
  )
  (num_literal_i32 0)
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_052.md:3:1:3:2
MODULE NOT FOUND - fuzz_crash_052.md:1:9:2:2
# PROBLEMS
**Unsupported Node**
at 1:9 to 2:2

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.num_literal_i32 0)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Num(_size)")
~~~
# TYPES
~~~roc
~~~
