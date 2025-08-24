# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]{
 
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenCurly ~~~
# PARSE
~~~clojure
(block
  (block)
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_035.md:1:9:1:10
# PROBLEMS
**Parse Error**
at 1:9 to 2:2

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.block
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
