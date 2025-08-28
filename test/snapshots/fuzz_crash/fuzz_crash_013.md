# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0{
~~~
# TOKENS
~~~text
Int OpenCurly ~~~
# PARSE
~~~clojure
(block
  (num_literal_i32 0)
  (block)
)
~~~
# FORMATTED
~~~roc
0{
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:2 to 1:3

# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
