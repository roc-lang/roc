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
0
{}
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_013.md:1:1:1:2
PARSE ERROR - fuzz_crash_013.md:1:2:1:3
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_013.md:1:2:1:3:**
```roc
0{
```
 ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.num_literal_i32 0)
  (Expr.record_literal
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 5
(var #0 _)
(var #1 Num *)
(var #2 -> #4)
(var #3 _)
(var #4 {})
~~~
# TYPES
~~~roc
~~~
