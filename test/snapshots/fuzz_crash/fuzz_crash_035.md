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
(module-header)
(block
  (block)
)
~~~
# FORMATTED
~~~roc
module []

{}
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_035.md:1:9:1:10
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_035.md:1:9:2:2:**
```roc
module[]{
 
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.record_literal
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 4
(var #0 _)
(var #1 -> #3)
(var #2 _)
(var #3 {})
~~~
# TYPES
~~~roc
~~~
