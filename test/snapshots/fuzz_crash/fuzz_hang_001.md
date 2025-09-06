# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0 (
~~~
# TOKENS
~~~text
Int OpenRound ~~~
# PARSE
~~~clojure
(block
  (apply_anon
    (num_literal_i32 0)
  )
)
~~~
# FORMATTED
~~~roc
0()
~~~
# EXPECTED
MISSING HEADER - fuzz_hang_001.md:1:1:1:2
PARSE ERROR - fuzz_hang_001.md:1:3:1:4
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**fuzz_hang_001.md:1:2:1:3:**
```roc
0 (
```
 ^


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**fuzz_hang_001.md:1:1:1:4:**
```roc
0 (
```
^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.fn_call)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 5
(var #0 _)
(var #1 -> #4)
(var #2 _)
(var #3 _)
(var #4 <error>)
~~~
# TYPES
~~~roc
~~~
