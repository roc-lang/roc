# META
~~~ini
description=Backslash is not valid lambda syntax - helpful error
type=expr
~~~
# SOURCE
~~~roc
\x -> x + 1
~~~
# TOKENS
~~~text
OpBackslash LowerIdent OpArrow LowerIdent OpPlus Int ~~~
# PARSE
~~~clojure
(binop_thin_arrow
  (malformed malformed:backslash_not_valid_lambda_syntax)
  (binop_plus
    (lc "x")
    (num_literal_i32 1)
  )
)
~~~
# FORMATTED
~~~roc
x -> x + 1
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **backslash_not_valid_lambda_syntax**
This is an unexpected parsing error. Please check your syntax.

**parse_error_backslash_lambda.md:1:2:1:4:**
```roc
\x -> x + 1
```
 ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**parse_error_backslash_lambda.md:1:2:1:12:**
```roc
\x -> x + 1
```
 ^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~
