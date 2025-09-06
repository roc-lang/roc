# META
~~~ini
description=Malformed record syntax using equals instead of colon (error case)
type=expr
~~~
# SOURCE
~~~roc
{ age: 42, name = "Alice" }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon Int Comma LowerIdent OpAssign String CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (binop_colon
    (lc "age")
    (num_literal_i32 42)
  )
  (binop_equals
    (lc "name")
    (str_literal_big "Alice")
  )
)
~~~
# FORMATTED
~~~roc
{ age: 42, name = "Alice" }
~~~
# EXPECTED
UNEXPECTED TOKEN IN TYPE ANNOTATION - error_malformed_syntax_2.md:1:8:1:10
UNEXPECTED TOKEN IN EXPRESSION - error_malformed_syntax_2.md:1:10:1:11
MALFORMED TYPE - error_malformed_syntax_2.md:1:8:1:10
UNRECOGNIZED SYNTAX - error_malformed_syntax_2.md:1:10:1:11
UNUSED VARIABLE - error_malformed_syntax_2.md:1:12:1:16
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **name** in this scope.
Is there an **import** or **exposing** missing up-top?

**error_malformed_syntax_2.md:1:12:1:16:**
```roc
{ age: 42, name = "Alice" }
```
           ^^^^


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (lc "age")
    (Expr.num_literal_i32 42)
  )
  (Expr.binop_equals
    (Expr.lookup "name")
    (Expr.str_literal_big)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
