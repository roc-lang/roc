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
  (Expr.record_field
    (Expr.malformed)
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
; Total type variables: 9
(var #0 _)
(var #1 _)
(var #2 Num *)
(var #3 _)
(var #4 _)
(var #5 Str)
(var #6 _)
(var #7 -> #8)
(var #8 {})
~~~
# TYPES
~~~roc
~~~
