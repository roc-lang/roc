# META
~~~ini
description=Record construction using mixed shorthand and explicit record fields
type=expr
~~~
# SOURCE
~~~roc
{ name, age: 30, email, status: "active", balance }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent Comma LowerIdent OpColon Int Comma LowerIdent Comma LowerIdent OpColon String Comma LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (lc "name")
  (binop_colon
    (lc "age")
    (num_literal_i32 30)
  )
  (binop_colon
    (lc "email")
    (lc "email")
  )
  (binop_colon
    (lc "status")
    (str_literal_big "active")
  )
  (binop_colon
    (lc "balance")
    (lc "balance")
  )
)
~~~
# FORMATTED
~~~roc
{ name, age: 30, email: email, status: "active", balance: balance }
~~~
# EXPECTED
UNDEFINED VARIABLE - record_mixed_field_syntax.md:1:3:1:7
UNDEFINED VARIABLE - record_mixed_field_syntax.md:1:18:1:23
UNDEFINED VARIABLE - record_mixed_field_syntax.md:1:43:1:50
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **name** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_mixed_field_syntax.md:1:3:1:7:**
```roc
{ name, age: 30, email, status: "active", balance }
```
  ^^^^


**UNDEFINED VARIABLE**
Nothing is named **email** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_mixed_field_syntax.md:1:18:1:23:**
```roc
{ name, age: 30, email, status: "active", balance }
```
                 ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **balance** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_mixed_field_syntax.md:1:43:1:50:**
```roc
{ name, age: 30, email, status: "active", balance }
```
                                          ^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.lookup "name")
  (Expr.record_field
    (Expr.malformed)
    (Expr.num_literal_i32 30)
  )
  (Expr.record_field
    (Expr.lookup "email")
    (Expr.lookup "email")
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.str_literal_big)
  )
  (Expr.record_field
    (Expr.lookup "balance")
    (Expr.lookup "balance")
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 15
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 Num *)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 Str)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 -> #14)
(var #13 {})
(var #14 record)
~~~
# TYPES
~~~roc
~~~
