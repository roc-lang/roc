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
  (lc "email")
  (binop_colon
    (lc "status")
    (str_literal_big "active")
  )
  (lc "balance")
)
~~~
# FORMATTED
~~~roc
{ name, age : 30, email, status : "active", balance }
~~~
# EXPECTED
NIL
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
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.num_literal_i32 30)
  )
  (Expr.lookup "email")
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.str_literal_big)
  )
  (Expr.lookup "balance")
)
~~~
# SOLVED
~~~clojure
(expr :tag record_literal :type "{ name: _field, age: Num(_size), email: _field2, status: Str, balance: _field3 }")
~~~
# TYPES
~~~roc
{ name: _field, age: Num(_size), email: _field2, status: Str, balance: _field3 }
~~~
