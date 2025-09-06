# META
~~~ini
description=Record construction using shorthand field syntax
type=expr
~~~
# SOURCE
~~~roc
{ name, age, email, active }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (lc "name")
  (lc "age")
  (lc "email")
  (lc "active")
)
~~~
# FORMATTED
~~~roc
{ name, age, email, active }
~~~
# EXPECTED
UNDEFINED VARIABLE - record_shorthand_fields.md:1:3:1:7
UNDEFINED VARIABLE - record_shorthand_fields.md:1:9:1:12
UNDEFINED VARIABLE - record_shorthand_fields.md:1:14:1:19
UNDEFINED VARIABLE - record_shorthand_fields.md:1:21:1:27
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **name** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_shorthand_fields.md:1:3:1:7:**
```roc
{ name, age, email, active }
```
  ^^^^


**UNDEFINED VARIABLE**
Nothing is named **age** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_shorthand_fields.md:1:9:1:12:**
```roc
{ name, age, email, active }
```
        ^^^


**UNDEFINED VARIABLE**
Nothing is named **email** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_shorthand_fields.md:1:14:1:19:**
```roc
{ name, age, email, active }
```
             ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **active** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_shorthand_fields.md:1:21:1:27:**
```roc
{ name, age, email, active }
```
                    ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.lookup "name")
  (Expr.lookup "age")
  (Expr.lookup "email")
  (Expr.lookup "active")
)
~~~
# SOLVED
~~~clojure
; Total type variables: 8
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 -> #7)
(var #6 {})
(var #7 record)
~~~
# TYPES
~~~roc
~~~
