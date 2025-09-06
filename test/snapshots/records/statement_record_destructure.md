# META
~~~ini
description=Record destructuring in assignment statement
type=file
~~~
# SOURCE
~~~roc
module []

{ name, age, email } = person
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine OpenCurly LowerIdent Comma LowerIdent Comma LowerIdent CloseCurly OpAssign LowerIdent ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_equals
    (record_literal
      (lc "name")
      (binop_colon
        (lc "age")
        (lc "age")
      )
      (binop_colon
        (lc "email")
        (lc "email")
      )
    )
    (lc "person")
  )
)
~~~
# FORMATTED
~~~roc
module []

{ name, age: age, email: email } = person
~~~
# EXPECTED
UNDEFINED VARIABLE - statement_record_destructure.md:3:24:3:30
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **person** in this scope.
Is there an **import** or **exposing** missing up-top?

**statement_record_destructure.md:3:24:3:30:**
```roc
{ name, age, email } = person
```
                       ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.record_destructure))
    (Expr.lookup "person")
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 10
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 -> #7)
(var #7 _)
(var #8 _)
(var #9 _)
~~~
# TYPES
~~~roc
name : _a
~~~
