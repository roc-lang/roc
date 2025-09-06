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
      (lc "age")
      (lc "email")
    )
    (lc "person")
  )
)
~~~
# FORMATTED
~~~roc
module []

{ name, age, email } = person
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
; Total type variables: 8
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 -> #5)
(var #5 _)
(var #6 _)
(var #7 _)
~~~
# TYPES
~~~roc
age : _a
name : _a
email : _a
~~~
