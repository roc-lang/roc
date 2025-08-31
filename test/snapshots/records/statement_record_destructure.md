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
~~~
# FORMATTED
~~~roc
module []


{ name, age, email } = person
~~~
# EXPECTED
NIL
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
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
