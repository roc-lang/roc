# META
~~~ini
description=Boolean closure type checking - should have no errors
type=expr
~~~
# SOURCE
~~~roc
(|x| !x)(True)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar OpBang LowerIdent CloseRound OpenRound UpperIdent CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (lambda
    (body
      (unary_not <unary_op>)
    )
    (args
      (lc "x")
    )
  )
  (uc "True")
)
~~~
# FORMATTED
~~~roc
(|x| !x)(True)
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**bool_closure_type_check.md:1:7:1:9:**
```roc
(|x| !x)(True)
```
      ^^


# CANONICALIZE
~~~clojure
(Expr.fn_call)
~~~
# SOLVED
~~~clojure
; Total type variables: 10
(var #0 _)
(var #1 _)
(var #2 -> #6)
(var #3 -> #2)
(var #4 -> #8)
(var #5 _)
(var #6 _)
(var #7 -> #5)
(var #8 -> #9)
(var #9 fn_pure)
~~~
# TYPES
~~~roc
x : _a
~~~
