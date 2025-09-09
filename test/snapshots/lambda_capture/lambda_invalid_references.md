# META
~~~ini
description=Error handling for invalid variable references in lambda captures
type=expr
~~~
# SOURCE
~~~roc
|x| |y| x + z
~~~
# TOKENS
~~~text
OpBar LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent ~~~
# PARSE
~~~clojure
(lambda
  (body
    (lambda
      (body
        (binop_plus
          (lc "x")
          (lc "z")
        )
      )
      (args
        (lc "y")
      )
    )
  )
  (args
    (lc "x")
  )
)
~~~
# FORMATTED
~~~roc
|x| |y| x + z
~~~
# EXPECTED
UNDEFINED VARIABLE - lambda_invalid_references.md:1:13:1:14
UNUSED VARIABLE - lambda_invalid_references.md:1:6:1:7
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **z** in this scope.
Is there an **import** or **exposing** missing up-top?

**lambda_invalid_references.md:1:13:1:14:**
```roc
|x| |y| x + z
```
            ^


# CANONICALIZE
~~~clojure
(Expr.lambda (canonicalized))
~~~
# SOLVED
~~~clojure
; Total type variables: 12
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 -> #4)
(var #4 -> #5)
(var #5 _)
(var #6 -> #10)
(var #7 -> #11)
(var #8 _)
(var #9 _)
(var #10 fn_pure)
(var #11 fn_pure)
~~~
# TYPES
~~~roc
x : _a
y : _a
~~~
