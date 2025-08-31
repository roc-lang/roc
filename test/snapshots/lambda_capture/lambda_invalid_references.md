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
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **z** in this scope.
Is there an **import** or **exposing** missing up-top?

**lambda_invalid_references.md:1:13:1:14:**
```roc
|x| |y| x + z
```
            ^


**UNUSED VARIABLE**
Variable **y** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_y` to suppress this warning.
The unused variable is declared here:

**lambda_invalid_references.md:1:6:1:7:**
```roc
|x| |y| x + z
```
     ^


# CANONICALIZE
~~~clojure
(Expr.lambda)
~~~
# SOLVED
~~~clojure
(expr :tag lambda :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
