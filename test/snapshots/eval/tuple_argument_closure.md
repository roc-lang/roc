# META
~~~ini
description=Tuple as an argument
type=expr
~~~
# SOURCE
~~~roc
(|(x,y)| x * y )((1,2))
~~~
# TOKENS
~~~text
OpenRound OpBar OpenRound LowerIdent Comma LowerIdent CloseRound OpBar LowerIdent OpStar LowerIdent CloseRound OpenRound OpenRound Int Comma Int CloseRound CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (lambda
    (body
      (binop_star
        (lc "x")
        (lc "y")
      )
    )
    (args
      (tuple_literal
        (lc "x")
        (lc "y")
      )
    )
  )
  (tuple_literal
    (num_literal_i32 1)
    (num_literal_i32 2)
  )
)
~~~
# FORMATTED
~~~roc
(|x, y| x * y)((1, 2))
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**tuple_argument_closure.md:1:15:1:17:**
```roc
(|(x,y)| x * y )((1,2))
```
              ^^


# CANONICALIZE
~~~clojure
(Expr.fn_call)
~~~
# SOLVED
~~~clojure
; Total type variables: 16
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 -> #5)
(var #5 -> #6)
(var #6 -> #11)
(var #7 -> #13)
(var #8 Num *)
(var #9 Num *)
(var #10 -> #14)
(var #11 _)
(var #12 -> #14)
(var #13 -> #15)
(var #14 tuple)
(var #15 fn_pure)
~~~
# TYPES
~~~roc
x : _a
y : _a
~~~
