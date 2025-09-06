# META
~~~ini
description=Record with single field as an argument
type=expr
~~~
# SOURCE
~~~roc
(|{ x }| x )({ x: -10 })
~~~
# TOKENS
~~~text
OpenRound OpBar OpenCurly LowerIdent CloseCurly OpBar LowerIdent CloseRound OpenRound OpenCurly LowerIdent OpColon OpUnaryMinus Int CloseCurly CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (lambda
    (body
      (lc "x")
    )
    (args
      (block
        (lc "x")
      )
    )
  )
  (block
    (binop_colon
      (lc "x")
      (unary_neg <unary_op>)
    )
  )
)
~~~
# FORMATTED
~~~roc
(|{
	x
}| x)({
	x : -10
})
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**record_argument_closure_single.md:1:11:1:13:**
```roc
(|{ x }| x )({ x: -10 })
```
          ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**record_argument_closure_single.md:1:3:1:8:**
```roc
(|{ x }| x )({ x: -10 })
```
  ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **x** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_argument_closure_single.md:1:10:1:11:**
```roc
(|{ x }| x )({ x: -10 })
```
         ^


# CANONICALIZE
~~~clojure
(Expr.fn_call)
~~~
# SOLVED
~~~clojure
; Total type variables: 15
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 -> #10)
(var #4 -> #12)
(var #5 _)
(var #6 Num *)
(var #7 _)
(var #8 _)
(var #9 -> #13)
(var #10 _)
(var #11 -> #13)
(var #12 -> #14)
(var #13 {})
(var #14 fn_pure)
~~~
# TYPES
~~~roc
~~~
