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
        (binop_colon
          (lc "x")
          (lc "x")
        )
      )
    )
  )
  (block
    (binop_colon
      (lc "x")
      (unary_neg <unary>)
    )
  )
)
~~~
# FORMATTED
~~~roc
(|{
	x : x
}| x)({
	x : -10
})
~~~
# EXPECTED
NIL
# PROBLEMS
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


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_argument_closure_single.md:1:16:1:20:**
```roc
(|{ x }| x )({ x: -10 })
```
               ^^^^


# CANONICALIZE
~~~clojure
(Expr.apply_ident)
~~~
# SOLVED
~~~clojure
(expr :tag apply_ident :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
