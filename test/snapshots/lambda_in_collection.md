# META
~~~ini
description=Lambda inside a collection
type=expr
~~~
# SOURCE
~~~roc
(
	|
		a,
		b,
	| {
		a + b
	},
	|a, b| {
		a - b
	},
)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent Comma LowerIdent Comma OpBar OpenCurly LowerIdent OpPlus LowerIdent CloseCurly Comma OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpBinaryMinus LowerIdent CloseCurly Comma CloseRound ~~~
# PARSE
~~~clojure
(lambda
  (body
    (tuple_literal
      (block
        (binop_plus
          (lc "a")
          (lc "b")
        )
      )
      (lambda
        (body
          (block
            (binop_minus
              (lc "a")
              (lc "b")
            )
          )
        )
        (args
          (lc "a")
          (lc "b")
        )
      )
    )
  )
  (args
    (lc "a")
    (lc "b")
  )
)
~~~
# FORMATTED
~~~roc
|a, b| ({
	a + b
}, |a, b| {
	a - b
})
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**lambda_in_collection.md:8:3:8:4:**
```roc
	|a, b| {
```
	 ^


**SHADOWING**
This definition shadows an existing one.

**lambda_in_collection.md:8:6:8:7:**
```roc
	|a, b| {
```
	    ^


# CANONICALIZE
~~~clojure
(Expr.lambda (canonicalized))
~~~
# SOLVED
~~~clojure
; Total type variables: 23
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 -> #4)
(var #4 -> #5)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 -> #10)
(var #10 -> #11)
(var #11 _)
(var #12 _)
(var #13 -> #20)
(var #14 -> #21)
(var #15 -> #22)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 fn_pure)
(var #21 tuple)
(var #22 fn_pure)
~~~
# TYPES
~~~roc
a : _c
b : _c
~~~
