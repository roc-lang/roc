# META
~~~ini
description=Mixed capture patterns in block expression
type=expr
~~~
# SOURCE
~~~roc
(|base| {
		simple = |x| base + x + 1
		simple(1)
})(1)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent OpPlus Int LowerIdent OpenRound Int CloseRound CloseCurly CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (lambda
    (body
      (block
        (binop_equals
          (lc "simple")
          (lambda
            (body
              (binop_plus
                (binop_plus
                  (lc "base")
                  (lc "x")
                )
                (num_literal_i32 1)
              )
            )
            (args
              (lc "x")
            )
          )
        )
        (apply_lc
          (lc "simple")
          (num_literal_i32 1)
        )
      )
    )
    (args
      (lc "base")
    )
  )
  (num_literal_i32 1)
)
~~~
# FORMATTED
~~~roc
(|base| {
	simple = |x| (base + x) + 1
	simple(1)
})(1)
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**lambda_capture_mixed_patterns.md:4:2:4:3:**
```roc
})(1)
```
 ^


# CANONICALIZE
~~~clojure
(Expr.fn_call)
~~~
# SOLVED
~~~clojure
; Total type variables: 24
(var #0 _)
(var #1 _)
(var #2 -> #20)
(var #3 _)
(var #4 -> #5)
(var #5 -> #6)
(var #6 -> #7)
(var #7 -> #8)
(var #8 Num *)
(var #9 -> #20)
(var #10 _)
(var #11 -> #21)
(var #12 Num *)
(var #13 _)
(var #14 -> #17)
(var #15 -> #22)
(var #16 Num *)
(var #17 _)
(var #18 -> #16)
(var #19 _)
(var #20 fn_pure)
(var #21 fn_pure)
(var #22 -> #23)
(var #23 fn_pure)
~~~
# TYPES
~~~roc
base : _a
x : _a
~~~
