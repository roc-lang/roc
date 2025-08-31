# META
~~~ini
description="A `let` binding inside a lambda's body shadows a would-be captured variable."
type=expr
~~~
# SOURCE
~~~roc
{
    x = 5
    y = (|_| { 
        x = 10
        x 
    })({}) # Inner `x` should be used; outer `x` is not captured (it should be a shadowing warning)
    y
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign OpenRound OpBar Underscore OpBar OpenCurly LowerIdent OpAssign Int LowerIdent CloseCurly CloseRound OpenRound OpenCurly CloseCurly CloseRound LineComment LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "x")
    (num_literal_i32 5)
  )
  (binop_equals
    (lc "y")
    (apply_anon
      (lambda
        (body
          (block
            (binop_equals
              (lc "x")
              (num_literal_i32 10)
            )
            (binop_colon
              (lc "x")
              (lc "x")
            )
          )
        )
        (args
          (underscore)
        )
      )
      (record_literal)
    )
  )
  (lc "y")
)
~~~
# FORMATTED
~~~roc
x = 5
y = (|_| {
	x = 10
	x : x
})({})

# Inner `x` should be used; outer `x` is not captured (it should be a shadowing warning)
y
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **x** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:

**let_shadows_capture.md:5:9:5:10:**
```roc
        x 
```
        ^


**UNUSED VARIABLE**
Variable **x** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:

**let_shadows_capture.md:2:5:2:6:**
```roc
    x = 5
```
    ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "y")
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
x : Num(_size)
y : _a
~~~
