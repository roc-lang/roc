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
            (lc "x")
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
	x
})({})

# Inner `x` should be used; outer `x` is not captured (it should be a shadowing warning)
y
~~~
# EXPECTED
DUPLICATE DEFINITION - let_shadows_capture.md:4:9:4:10
UNUSED VARIABLE - let_shadows_capture.md:2:5:2:6
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**let_shadows_capture.md:6:6:6:7:**
```roc
    })({}) # Inner `x` should be used; outer `x` is not captured (it should be a shadowing warning)
```
     ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.num_literal_i32 5)
  )
  (Stmt.assign
    (pattern (Patt.ident "y"))
    (Expr.fn_call)
  )
  (Expr.lookup "y")
)
~~~
# SOLVED
~~~clojure
; Total type variables: 21
(var #0 _)
(var #1 -> #2)
(var #2 Num *)
(var #3 _)
(var #4 -> #13)
(var #5 _)
(var #6 -> #7)
(var #7 Num *)
(var #8 _)
(var #9 _)
(var #10 -> #13)
(var #11 -> #18)
(var #12 -> #19)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 -> #19)
(var #18 -> #20)
(var #19 {})
(var #20 fn_pure)
~~~
# TYPES
~~~roc
x : Num(_size)
y : _a
~~~
