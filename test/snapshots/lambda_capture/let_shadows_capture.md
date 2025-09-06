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


**UNUSED VARIABLE**
Variable **x** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:

**let_shadows_capture.md:5:9:5:10:**
```roc
        x 
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
    (Expr.apply_ident)
  )
  (Expr.lookup "y")
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
