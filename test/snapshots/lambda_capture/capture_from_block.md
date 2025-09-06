# META
~~~ini
description="A lambda within a block expression captures a variable also defined within that block."
type=expr
~~~
# SOURCE
~~~roc
{
    a = 10
    b = (|_| a * 2)(5)
    b
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign OpenRound OpBar Underscore OpBar LowerIdent OpStar Int CloseRound OpenRound Int CloseRound LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "a")
    (num_literal_i32 10)
  )
  (binop_equals
    (lc "b")
    (apply_anon
      (lambda
        (body
          (binop_star
            (lc "a")
            (num_literal_i32 2)
          )
        )
        (args
          (underscore)
        )
      )
      (num_literal_i32 5)
    )
  )
  (lc "b")
)
~~~
# FORMATTED
~~~roc
a = 10
b = (|_| a * 2)(5)
b
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**capture_from_block.md:3:19:3:20:**
```roc
    b = (|_| a * 2)(5)
```
                  ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "a"))
    (Expr.num_literal_i32 10)
  )
  (Stmt.assign
    (pattern (Patt.ident "b"))
    (Expr.fn_call)
  )
  (Expr.lookup "b")
)
~~~
# SOLVED
~~~clojure
; Total type variables: 18
(var #0 _)
(var #1 -> #2)
(var #2 Num *)
(var #3 _)
(var #4 -> #11)
(var #5 _)
(var #6 -> #7)
(var #7 -> #8)
(var #8 -> #11)
(var #9 -> #16)
(var #10 Num *)
(var #11 Num *)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 -> #10)
(var #16 -> #17)
(var #17 fn_pure)
~~~
# TYPES
~~~roc
a : Num(_size)
b : Num(_size)
~~~
