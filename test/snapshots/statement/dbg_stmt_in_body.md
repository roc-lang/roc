# META
~~~ini
description=Debug statement in body context
type=file
~~~
# SOURCE
~~~roc
module [main]

main = {
    x = 42
    dbg x
    x + 1
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpAssign OpenCurly LowerIdent OpAssign Int KwDbg LowerIdent LowerIdent OpPlus Int CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "main")
))
(block
  (binop_equals
    (lc "main")
    (block
      (binop_equals
        (lc "x")
        (num_literal_i32 42)
      )
      (malformed)
      (lc "x")
      (binop_plus
        (lc "x")
        (num_literal_i32 1)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [main]

main = {
	x = 42
	dbg 
	x
	x + 1
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**dbg_stmt_in_body.md:5:5:5:9:**
```roc
    dbg x
```
    ^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.block
      (Stmt.assign
        (pattern (Patt.ident "x"))
        (Expr.num_literal_i32 42)
      )
      (Expr.malformed)
      (Expr.lookup "x")
      (Expr.binop_plus
        (Expr.lookup "x")
        (Expr.num_literal_i32 1)
      )
    )
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 15
(var #0 _)
(var #1 _)
(var #2 -> #11)
(var #3 -> #4)
(var #4 Num *)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 -> #9)
(var #9 -> #10)
(var #10 Num *)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
~~~
# TYPES
~~~roc
~~~
