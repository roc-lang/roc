# META
~~~ini
description=Simple debug test to understand parsing behavior
type=file
~~~
# SOURCE
~~~roc
module [test]

test = {
    x = 42
    dbg(x)
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpAssign OpenCurly LowerIdent OpAssign Int KwDbg OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "test")
))
(block
  (binop_equals
    (lc "test")
    (block
      (binop_equals
        (lc "x")
        (num_literal_i32 42)
      )
      (apply_anon
        (malformed)
        (lc "x")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [test]

test = {
	x = 42
	dbg(x)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**dbg_simple_test.md:5:5:5:8:**
```roc
    dbg(x)
```
    ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "test"))
    (Expr.block
      (Stmt.assign
        (pattern (Patt.ident "x"))
        (Expr.num_literal_i32 42)
      )
      (Expr.fn_call)
    )
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 14
(var #0 _)
(var #1 _)
(var #2 -> #9)
(var #3 -> #4)
(var #4 Num *)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 -> #13)
(var #13 fn_pure)
~~~
# TYPES
~~~roc
~~~
