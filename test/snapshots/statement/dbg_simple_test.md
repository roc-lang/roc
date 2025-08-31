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
      (Expr.apply_ident)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
