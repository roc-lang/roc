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
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
