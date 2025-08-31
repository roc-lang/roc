# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module []

f = || {
    crash 1
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpAssign OpOr OpenCurly KwCrash Int CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

f = || 
{
	crash 1
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **|| ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_067.md:3:5:3:8:**
```roc
f = || {
```
    ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "f")
    (Expr.malformed)
  )
  (Expr.block
    (Expr.crash
      (Expr.num_literal_i32 1)
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
f : Error
~~~
