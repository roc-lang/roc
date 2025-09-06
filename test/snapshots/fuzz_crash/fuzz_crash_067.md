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
(block
  (binop_equals
    (lc "f")
    (malformed)
  )
  (block
    (crash
      (num_literal_i32 1)
    )
  )
)
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
CRASH EXPECTS STRING - fuzz_crash_067.md:3:8:5:2
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
  (Stmt.assign
    (pattern (Patt.ident "f"))
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
; Total type variables: 9
(var #0 _)
(var #1 -> #8)
(var #2 _)
(var #3 _)
(var #4 Num *)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
~~~
# TYPES
~~~roc
f : _a
~~~
