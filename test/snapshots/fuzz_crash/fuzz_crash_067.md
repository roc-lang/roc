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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_067.md:3:8:5:2:**
```roc
f = || {
    crash 1
}
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "f"))
    (Expr.malformed)
  )
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
