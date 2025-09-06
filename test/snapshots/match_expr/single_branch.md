# META
~~~ini
description=Match expression with single branch (simple variable pattern)
type=expr
~~~
# SOURCE
~~~roc
match value {
    x => x + 1
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly LowerIdent OpFatArrow LowerIdent OpPlus Int CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "value")
)
  (branch1     (binop_thick_arrow
      (lc "x")
      (binop_plus
        (lc "x")
        (num_literal_i32 1)
      )
    )
))
~~~
# FORMATTED
~~~roc
match value
	x => x + 1
~~~
# EXPECTED
UNDEFINED VARIABLE - single_branch.md:1:7:1:12
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **value** in this scope.
Is there an **import** or **exposing** missing up-top?

**single_branch.md:1:7:1:12:**
```roc
match value {
```
      ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**single_branch.md:2:5:2:15:**
```roc
    x => x + 1
```
    ^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 8
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
~~~
# TYPES
~~~roc
~~~
