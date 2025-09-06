# META
~~~ini
description=Simple record destructuring pattern
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name, age } => name
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenCurly LowerIdent Comma LowerIdent CloseCurly OpFatArrow LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "person")
)
  (branch1     (binop_thick_arrow
      (record_literal
        (lc "name")
        (lc "age")
      )
      (lc "name")
    )
))
~~~
# FORMATTED
~~~roc
match person
	{name, age} => name
~~~
# EXPECTED
UNDEFINED VARIABLE - pattern_destructure_simple.md:1:7:1:13
UNUSED VARIABLE - pattern_destructure_simple.md:2:13:2:16
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **person** in this scope.
Is there an **import** or **exposing** missing up-top?

**pattern_destructure_simple.md:1:7:1:13:**
```roc
match person {
```
      ^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**pattern_destructure_simple.md:2:5:2:26:**
```roc
    { name, age } => name
```
    ^^^^^^^^^^^^^^^^^^^^^


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
