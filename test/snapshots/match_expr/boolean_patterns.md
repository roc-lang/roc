# META
~~~ini
description=Match expression with boolean-like tag patterns
type=expr
~~~
# SOURCE
~~~roc
match isReady {
	True => "ready to go!"
	False => "not ready yet"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpFatArrow String UpperIdent OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "isReady")
)
  (branch1     (binop_thick_arrow
      (uc "True")
      (str_literal_big "ready to go!")
    )
)
  (branch2     (binop_thick_arrow
      (uc "False")
      (str_literal_big "not ready yet")
    )
))
~~~
# FORMATTED
~~~roc
match isReady
	True => "ready to go!"
	False => "not ready yet"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **isReady** in this scope.
Is there an **import** or **exposing** missing up-top?

**boolean_patterns.md:1:7:1:14:**
```roc
match isReady {
```
      ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**boolean_patterns.md:2:2:2:24:**
```roc
	True => "ready to go!"
```
	^^^^^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
