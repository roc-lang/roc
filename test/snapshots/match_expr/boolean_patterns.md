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
UNDEFINED VARIABLE - boolean_patterns.md:1:7:1:14
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **isReady** in this scope.
Is there an **import** or **exposing** missing up-top?

**boolean_patterns.md:1:7:1:14:**
```roc
match isReady {
```
      ^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 9
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 Str)
(var #4 _)
(var #5 _)
(var #6 Str)
(var #7 _)
(var #8 _)
~~~
# TYPES
~~~roc
~~~
