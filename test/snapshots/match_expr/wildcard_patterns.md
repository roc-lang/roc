# META
~~~ini
description=Match expression with tag patterns and variable catch-all pattern
type=expr
~~~
# SOURCE
~~~roc
match value {
    Answer => "the answer"
    Zero => "zero"
    other => "something else"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpFatArrow String UpperIdent OpFatArrow String LowerIdent OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "value")
)
  (branch1     (binop_thick_arrow
      (uc "Answer")
      (str_literal_big "the answer")
    )
)
  (branch2     (binop_thick_arrow
      (uc "Zero")
      (block
        (str_literal_small "zero")
        (binop_thick_arrow
          (lc "other")
          (str_literal_big "something else")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match value
	Answer => "the answer"
	Zero => 
		"zero"
		other => "something else"
~~~
# EXPECTED
UNDEFINED VARIABLE - wildcard_patterns.md:1:7:1:12
UNUSED VARIABLE - wildcard_patterns.md:4:5:4:10
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **value** in this scope.
Is there an **import** or **exposing** missing up-top?

**wildcard_patterns.md:1:7:1:12:**
```roc
match value {
```
      ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**wildcard_patterns.md:2:5:2:27:**
```roc
    Answer => "the answer"
```
    ^^^^^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 13
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 Str)
(var #7 _)
(var #8 Str)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
~~~
# TYPES
~~~roc
other : _a
~~~
