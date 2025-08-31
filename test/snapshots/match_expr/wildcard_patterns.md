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
NIL
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


**UNUSED VARIABLE**
Variable **other** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_other` to suppress this warning.
The unused variable is declared here:

**wildcard_patterns.md:4:5:4:10:**
```roc
    other => "something else"
```
    ^^^^^


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
