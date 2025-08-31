# META
~~~ini
description=Match expression demonstrating variable shadowing between outer scope and branches
type=expr
~~~
# SOURCE
~~~roc
match (value, other) {
    (Some(x), y) => x + y
    (None, x) => x * 2
}
~~~
# TOKENS
~~~text
KwMatch OpenRound LowerIdent Comma LowerIdent CloseRound OpenCurly OpenRound UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent CloseRound OpFatArrow LowerIdent OpPlus LowerIdent OpenRound UpperIdent Comma LowerIdent CloseRound OpFatArrow LowerIdent OpStar Int CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (tuple_literal
      (lc "value")
      (lc "other")
    )
)
  (branch1     (binop_thick_arrow
      (tuple_literal
        (apply_uc
          (uc "Some")
          (lc "x")
        )
        (lc "y")
      )
      (binop_thick_arrow
        (binop_plus
          (lc "x")
          (apply_lc
            (lc "y")
            (tuple_literal
              (uc "None")
              (lc "x")
            )
          )
        )
        (binop_star
          (lc "x")
          (num_literal_i32 2)
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match (value, other)
	(Some(x), y) => x + y((None, x)) => x * 2
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **value** in this scope.
Is there an **import** or **exposing** missing up-top?

**variable_shadowing.md:1:8:1:13:**
```roc
match (value, other) {
```
       ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **other** in this scope.
Is there an **import** or **exposing** missing up-top?

**variable_shadowing.md:1:15:1:20:**
```roc
match (value, other) {
```
              ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**variable_shadowing.md:2:5:3:23:**
```roc
    (Some(x), y) => x + y
    (None, x) => x * 2
```


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
