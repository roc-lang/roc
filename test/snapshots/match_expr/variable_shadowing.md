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
UNDEFINED VARIABLE - variable_shadowing.md:1:8:1:13
UNDEFINED VARIABLE - variable_shadowing.md:1:15:1:20
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**variable_shadowing.md:2:26:3:5:**
```roc
    (Some(x), y) => x + y
    (None, x) => x * 2
```


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
; Total type variables: 22
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
~~~
# TYPES
~~~roc
~~~
