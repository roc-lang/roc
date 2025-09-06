# META
~~~ini
description=Match expression with multiple patterns in one branch
type=expr
~~~
# SOURCE
~~~roc
match color {
    Blue | Green | Red => 1
    Black => 2
    White => 3
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpBar UpperIdent OpBar UpperIdent OpFatArrow Int UpperIdent OpFatArrow Int UpperIdent OpFatArrow Int CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "color")
)
  (branch1     (binop_thick_arrow
      (binop_or
        (binop_or
          (uc "Blue")
          (uc "Green")
        )
        (uc "Red")
      )
      (num_literal_i32 1)
    )
)
  (branch2     (binop_thick_arrow
      (uc "Black")
      (num_literal_i32 2)
    )
)
  (branch3     (binop_thick_arrow
      (uc "White")
      (num_literal_i32 3)
    )
))
~~~
# FORMATTED
~~~roc
match color
	(Blue || Green) || Red => 1
	Black => 2
	White => 3
~~~
# EXPECTED
UNDEFINED VARIABLE - multi_pattern_branch.md:1:7:1:12
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **color** in this scope.
Is there an **import** or **exposing** missing up-top?

**multi_pattern_branch.md:1:7:1:12:**
```roc
match color {
```
      ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multi_pattern_branch.md:2:5:2:28:**
```roc
    Blue | Green | Red => 1
```
    ^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multi_pattern_branch.md:4:5:4:15:**
```roc
    White => 3
```
    ^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 16
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
(var #10 Num *)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
~~~
# TYPES
~~~roc
~~~
