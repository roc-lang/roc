# META
~~~ini
description=Match expression with mixed tag and list patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match data {
    Ok([x, y]) => x + y
    Err(x) => x - 1
    Ok([x]) => x * 2
    Err(y) => y / 2
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpenRound OpenSquare LowerIdent Comma LowerIdent CloseSquare CloseRound OpThinArrow LowerIdent OpPlus LowerIdent UpperIdent OpenRound LowerIdent CloseRound OpThinArrow LowerIdent OpBinaryMinus Int UpperIdent OpenRound OpenSquare LowerIdent CloseSquare CloseRound OpThinArrow LowerIdent OpStar Int UpperIdent OpenRound LowerIdent CloseRound OpThinArrow LowerIdent OpSlash Int CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "data")
)
  (branch1     (binop_thick_arrow
      (apply_uc
        (uc "Ok")
        (list_literal
          (lc "x")
          (lc "y")
        )
      )
      (binop_plus
        (lc "x")
        (lc "y")
      )
    )
)
  (branch2     (binop_thick_arrow
      (apply_uc
        (uc "Err")
        (lc "x")
      )
      (binop_minus
        (lc "x")
        (num_literal_i32 1)
      )
    )
)
  (branch3     (binop_thick_arrow
      (apply_uc
        (uc "Ok")
        (list_literal
          (lc "x")
        )
      )
      (binop_star
        (lc "x")
        (num_literal_i32 2)
      )
    )
)
  (branch4     (binop_thick_arrow
      (apply_uc
        (uc "Err")
        (lc "y")
      )
      (binop_slash
        (lc "y")
        (num_literal_i32 2)
      )
    )
))
~~~
# FORMATTED
~~~roc
match data
	Ok([x, y]) => x + y
	Err(x) => x - 1
	Ok([x]) => x * 2
	Err(y) => y / 2
~~~
# EXPECTED
UNDEFINED VARIABLE - mixed_pattern_scoping.md:1:7:1:11
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **data** in this scope.
Is there an **import** or **exposing** missing up-top?

**mixed_pattern_scoping.md:1:7:1:11:**
```roc
match data {
```
      ^^^^


**SHADOWING**
This definition shadows an existing one.

**mixed_pattern_scoping.md:3:9:3:10:**
```roc
    Err(x) => x - 1
```
        ^


**SHADOWING**
This definition shadows an existing one.

**mixed_pattern_scoping.md:4:9:4:10:**
```roc
    Ok([x]) => x * 2
```
        ^


**SHADOWING**
This definition shadows an existing one.

**mixed_pattern_scoping.md:5:9:5:10:**
```roc
    Err(y) => y / 2
```
        ^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 34
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
(var #15 Num *)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 Num *)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 Num *)
(var #31 _)
(var #32 _)
(var #33 _)
~~~
# TYPES
~~~roc
y : _a
x : _a
~~~
