# META
~~~ini
description=Comprehensive test for match branch scoping with variable isolation
type=expr
~~~
# SOURCE
~~~roc
match result {
    Ok(value) => value + 1
    Err(value) => value - 1
    Ok(different) => different * 2
    Err(different) => different / 2
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent OpPlus Int UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent OpBinaryMinus Int UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent OpStar Int UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent OpSlash Int CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "result")
)
  (branch1     (binop_thick_arrow
      (apply_uc
        (uc "Ok")
        (lc "value")
      )
      (binop_plus
        (lc "value")
        (num_literal_i32 1)
      )
    )
)
  (branch2     (binop_thick_arrow
      (apply_uc
        (uc "Err")
        (lc "value")
      )
      (binop_minus
        (lc "value")
        (num_literal_i32 1)
      )
    )
)
  (branch3     (binop_thick_arrow
      (apply_uc
        (uc "Ok")
        (lc "different")
      )
      (binop_star
        (lc "different")
        (num_literal_i32 2)
      )
    )
)
  (branch4     (binop_thick_arrow
      (apply_uc
        (uc "Err")
        (lc "different")
      )
      (binop_slash
        (lc "different")
        (num_literal_i32 2)
      )
    )
))
~~~
# FORMATTED
~~~roc
match result
	Ok(value) => value + 1
	Err(value) => value - 1
	Ok(different) => different * 2
	Err(different) => different / 2
~~~
# EXPECTED
UNDEFINED VARIABLE - branch_scoping.md:1:7:1:13
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **result** in this scope.
Is there an **import** or **exposing** missing up-top?

**branch_scoping.md:1:7:1:13:**
```roc
match result {
```
      ^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**branch_scoping.md:2:5:2:27:**
```roc
    Ok(value) => value + 1
```
    ^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**branch_scoping.md:4:5:4:35:**
```roc
    Ok(different) => different * 2
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 31
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
(var #13 Num *)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 Num *)
(var #28 _)
(var #29 _)
(var #30 _)
~~~
# TYPES
~~~roc
value : _a
different : _a
~~~
