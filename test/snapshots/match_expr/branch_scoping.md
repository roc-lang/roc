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
NIL
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
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
