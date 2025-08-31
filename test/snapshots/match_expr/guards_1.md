# META
~~~ini
description=Match expression with guard conditions using if clauses
type=expr
~~~
# SOURCE
~~~roc
match value {
    x if x > 0 => "positive: ${Num.toStr x}"
    x if x < 0 => "negative: ${Num.toStr x}"
    _ => "other"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly LowerIdent KwIf LowerIdent OpGreaterThan Int OpFatArrow String LowerIdent KwIf LowerIdent OpLessThan Int OpFatArrow String Underscore OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "value")
)
  (branch1     (binop_thick_arrow
      (if_without_else
        (condition           (lc "x")
)
        (then           (binop_gt
            (lc "x")
            (num_literal_i32 0)
          )
))
      (block
        (str_literal_big "positive: ${Num.toStr x}")
        (lc "x")
        (if_without_else
          (condition             (binop_thick_arrow
              (binop_lt
                (lc "x")
                (num_literal_i32 0)
              )
              (str_literal_big "negative: ${Num.toStr x}")
            )
)
          (then             (binop_thick_arrow
              (malformed malformed:expr_unexpected_token)
              (str_literal_big "other")
            )
))
      )
    )
))
~~~
# FORMATTED
~~~roc
match value
	x if x > 0 => 
		"positive: ${Num.toStr x}"
		x
		if x < 0 => "negative: ${Num.toStr x}" _ => "other"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **_ ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_1.md:4:5:4:7:**
```roc
    _ => "other"
```
    ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**guards_1.md:2:5:4:17:**
```roc
    x if x > 0 => "positive: ${Num.toStr x}"
    x if x < 0 => "negative: ${Num.toStr x}"
    _ => "other"
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
