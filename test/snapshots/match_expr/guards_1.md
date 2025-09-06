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
              (underscore)
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
PARSE ERROR - guards_1.md:2:7:2:7
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:2:16:2:18
IF WITHOUT ELSE - guards_1.md:2:7:2:9
UNEXPECTED TOKEN IN PATTERN - guards_1.md:2:20:2:30
PARSE ERROR - guards_1.md:2:30:2:30
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:2:30:2:32
UNEXPECTED TOKEN IN PATTERN - guards_1.md:2:32:2:35
PARSE ERROR - guards_1.md:2:43:2:43
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:2:43:2:44
UNEXPECTED TOKEN IN PATTERN - guards_1.md:2:44:2:44
PARSE ERROR - guards_1.md:2:44:2:44
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:2:44:2:45
PARSE ERROR - guards_1.md:3:7:3:7
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:3:16:3:18
IF WITHOUT ELSE - guards_1.md:3:7:3:9
UNEXPECTED TOKEN IN PATTERN - guards_1.md:3:20:3:30
PARSE ERROR - guards_1.md:3:30:3:30
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:3:30:3:32
UNEXPECTED TOKEN IN PATTERN - guards_1.md:3:32:3:35
PARSE ERROR - guards_1.md:3:43:3:43
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:3:43:3:44
UNEXPECTED TOKEN IN PATTERN - guards_1.md:3:44:3:44
PARSE ERROR - guards_1.md:3:44:3:44
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:3:44:3:45
UNDEFINED VARIABLE - guards_1.md:1:7:1:12
UNRECOGNIZED SYNTAX - guards_1.md:2:7:2:20
UNUSED VARIABLE - guards_1.md:2:5:2:6
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **value** in this scope.
Is there an **import** or **exposing** missing up-top?

**guards_1.md:1:7:1:12:**
```roc
match value {
```
      ^^^^^


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
; Total type variables: 21
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
~~~
# TYPES
~~~roc
~~~
