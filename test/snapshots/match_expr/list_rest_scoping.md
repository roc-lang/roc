# META
~~~ini
description=Match expression with list rest patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match items {
    [first, ..rest] => first + 1
    [..rest, last] => last + 2
    [x, ..rest, y] => x + y
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent Comma DoubleDot LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus Int OpenSquare DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus Int OpenSquare LowerIdent Comma DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "items")
)
  (branch1     (binop_thick_arrow
      (list_literal
        (lc "first")
        (double_dot_lc "rest")
      )
      (block
        (binop_plus
          (lc "first")
          (num_literal_i32 1)
        )
        (binop_thick_arrow
          (list_literal
            (unary_double_dot <unary_op>)
            (lc "last")
          )
          (binop_plus
            (lc "last")
            (num_literal_i32 2)
          )
        )
        (binop_thick_arrow
          (list_literal
            (lc "x")
            (unary_double_dot <unary_op>)
            (lc "y")
          )
          (binop_plus
            (lc "x")
            (lc "y")
          )
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match items
	[first, ..rest] => 
		first + 1
		[..rest, last] => last + 2
		[x, ..rest, y] => x + y
~~~
# EXPECTED
BAD LIST REST PATTERN SYNTAX - list_rest_scoping.md:2:13:2:19
BAD LIST REST PATTERN SYNTAX - list_rest_scoping.md:3:6:3:12
BAD LIST REST PATTERN SYNTAX - list_rest_scoping.md:4:9:4:15
UNDEFINED VARIABLE - list_rest_scoping.md:1:7:1:12
UNUSED VARIABLE - list_rest_scoping.md:2:15:2:15
UNUSED VARIABLE - list_rest_scoping.md:3:8:3:8
UNUSED VARIABLE - list_rest_scoping.md:4:11:4:11
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **items** in this scope.
Is there an **import** or **exposing** missing up-top?

**list_rest_scoping.md:1:7:1:12:**
```roc
match items {
```
      ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**list_rest_scoping.md:3:6:3:8:**
```roc
    [..rest, last] => last + 2
```
     ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**list_rest_scoping.md:4:9:4:11:**
```roc
    [x, ..rest, y] => x + y
```
        ^^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 28
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 Num *)
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
(var #27 _)
~~~
# TYPES
~~~roc
y : _a
x : _a
rest : _a
last : _a
first : _a
~~~
