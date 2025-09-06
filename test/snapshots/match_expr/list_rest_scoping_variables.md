# META
~~~ini
description=List rest patterns with proper variable scoping across branches
type=expr
~~~
# SOURCE
~~~roc
match data {
    [..items] => 1
    [first, ..items] => first
    [..items, last] => last
    [first, ..items, last] => first + last
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare DoubleDot LowerIdent CloseSquare OpFatArrow Int OpenSquare LowerIdent Comma DoubleDot LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "data")
)
  (branch1     (binop_thick_arrow
      (list_literal
        (double_dot_lc "items")
      )
      (block
        (num_literal_i32 1)
        (binop_thick_arrow
          (list_literal
            (lc "first")
            (unary_double_dot <unary_op>)
          )
          (lc "first")
        )
        (binop_thick_arrow
          (list_literal
            (unary_double_dot <unary_op>)
            (lc "last")
          )
          (lc "last")
        )
        (binop_thick_arrow
          (list_literal
            (lc "first")
            (unary_double_dot <unary_op>)
            (lc "last")
          )
          (binop_plus
            (lc "first")
            (lc "last")
          )
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match data
	[..items] => 
		1
		[first, ..items] => first
		[..items, last] => last
		[first, ..items, last] => first + last
~~~
# EXPECTED
BAD LIST REST PATTERN SYNTAX - list_rest_scoping_variables.md:2:6:2:13
BAD LIST REST PATTERN SYNTAX - list_rest_scoping_variables.md:3:13:3:20
BAD LIST REST PATTERN SYNTAX - list_rest_scoping_variables.md:4:6:4:13
BAD LIST REST PATTERN SYNTAX - list_rest_scoping_variables.md:5:13:5:20
UNDEFINED VARIABLE - list_rest_scoping_variables.md:1:7:1:11
UNUSED VARIABLE - list_rest_scoping_variables.md:2:8:2:8
UNUSED VARIABLE - list_rest_scoping_variables.md:3:15:3:15
UNUSED VARIABLE - list_rest_scoping_variables.md:4:8:4:8
UNUSED VARIABLE - list_rest_scoping_variables.md:5:15:5:15
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **data** in this scope.
Is there an **import** or **exposing** missing up-top?

**list_rest_scoping_variables.md:1:7:1:11:**
```roc
match data {
```
      ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**list_rest_scoping_variables.md:3:13:3:15:**
```roc
    [first, ..items] => first
```
            ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**list_rest_scoping_variables.md:4:6:4:8:**
```roc
    [..items, last] => last
```
     ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**list_rest_scoping_variables.md:5:13:5:15:**
```roc
    [first, ..items, last] => first + last
```
            ^^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 29
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 Num *)
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
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
~~~
# TYPES
~~~roc
items : _a
last : _a
first : _a
~~~
