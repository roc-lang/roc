# META
~~~ini
description=Match expression with various list destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match list {
    [] => 0
    [x] => x
    [first, second] => first + second
    [head, .. as tail] => head
    [One, Two, .. as rest] => 3
    [x, y, z, .. as more] => x + y + z
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare CloseSquare OpFatArrow Int OpenSquare LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent OpenSquare LowerIdent Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare UpperIdent Comma UpperIdent Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow Int OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "list")
)
  (branch1     (binop_thick_arrow
      (list_literal)
      (block
        (num_literal_i32 0)
        (binop_thick_arrow
          (list_literal
            (lc "x")
          )
          (lc "x")
        )
        (binop_thick_arrow
          (list_literal
            (lc "first")
            (lc "second")
          )
          (binop_plus
            (lc "first")
            (lc "second")
          )
        )
        (list_literal
          (lc "head")
          (unary_double_dot <unary_op>)
        )
        (lc "tail")
        (binop_thick_arrow
          (malformed)
          (lc "head")
        )
        (list_literal
          (uc "One")
          (uc "Two")
          (unary_double_dot <unary_op>)
        )
        (lc "rest")
        (binop_thick_arrow
          (malformed)
          (num_literal_i32 3)
        )
        (list_literal
          (lc "x")
          (lc "y")
          (lc "z")
          (unary_double_dot <unary_op>)
        )
        (lc "more")
        (binop_thick_arrow
          (malformed)
          (binop_plus
            (binop_plus
              (lc "x")
              (lc "y")
            )
            (lc "z")
          )
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match list
	[] => 
		0
		[x] => x
		[first, second] => first + second
		[head, ..as ]
		tail
		] => head
		[One, Two, ..as ]
		rest
		] => 3
		[x, y, z, ..as ]
		more
		] => (x + y) + z
~~~
# EXPECTED
UNDEFINED VARIABLE - list_destructure_variations.md:1:7:1:11
UNUSED VARIABLE - list_destructure_variations.md:1:1:1:1
UNUSED VARIABLE - list_destructure_variations.md:1:1:1:1
UNUSED VARIABLE - list_destructure_variations.md:1:1:1:1
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**list_destructure_variations.md:5:15:5:18:**
```roc
    [head, .. as tail] => head
```
              ^^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**list_destructure_variations.md:5:5:5:18:**
```roc
    [head, .. as tail] => head
```
    ^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**list_destructure_variations.md:5:22:5:24:**
```roc
    [head, .. as tail] => head
```
                     ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**list_destructure_variations.md:6:19:6:22:**
```roc
    [One, Two, .. as rest] => 3
```
                  ^^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**list_destructure_variations.md:6:5:6:22:**
```roc
    [One, Two, .. as rest] => 3
```
    ^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**list_destructure_variations.md:6:26:6:28:**
```roc
    [One, Two, .. as rest] => 3
```
                         ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**list_destructure_variations.md:7:18:7:21:**
```roc
    [x, y, z, .. as more] => x + y + z
```
                 ^^^


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**list_destructure_variations.md:7:5:7:21:**
```roc
    [x, y, z, .. as more] => x + y + z
```
    ^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**list_destructure_variations.md:7:25:7:27:**
```roc
    [x, y, z, .. as more] => x + y + z
```
                        ^^


**UNDEFINED VARIABLE**
Nothing is named **list** in this scope.
Is there an **import** or **exposing** missing up-top?

**list_destructure_variations.md:1:7:1:11:**
```roc
match list {
```
      ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**list_destructure_variations.md:2:5:7:39:**
```roc
    [] => 0
    [x] => x
    [first, second] => first + second
    [head, .. as tail] => head
    [One, Two, .. as rest] => 3
    [x, y, z, .. as more] => x + y + z
```


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 49
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
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
~~~
# TYPES
~~~roc
~~~
