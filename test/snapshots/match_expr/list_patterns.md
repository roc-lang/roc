# META
~~~ini
description=Match expression with list patterns including invalid rest pattern
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [] => acc
    [first, ..rest] => 0 # invalid rest pattern should error
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare CloseSquare OpThinArrow LowerIdent OpenSquare LowerIdent Comma DoubleDot LowerIdent CloseSquare OpThinArrow Int LineComment CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "numbers")
)
  (branch1     (binop_thick_arrow
      (list_literal)
      (block
        (lc "acc")
        (binop_thick_arrow
          (list_literal
            (lc "first")
            (unary_double_dot <unary_op>)
          )
          (num_literal_i32 0)
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match numbers
	[] =>
		acc
		[first, ..rest] => 0
# invalid rest pattern should error
~~~
# EXPECTED
BAD LIST REST PATTERN SYNTAX - list_patterns.md:3:13:3:19
UNDEFINED VARIABLE - list_patterns.md:1:7:1:14
UNDEFINED VARIABLE - list_patterns.md:2:11:2:14
UNUSED VARIABLE - list_patterns.md:3:6:3:11
UNUSED VARIABLE - list_patterns.md:3:15:3:15
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **numbers** in this scope.
Is there an **import** or **exposing** missing up-top?

**list_patterns.md:1:7:1:14:**
```roc
match numbers {
```
      ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **acc** in this scope.
Is there an **import** or **exposing** missing up-top?

**list_patterns.md:2:11:2:14:**
```roc
    [] => acc
```
          ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**list_patterns.md:3:13:3:15:**
```roc
    [first, ..rest] => 0 # invalid rest pattern should error
```
            ^^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 13
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 Num *)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
~~~
# TYPES
~~~roc
first : _a
~~~
