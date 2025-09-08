# META
~~~ini
description=Match expression with tuple destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match coord {
    (Zero, Zero) => "origin"
    (x, Zero) => x
    (Zero, y) => y
    (x, y) => x
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenRound UpperIdent Comma UpperIdent CloseRound OpThinArrow String OpenRound LowerIdent Comma UpperIdent CloseRound OpThinArrow LowerIdent OpenRound UpperIdent Comma LowerIdent CloseRound OpThinArrow LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpThinArrow LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "coord")
)
  (branch1     (binop_thick_arrow
      (tuple_literal
        (uc "Zero")
        (uc "Zero")
      )
      (binop_thick_arrow
        (binop_thick_arrow
          (binop_thick_arrow
            (apply_anon
              (str_literal_big "origin")
              (tuple_literal
                (lc "x")
                (uc "Zero")
              )
            )
            (apply_lc
              (lc "x")
              (tuple_literal
                (uc "Zero")
                (lc "y")
              )
            )
          )
          (apply_lc
            (lc "y")
            (tuple_literal
              (lc "x")
              (lc "y")
            )
          )
        )
        (lc "x")
      )
    )
))
~~~
# FORMATTED
~~~roc
match coord
	(Zero, Zero) => (("origin"((x, Zero)) => x((Zero, y))) => y((x, y))) => x
~~~
# EXPECTED
UNDEFINED VARIABLE - tuple_patterns.md:1:7:1:12
UNUSED VARIABLE - tuple_patterns.md:5:9:5:10
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**tuple_patterns.md:2:29:3:5:**
```roc
    (Zero, Zero) => "origin"
    (x, Zero) => x
```


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**tuple_patterns.md:3:19:4:5:**
```roc
    (x, Zero) => x
    (Zero, y) => y
```


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**tuple_patterns.md:4:19:5:5:**
```roc
    (Zero, y) => y
    (x, y) => x
```


**UNDEFINED VARIABLE**
Nothing is named **coord** in this scope.
Is there an **import** or **exposing** missing up-top?

**tuple_patterns.md:1:7:1:12:**
```roc
match coord {
```
      ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**tuple_patterns.md:2:21:5:11:**
```roc
    (Zero, Zero) => "origin"
    (x, Zero) => x
    (Zero, y) => y
    (x, y) => x
```


**UNDEFINED VARIABLE**
Nothing is named **x** in this scope.
Is there an **import** or **exposing** missing up-top?

**tuple_patterns.md:5:15:5:16:**
```roc
    (x, y) => x
```
              ^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 26
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
~~~
# TYPES
~~~roc
~~~
