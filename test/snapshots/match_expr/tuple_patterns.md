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
KwMatch LowerIdent OpenCurly OpenRound UpperIdent Comma UpperIdent CloseRound OpFatArrow String OpenRound LowerIdent Comma UpperIdent CloseRound OpFatArrow LowerIdent OpenRound UpperIdent Comma LowerIdent CloseRound OpFatArrow LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpFatArrow LowerIdent CloseCurly ~~~
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
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**tuple_patterns.md:2:5:5:16:**
```roc
    (Zero, Zero) => "origin"
    (x, Zero) => x
    (Zero, y) => y
    (x, y) => x
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
