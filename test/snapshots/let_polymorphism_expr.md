# META
~~~ini
description=Let-polymorphism with empty list in expression
type=expr
~~~
# SOURCE
~~~roc
match [] {
    empty => { ints: [1, 2, 3], strs: ["a", "b", "c"], empty: empty }
}
~~~
# TOKENS
~~~text
KwMatch OpenSquare CloseSquare OpenCurly LowerIdent OpFatArrow OpenCurly LowerIdent OpColon OpenSquare Int Comma Int Comma Int CloseSquare Comma LowerIdent OpColon OpenSquare String Comma String Comma String CloseSquare Comma LowerIdent OpColon LowerIdent CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (list_literal)
)
  (branch1     (binop_thick_arrow
      (lc "empty")
      (record_literal
        (binop_colon
          (lc "ints")
          (list_literal
            (num_literal_i32 1)
            (num_literal_i32 2)
            (num_literal_i32 3)
          )
        )
        (binop_colon
          (lc "strs")
          (list_literal
            (str_literal_small "a")
            (str_literal_small "b")
            (str_literal_small "c")
          )
        )
        (binop_colon
          (lc "empty")
          (lc "empty")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match []
	empty => { ints : [1, 2, 3], strs : ["a", "b", "c"], empty : empty }
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:11 to 2:13

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
