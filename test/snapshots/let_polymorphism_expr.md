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
	empty => { ints: [1, 2, 3], strs: ["a", "b", "c"], empty: empty }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
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
(var #4 Num *)
(var #5 Num *)
(var #6 Num *)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 Str)
(var #11 Str)
(var #12 Str)
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
empty : _a
~~~
