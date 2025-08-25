# META
~~~ini
description=Where clause with multiline constraints
type=file
~~~
# SOURCE
~~~roc
module [process]

process : a, b -> c
	where
		module(a).convert : a -> c,
		module(b).transform : b -> c
process = ...
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign TripleDot ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "process")
    (binop_thin_arrow
      (lc "a")
      (binop_thin_arrow
        (lc "b")
        (binop_thin_arrow
          (binop_colon
            (tuple_literal
              (binop_thin_arrow
                (binop_where
                  (lc "c")
                  (binop_colon
                    (binop_pipe
                      (apply_module
                        (lc "a")
                      )
                      (dot_lc "convert")
                    )
                    (lc "a")
                  )
                )
                (lc "c")
              )
              (binop_pipe
                (apply_module
                  (lc "b")
                )
                (dot_lc "transform")
              )
            )
            (lc "b")
          )
          (lc "c")
        )
      )
    )
  )
  (binop_equals
    (lc "process")
    (ellipsis)
  )
)
~~~
# FORMATTED
~~~roc
module [
	process,
]

process: (a -> (b -> (((c where a() | .convert: a) -> c, b() | .transform): b -> c)))
process = ...
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:11 to 6:31

**Unsupported Node**
at 7:11 to 7:14

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "process")
    (Expr.malformed)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
