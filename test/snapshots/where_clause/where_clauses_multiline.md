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
(module-header
  (exposes
    (lc "process")
))
~~~
# FORMATTED
~~~roc
module [process]

process : a -> b -> (c where module(a).convert : a) -> c, module(b).transform : b -> c
process = ...
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 5:9 to 5:12

**Unsupported Node**
at 6:9 to 6:12

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "process")
    (Expr.binop_thin_arrow
      (Expr.lookup "a")
      (Expr.binop_thin_arrow
        (Expr.lookup "b")
        (Expr.binop_thin_arrow
          (Expr.binop_colon
            (Expr.tuple_literal
              (Expr.binop_thin_arrow
                (Expr.binop_colon
                  (Expr.lookup "c")
                  (Expr.binop_colon
                    (Expr.lambda)
                    (Expr.lookup "a")
                  )
                )
                (Expr.lookup "c")
              )
              (Expr.lambda)
            )
            (Expr.lookup "b")
          )
          (Expr.lookup "c")
        )
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "process")
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_d")
~~~
# TYPES
~~~roc
process : Error
~~~
