# META
~~~ini
description=Minimal where clause test
type=file
~~~
# SOURCE
~~~roc
module [convert_me]

convert_me : a -> b
	where
		module(a).convert : a -> b
convert_me = ...
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign TripleDot ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "convert_me")
))
(block
  (binop_colon
    (lc "convert_me")
    (binop_arrow_call
      (binop_where
        (binop_arrow_call
          (lc "a")
          (lc "b")
        )
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
      (lc "b")
    )
  )
  (binop_equals
    (lc "convert_me")
    (ellipsis)
  )
)
~~~
# FORMATTED
~~~roc
module [convert_me]

convert_me : a -> b where module(a).convert : a -> b
convert_me = ...
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "convert_me"))
    (type type_14)
  )
  (Stmt.assign
    (pattern (Patt.ident "convert_me"))
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 21
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
(var #16 -> #20)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
~~~
# TYPES
~~~roc
convert_me : _c
~~~
