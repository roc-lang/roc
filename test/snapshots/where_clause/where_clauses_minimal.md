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
  (Stmt.type_anno
    (name "convert_me")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "convert_me"))
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
