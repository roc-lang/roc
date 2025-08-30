# META
~~~ini
description=Nested instantiation with record field access causing type mismatch
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }
# TODO: if you do this whole thing as an expr block, with `composed` at
# the end instead of `answer =`, it triggers a parser bug!

make_record : a -> { value: a, tag: Str }
make_record = |x| { value: x, tag: "data" }

get_value : { value: a, tag: Str } -> a
get_value = |r| r.value

composed : List(a) -> Str
composed = |n| get_value(make_record(n))

answer = composed([42])
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly LowerIdent OpColon LowerIdent OpArrow OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent CloseCurly LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon String CloseCurly LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent CloseCurly OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpenRound LowerIdent OpenRound LowerIdent CloseRound CloseRound LowerIdent OpAssign LowerIdent OpenRound OpenSquare Int CloseSquare CloseRound ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block
          (lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

make_record : a -> {value : a, tag : Str}
make_record = |x| { value : x, tag : "data" }
get_value : {value : a, tag : Str} -> a
get_value = |r| r.value
composed : List a -> Str
composed = |n| get_value(make_record(n))
answer = composed([42])
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "make_record")
    (Expr.binop_thin_arrow
      (Expr.lookup "a")
      (Expr.record_literal
        (Expr.binop_colon
          (Expr.lookup "value")
          (Expr.lookup "a")
        )
        (Expr.binop_colon
          (Expr.lookup "tag")
          (Expr.apply_tag)
        )
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "make_record")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "get_value")
    (Expr.binop_thin_arrow
      (Expr.record_literal
        (Expr.binop_colon
          (Expr.lookup "value")
          (Expr.lookup "a")
        )
        (Expr.binop_colon
          (Expr.lookup "tag")
          (Expr.apply_tag)
        )
      )
      (Expr.lookup "a")
    )
  )
  (Expr.binop_equals
    (Expr.lookup "get_value")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "composed")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "composed")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "answer")
    (Expr.apply_ident)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
make_record : _b
get_value : _b
composed : _b
answer : _b
~~~
