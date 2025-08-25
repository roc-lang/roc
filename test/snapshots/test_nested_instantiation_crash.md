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
(block
  (binop_colon
    (lc "make_record")
    (binop_thin_arrow
      (lc "a")
      (block
        (binop_colon
          (lc "value")
          (binop_colon
            (tuple_literal
              (lc "a")
              (lc "tag")
            )
            (uc "Str")
          )
        )
      )
    )
  )
  (binop_equals
    (lc "make_record")
    (lambda
      (body
        (block
          (binop_colon
            (lc "value")
            (binop_colon
              (tuple_literal
                (lc "x")
                (lc "tag")
              )
              (str_literal_small "data")
            )
          )
        )
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_colon
    (lc "get_value")
    (binop_thin_arrow
      (block
        (binop_colon
          (lc "value")
          (binop_colon
            (tuple_literal
              (lc "a")
              (lc "tag")
            )
            (uc "Str")
          )
        )
      )
      (lc "a")
    )
  )
  (binop_equals
    (lc "get_value")
    (lambda
      (body
        (binop_pipe
          (lc "r")
          (dot_lc "value")
        )
      )
      (args
        (lc "r")
      )
    )
  )
  (binop_colon
    (lc "composed")
    (binop_thin_arrow
      (apply_uc
        (uc "List")
        (lc "a")
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "composed")
    (lambda
      (body
        (apply_lc
          (lc "get_value")
          (apply_lc
            (lc "make_record")
            (lc "n")
          )
        )
      )
      (args
        (lc "n")
      )
    )
  )
  (binop_equals
    (lc "answer")
    (apply_lc
      (lc "composed")
      (list_literal
        (num_literal_i32 42)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app { pf: ("../basic-cli/platform.roc" platform [main]) }

make_record: (a -> {
	value: ((a, tag): Str)
})
make_record = \x -> {
	value: ((x, tag): "data")
}
get_value: ({
	value: ((a, tag): Str)
} -> a)
get_value = \r -> r | .value
composed: (List(a) -> Str)
composed = \n -> get_value(make_record(n))
answer = composed([42])
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 5:15 to 5:41

**Unsupported Node**
at 6:15 to 6:19

**Unsupported Node**
at 8:13 to 8:40

**Unsupported Node**
at 9:13 to 9:17

**Unsupported Node**
at 11:12 to 11:26

**Unsupported Node**
at 12:12 to 12:16

**Unsupported Node**
at 14:19 to 14:23

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "make_record")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "get_value")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "composed")
    (Expr.malformed)
  )
  (Expr.malformed)
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
