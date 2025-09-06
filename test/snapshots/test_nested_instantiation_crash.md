# META
~~~ini
description=Nested instantiation with record field access causing type mismatch
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }
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
KwApp OpenSquare LowerIdent CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly LineComment LineComment BlankLine LowerIdent OpColon LowerIdent OpArrow OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent CloseCurly LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon String CloseCurly BlankLine LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon UpperIdent CloseCurly OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent BlankLine LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpenRound LowerIdent OpenRound LowerIdent CloseRound CloseRound BlankLine LowerIdent OpAssign LowerIdent OpenRound OpenSquare Int CloseSquare CloseRound ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block)
      )
    )
))
(block
  (binop_colon
    (lc "make_record")
    (binop_arrow_call
      (lc "a")
      (record_literal
        (binop_colon
          (lc "value")
          (lc "a")
        )
        (binop_colon
          (lc "tag")
          (uc "Str")
        )
      )
    )
  )
  (binop_equals
    (lc "make_record")
    (lambda
      (body
        (record_literal
          (binop_colon
            (lc "value")
            (lc "x")
          )
          (binop_colon
            (lc "tag")
            (str_literal_small "data")
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
    (binop_arrow_call
      (record_literal
        (binop_colon
          (lc "value")
          (lc "a")
        )
        (binop_colon
          (lc "tag")
          (uc "Str")
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
    (binop_arrow_call
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
app [main] { pf: "../basic-cli/platform.roc" platform [] }

# TODO: if you do this whole thing as an expr block, with `composed` at
# the end instead of `answer =`, it triggers a parser bug!
make_record : a -> {value: a, tag: Str}
make_record = |x| { value: x, tag: "data" }
get_value : {value: a, tag: Str} -> a
get_value = |r| r.value
composed : List a -> Str
composed = |n| get_value(make_record(n))
answer = composed([42])
~~~
# EXPECTED
TYPE MISMATCH - test_nested_instantiation_crash.md:12:16:12:41
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "make_record"))
    (type type_16)
  )
  (Stmt.assign
    (pattern (Patt.ident "make_record"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "get_value"))
    (type type_38)
  )
  (Stmt.assign
    (pattern (Patt.ident "get_value"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "composed"))
    (type type_52)
  )
  (Stmt.assign
    (pattern (Patt.ident "composed"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "answer"))
    (Expr.fn_call)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 80
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
(var #18 -> #72)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 Str)
(var #25 _)
(var #26 -> #71)
(var #27 -> #72)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 -> #74)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 -> #74)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 -> #78)
(var #55 _)
(var #56 -> #77)
(var #57 -> #76)
(var #58 _)
(var #59 _)
(var #60 _)
(var #61 -> #78)
(var #62 _)
(var #63 -> #67)
(var #64 -> #79)
(var #65 Num *)
(var #66 _)
(var #67 _)
(var #68 _)
(var #69 _)
(var #70 _)
(var #71 {})
(var #72 fn_pure)
(var #73 _)
(var #74 fn_pure)
(var #75 _)
(var #76 fn_pure)
(var #77 fn_pure)
(var #78 fn_pure)
(var #79 fn_pure)
~~~
# TYPES
~~~roc
x : _b
get_value : _arg -> _ret
make_record : _arg -> {}
r : _b
composed : _arg -> _ret
answer : _b
n : _b
~~~
