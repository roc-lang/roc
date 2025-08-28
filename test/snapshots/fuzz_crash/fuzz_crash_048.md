# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module []

foo : U64
bar : Thing(a, b, _)
biz : (a, b, c)
add_one : (
U8, U16 -> U32)
main! : List(String) -> Result({}, _)
tag_tuple : Value((a, b, c))
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpColon UpperIdent LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent Comma Underscore CloseRound LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound LowerIdent OpColon OpenRound UpperIdent Comma UpperIdent OpArrow UpperIdent CloseRound LowerIdent OpBang OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound OpenCurly CloseCurly Comma Underscore CloseRound LowerIdent OpColon UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "foo")
    (uc "U64")
  )
  (binop_colon
    (lc "bar")
    (apply_uc
      (uc "Thing")
      (tuple_literal
        (lc "a")
        (lc "b")
        (underscore)
      )
    )
  )
  (binop_colon
    (lc "biz")
    (tuple_literal
      (lc "a")
      (lc "b")
      (lc "c")
    )
  )
  (binop_colon
    (lc "add_one")
    (binop_thin_arrow
      (uc "U8")
      (binop_thin_arrow
        (uc "U16")
        (uc "U32")
      )
    )
  )
  (binop_colon
    (not_lc "main")
    (binop_thin_arrow
      (apply_uc
        (uc "List")
        (uc "String")
      )
      (apply_uc
        (uc "Result")
        (tuple_literal
          (record_literal)
          (underscore)
        )
      )
    )
  )
  (binop_colon
    (lc "tag_tuple")
    (apply_uc
      (uc "Value")
      (tuple_literal
        (lc "a")
        (lc "b")
        (lc "c")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

foo : U64
bar : Thing (a, b, _)
biz : (a, b, c)
add_one : U8 -> U16 -> U32
main! : List String -> Result ({  }, _)
tag_tuple : Value (a, b, c)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_d")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
