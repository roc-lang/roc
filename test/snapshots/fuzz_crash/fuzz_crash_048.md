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
  (lc "main")
  (unary_not <unary>)
  (apply_uc
    (uc "List")
    (uc "String")
  )
  (malformed malformed:expr_unexpected_token)
  (apply_uc
    (uc "Result")
    (tuple_literal
      (record_literal)
      (underscore)
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
NO CHANGE
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
UNDECLARED TYPE - fuzz_crash_048.md:4:7:4:12
UNDECLARED TYPE - fuzz_crash_048.md:8:14:8:20
UNDECLARED TYPE - fuzz_crash_048.md:9:13:9:18
# PROBLEMS
**Parse Error**
at 8:7 to 8:7

**Parse Error**
at 8:22 to 8:22

**Pattern in Expression Context**
at 3:7 to 3:10

**Unsupported Node**
at 5:15 to 5:16

**Unsupported Node**
at 7:1 to 7:15

**Unsupported Node**
at 8:7 to 8:7

**Unsupported Node**
at 8:22 to 8:22

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "foo")
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.lookup "bar")
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.lookup "biz")
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.lookup "add_one")
    (Expr.malformed)
  )
  (Expr.lookup "main")
  (Expr.malformed)
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.apply_tag)
  (Expr.binop_colon
    (Expr.lookup "tag_tuple")
    (Expr.apply_tag)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_d")
~~~
# TYPES
~~~roc
~~~
