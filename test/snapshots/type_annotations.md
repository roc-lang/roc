# META
~~~ini
description=Various type annotations
type=file
~~~
# SOURCE
~~~roc
module []

foo : U64
bar : Thing(_a, _b, _)
baz : (_a, _b, _c)
add_one : (U8, U16 -> U32)
main! : List(String) -> Result({}, _)
tag_tuple : Value((_a, _b, _c))
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
        (lc "_a")
        (lc "_b")
        (underscore)
      )
    )
  )
  (binop_colon
    (lc "baz")
    (tuple_literal
      (lc "_a")
      (lc "_b")
      (lc "_c")
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
        (lc "_a")
        (lc "_b")
        (lc "_c")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []


foo: U64
bar: Thing((_a, _b, _))
baz: (_a, _b, _c)
add_one: (U8 -> (U16 -> U32))
main<malformed>!List(String)
<malformed>
Result(({  }, _))
tag_tuple: Value((_a, _b, _c))
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 7:7 to 7:7

**Parse Error**
at 7:22 to 7:22

**Pattern in Expression Context**
at 4:21 to 4:22

**Unsupported Node**
at 5:18 to 5:19

**Unsupported Node**
at 6:12 to 6:26

**Unsupported Node**
at 7:7 to 7:7

**Unsupported Node**
at 7:22 to 7:22

**Pattern in Expression Context**
at 7:36 to 7:37

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "foo")
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.lookup "bar")
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.lookup "baz")
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.lookup "add_one")
    (Expr.malformed)
  )
  (Expr.lookup "main")
  (Expr.unary_not)
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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
