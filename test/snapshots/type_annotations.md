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
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent Comma Underscore CloseRound LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound LowerIdent OpColon OpenRound UpperIdent Comma UpperIdent OpArrow UpperIdent CloseRound LowerIdent OpBang OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound OpenCurly CloseCurly Comma Underscore CloseRound LowerIdent OpColon UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound CloseRound ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

foo : U64
bar : Thing(_a, _b, _)
baz : (_a, _b, _c)
add_one : U8 -> U16 -> U32
main! : List String -> Result({}, _)
tag_tuple : Value(_a, _b, _c)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "foo")
    (type uc)
  )
  (Stmt.type_anno
    (name "bar")
    (type apply_uc)
  )
  (Stmt.type_anno
    (name "baz")
    (type tuple_literal)
  )
  (Stmt.type_anno
    (name "add_one")
    (type binop_thin_arrow)
  )
  (Stmt.type_anno
    (name "main")
    (type binop_thin_arrow)
  )
  (Stmt.type_anno
    (name "tag_tuple")
    (type apply_uc)
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
