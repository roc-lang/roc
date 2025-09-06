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
UNDECLARED TYPE - type_annotations.md:4:7:4:12
UNDECLARED TYPE - type_annotations.md:7:14:7:20
UNDECLARED TYPE - type_annotations.md:8:13:8:18
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "foo")
    (type <mutated_tag:160>)
  )
  (Stmt.type_anno
    (name "bar")
    (type <mutated_tag:165>)
  )
  (Stmt.type_anno
    (name "baz")
    (type <mutated_tag:163>)
  )
  (Stmt.type_anno
    (name "add_one")
    (type <mutated_tag:161>)
  )
  (Stmt.type_anno
    (name "main")
    (type <mutated_tag:161>)
  )
  (Stmt.type_anno
    (name "tag_tuple")
    (type <mutated_tag:165>)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
