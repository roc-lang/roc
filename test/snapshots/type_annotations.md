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
    (binop_arrow_call
      (uc "U8")
      (binop_arrow_call
        (uc "U16")
        (uc "U32")
      )
    )
  )
  (binop_colon
    (not_lc "main")
    (binop_arrow_call
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
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "foo"))
    (type type_2)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "bar"))
    (type type_10)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "baz"))
    (type type_16)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "add_one"))
    (type type_23)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "main"))
    (type type_34)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "tag_tuple"))
    (type type_42)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 45
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
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
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
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
~~~
# TYPES
~~~roc
baz : _a
tag_tuple : _a
foo : _a
bar : _a
add_one : _a
main : _a
~~~
