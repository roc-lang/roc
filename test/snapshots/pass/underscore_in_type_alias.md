# META
~~~ini
description=Type aliases should not contain underscores (_) as they represent 'I don't care' types, which doesn't make sense when declaring a type.
type=file
~~~
# SOURCE
~~~roc
module []

MyType : _

OtherType := _

ComplexType := List(_)

RecordType := { field: _, other: U32 }

FunctionType := _ -> _

TupleType := (_, U32, _)

TagType := [Some(_), None]
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare UpperIdent OpColon Underscore UpperIdent OpColonEqual Underscore UpperIdent OpColonEqual UpperIdent OpenRound Underscore CloseRound UpperIdent OpColonEqual OpenCurly LowerIdent OpColon Underscore Comma LowerIdent OpColon UpperIdent CloseCurly UpperIdent OpColonEqual Underscore OpArrow Underscore UpperIdent OpColonEqual OpenRound Underscore Comma UpperIdent Comma Underscore CloseRound UpperIdent OpColonEqual OpenSquare UpperIdent OpenRound Underscore CloseRound Comma UpperIdent CloseSquare ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (uc "MyType")
    (underscore)
  )
  (binop_colon_equals
    (uc "OtherType")
    (underscore)
  )
  (binop_colon_equals
    (uc "ComplexType")
    (apply_uc
      (uc "List")
      (underscore)
    )
  )
  (binop_colon_equals
    (uc "RecordType")
    (record_literal
      (binop_colon
        (lc "field")
        (underscore)
      )
      (binop_colon
        (lc "other")
        (uc "U32")
      )
    )
  )
  (binop_colon_equals
    (uc "FunctionType")
    (binop_thin_arrow
      (underscore)
      (underscore)
    )
  )
  (binop_colon_equals
    (uc "TupleType")
    (tuple_literal
      (underscore)
      (uc "U32")
      (underscore)
    )
  )
  (binop_colon_equals
    (uc "TagType")
    (list_literal
      (apply_uc
        (uc "Some")
        (underscore)
      )
      (uc "None")
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

MyType : _
OtherType := _
ComplexType := List _
RecordType := {field : _, other : U32}
FunctionType := _ -> _
TupleType := (_, U32, _)
TagType := [Some(_), None]
~~~
# EXPECTED
NIL
# PROBLEMS
**Pattern in Expression Context**
at 3:10 to 3:11

**Pattern in Expression Context**
at 5:14 to 5:15

**Pattern in Expression Context**
at 7:21 to 7:22

**Pattern in Expression Context**
at 9:24 to 9:25

**Pattern in Expression Context**
at 11:17 to 11:18

**Pattern in Expression Context**
at 11:22 to 11:23

**Pattern in Expression Context**
at 13:15 to 13:16

**Pattern in Expression Context**
at 13:23 to 13:24

**Pattern in Expression Context**
at 15:18 to 15:19

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "field")
        (Expr.malformed)
      )
      (Expr.binop_colon
        (Expr.lookup "other")
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.binop_thin_arrow)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.tuple_literal)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.list_literal)
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
