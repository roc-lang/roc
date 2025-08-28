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
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
