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
    (block
      (binop_colon
        (lc "field")
        (binop_colon
          (tuple_literal
            (underscore)
            (lc "other")
          )
          (uc "U32")
        )
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
      (tuple_literal
        (apply_uc
          (uc "Some")
          (underscore)
        )
        (uc "None")
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
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:7:21:7:21
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:13:15:13:15
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:15:18:15:18
# PROBLEMS
**Pattern in Expression Context**
at 3:1 to 3:7

**Pattern in Expression Context**
at 3:10 to 3:11

**Unsupported Node**
at 5:1 to 5:15

**Unsupported Node**
at 7:1 to 7:23

**Unsupported Node**
at 9:1 to 9:38

**Unsupported Node**
at 11:1 to 11:23

**Unsupported Node**
at 13:1 to 13:25

**Unsupported Node**
at 15:1 to 15:26

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
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
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
