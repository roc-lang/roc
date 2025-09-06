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
KwModule OpenSquare CloseSquare BlankLine UpperIdent OpColon Underscore BlankLine UpperIdent OpColonEqual Underscore BlankLine UpperIdent OpColonEqual UpperIdent OpenRound Underscore CloseRound BlankLine UpperIdent OpColonEqual OpenCurly LowerIdent OpColon Underscore Comma LowerIdent OpColon UpperIdent CloseCurly BlankLine UpperIdent OpColonEqual Underscore OpArrow Underscore BlankLine UpperIdent OpColonEqual OpenRound Underscore Comma UpperIdent Comma Underscore CloseRound BlankLine UpperIdent OpColonEqual OpenSquare UpperIdent OpenRound Underscore CloseRound Comma UpperIdent CloseSquare ~~~
# PARSE
~~~clojure
(module-header)
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
    (binop_arrow_call
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
RecordType := {field: _, other: U32}
FunctionType := _ -> _
TupleType := (_, U32, _)
TagType := [Some(_), None]
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
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**underscore_in_type_alias.md:5:11:5:13:**
```roc
OtherType := _
```
          ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**underscore_in_type_alias.md:7:13:7:15:**
```roc
ComplexType := List(_)
```
            ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**underscore_in_type_alias.md:9:12:9:14:**
```roc
RecordType := { field: _, other: U32 }
```
           ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**underscore_in_type_alias.md:11:14:11:16:**
```roc
FunctionType := _ -> _
```
             ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**underscore_in_type_alias.md:13:11:13:13:**
```roc
TupleType := (_, U32, _)
```
          ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**underscore_in_type_alias.md:15:9:15:11:**
```roc
TagType := [Some(_), None]
```
        ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 40
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
~~~
# TYPES
~~~roc
~~~
