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
