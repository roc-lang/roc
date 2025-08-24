# META
~~~ini
description=Type declaration scope integration - redeclaration and undeclared type errors
type=file
~~~
# SOURCE
~~~roc
module [Foo, Bar]

# First declare a type
Foo : U64

# Try to redeclare the same type (should error)
Foo : Str

# Declare another type that uses an undeclared type
Bar : SomeUndeclaredType

# Declare a type that properly uses a declared type
Baz : Foo
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma UpperIdent CloseSquare UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (uc "Foo")
    (uc "U64")
  )
  (binop_colon
    (uc "Foo")
    (uc "Str")
  )
  (binop_colon
    (uc "Bar")
    (uc "SomeUndeclaredType")
  )
  (binop_colon
    (uc "Baz")
    (uc "Foo")
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
TYPE REDECLARED - type_scope_integration.md:7:1:7:10
UNDECLARED TYPE - type_scope_integration.md:10:7:10:25
# PROBLEMS
**Pattern in Expression Context**
at 4:1 to 4:4

**Pattern in Expression Context**
at 4:7 to 4:10

**Pattern in Expression Context**
at 7:1 to 7:4

**Pattern in Expression Context**
at 7:7 to 7:10

**Pattern in Expression Context**
at 10:1 to 10:4

**Pattern in Expression Context**
at 10:7 to 10:25

**Pattern in Expression Context**
at 13:1 to 13:4

**Pattern in Expression Context**
at 13:7 to 13:10

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
