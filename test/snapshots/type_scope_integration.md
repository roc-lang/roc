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
KwModule OpenSquare UpperIdent Comma UpperIdent CloseSquare BlankLine LineComment UpperIdent OpColon UpperIdent BlankLine LineComment UpperIdent OpColon UpperIdent BlankLine LineComment UpperIdent OpColon UpperIdent BlankLine LineComment UpperIdent OpColon UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "Foo")

    (uc "Bar")
))
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
# EXPECTED
TYPE REDECLARED - type_scope_integration.md:7:1:7:10
UNDECLARED TYPE - type_scope_integration.md:10:7:10:25
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 16
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
~~~
# TYPES
~~~roc
~~~
