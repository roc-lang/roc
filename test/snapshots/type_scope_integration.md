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
  (Stmt.type_anno
    (name node:uc)
    (type uc)
  )
  (Stmt.type_anno
    (name node:uc)
    (type uc)
  )
  (Stmt.type_anno
    (name node:uc)
    (type uc)
  )
  (Stmt.type_anno
    (name node:uc)
    (type uc)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
