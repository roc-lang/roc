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
module [
	Foo, Bar
]


# First declare a type
Foo: U64

# Try to redeclare the same type (should error)
Foo: Str

# Declare another type that uses an undeclared type
Bar: SomeUndeclaredType

# Declare a type that properly uses a declared type
Baz: Foo
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "[]_others")
~~~
# TYPES
~~~roc
~~~
