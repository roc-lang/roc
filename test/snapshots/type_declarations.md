# META
~~~ini
description=Various type declarations
type=file
~~~
# SOURCE
~~~roc
module [Map, Foo, Some, Maybe, SomeFunc, add_one, main!]

Map(a, b) : List(a), (a -> b) -> List(b)

Foo : (Bar, Baz)

Some(a) : { foo : Ok(a), bar : Something }

Maybe(a) : [Some(a), None]

SomeFunc(a) : Maybe(a), a -> Maybe(a)

MyType : U64

MyType2 : Module.Thingy
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent Comma LowerIdent Comma LowerIdent OpBang CloseSquare BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound BlankLine UpperIdent OpColon OpenRound UpperIdent Comma UpperIdent CloseRound BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon UpperIdent CloseCurly BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound BlankLine UpperIdent OpColon UpperIdent BlankLine UpperIdent OpColon UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "Map")

    (uc "Foo")

    (uc "Some")

    (uc "Maybe")

    (uc "SomeFunc")

    (lc "add_one")

    (not_lc "main")
))
~~~
# FORMATTED
~~~roc
module [Map, Foo, Some, Maybe, SomeFunc, add_one, main!]

Map((a, b)) : List a -> (a -> b) -> List b

Foo : (Bar, Baz)

Some(a) : {foo : Ok a, bar : Something}

Maybe(a) : [Some(a), None]

SomeFunc(a) : Maybe a -> a -> Maybe a

MyType : U64

MyType2 : Module.Thingy
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name node:apply_uc)
    (type binop_thin_arrow)
  )
  (Stmt.type_anno
    (name node:uc)
    (type tuple_literal)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type record_literal)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type list_literal)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type binop_thin_arrow)
  )
  (Stmt.type_anno
    (name node:uc)
    (type uc)
  )
  (Stmt.type_anno
    (name node:uc)
    (type binop_pipe)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
