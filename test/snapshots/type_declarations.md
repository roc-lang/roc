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
KwModule OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent Comma LowerIdent Comma LowerIdent OpBang CloseSquare UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound UpperIdent OpColon OpenRound UpperIdent Comma UpperIdent CloseRound UpperIdent OpenRound LowerIdent CloseRound OpColon OpenCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon UpperIdent CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent CloseSquare UpperIdent OpenRound LowerIdent CloseRound OpColon UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (apply_uc
      (uc "Map")
      (tuple_literal
        (lc "a")
        (lc "b")
      )
    )
    (binop_thin_arrow
      (apply_uc
        (uc "List")
        (lc "a")
      )
      (binop_thin_arrow
        (binop_thin_arrow
          (lc "a")
          (lc "b")
        )
        (apply_uc
          (uc "List")
          (lc "b")
        )
      )
    )
  )
  (binop_colon
    (uc "Foo")
    (tuple_literal
      (uc "Bar")
      (uc "Baz")
    )
  )
  (binop_colon
    (apply_uc
      (uc "Some")
      (lc "a")
    )
    (block
      (binop_colon
        (lc "foo")
        (binop_colon
          (tuple_literal
            (apply_uc
              (uc "Ok")
              (lc "a")
            )
            (lc "bar")
          )
          (uc "Something")
        )
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "Maybe")
      (lc "a")
    )
    (list_literal
      (tuple_literal
        (apply_uc
          (uc "Some")
          (lc "a")
        )
        (uc "None")
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "SomeFunc")
      (lc "a")
    )
    (binop_thin_arrow
      (apply_uc
        (uc "Maybe")
        (lc "a")
      )
      (binop_thin_arrow
        (lc "a")
        (apply_uc
          (uc "Maybe")
          (lc "a")
        )
      )
    )
  )
  (binop_colon
    (uc "MyType")
    (uc "U64")
  )
  (binop_colon
    (uc "MyType2")
    (binop_pipe
      (uc "Module")
      (uc "Thingy")
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDECLARED TYPE - type_declarations.md:5:8:5:11
UNDECLARED TYPE - type_declarations.md:5:13:5:16
UNDECLARED TYPE - type_declarations.md:7:19:7:21
UNDECLARED TYPE - type_declarations.md:7:32:7:41
MODULE NOT IMPORTED - type_declarations.md:15:11:15:24
EXPOSED BUT NOT DEFINED - type_declarations.md:1:51:1:56
EXPOSED BUT NOT DEFINED - type_declarations.md:1:42:1:49
# PROBLEMS
**Unsupported Node**
at 3:13 to 3:41

**Unsupported Node**
at 5:16 to 5:17

**Unsupported Node**
at 7:30 to 7:30

**Unsupported Node**
at 9:12 to 10:1

**Unsupported Node**
at 11:15 to 11:38

**Unsupported Node**
at 15:11 to 15:17

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
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "foo")
        (Expr.binop_colon
          (Expr.malformed)
          (Expr.apply_tag)
        )
      )
    )
  )
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
    (Expr.lambda)
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
