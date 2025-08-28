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
    (record_literal
      (binop_colon
        (lc "foo")
        (apply_uc
          (uc "Ok")
          (lc "a")
        )
      )
      (binop_colon
        (lc "bar")
        (uc "Something")
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "Maybe")
      (lc "a")
    )
    (list_literal
      (apply_uc
        (uc "Some")
        (lc "a")
      )
      (uc "None")
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
module [
	Map,
	Foo,
	Some,
	Maybe,
	SomeFunc,
	add_one,
	main,
]

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
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_c")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
