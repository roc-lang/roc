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
(block
  (binop_colon
    (apply_uc
      (uc "Map")
      (tuple_literal
        (lc "a")
        (lc "b")
      )
    )
    (binop_arrow_call
      (apply_uc
        (uc "List")
        (lc "a")
      )
      (binop_arrow_call
        (binop_arrow_call
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
    (binop_arrow_call
      (apply_uc
        (uc "Maybe")
        (lc "a")
      )
      (binop_arrow_call
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
module [Map, Foo, Some, Maybe, SomeFunc, add_one, main!]

Map((a, b)) : List a -> (a -> b) -> List b
Foo : (Bar, Baz)
Some(a) : {foo: Ok(a), bar: Something}
Maybe(a) : [Some(a), None]
SomeFunc(a) : Maybe a -> a -> Maybe a
MyType : U64
MyType2 : Module.Thingy
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
**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**type_declarations.md:15:11:15:24:**
```roc
MyType2 : Module.Thingy
```
          ^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 74
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
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 _)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 _)
(var #66 _)
(var #67 _)
(var #68 _)
(var #69 _)
(var #70 _)
(var #71 _)
(var #72 _)
(var #73 _)
~~~
# TYPES
~~~roc
~~~
