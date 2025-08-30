# META
~~~ini
description=Type declarations with underscores should become error types that fail unification
type=file
~~~
# SOURCE
~~~roc
module []

BadType := _

foo : BadType
foo = 42

BadList := List(_)

bar : BadList
bar = [1, 2, 3]

BadRecord := { field: _, other: U32 }

baz : BadRecord
baz = { field: "hi", other: 5 }

BadFunction := _ -> _

qux : BadFunction
qux = |x| x

BadTuple := (_, U32)

quux : BadTuple
quux = ("hello", 42)
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare UpperIdent OpColonEqual Underscore LowerIdent OpColon UpperIdent LowerIdent OpAssign Int UpperIdent OpColonEqual UpperIdent OpenRound Underscore CloseRound LowerIdent OpColon UpperIdent LowerIdent OpAssign OpenSquare Int Comma Int Comma Int CloseSquare UpperIdent OpColonEqual OpenCurly LowerIdent OpColon Underscore Comma LowerIdent OpColon UpperIdent CloseCurly LowerIdent OpColon UpperIdent LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly UpperIdent OpColonEqual Underscore OpArrow Underscore LowerIdent OpColon UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent UpperIdent OpColonEqual OpenRound Underscore Comma UpperIdent CloseRound LowerIdent OpColon UpperIdent LowerIdent OpAssign OpenRound String Comma Int CloseRound ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

BadType := _
foo : BadType
foo = 42
BadList := List _
bar : BadList
bar = [1, 2, 3]
BadRecord := {field : _, other : U32}
baz : BadRecord
baz = { field : "hi", other : 5 }
BadFunction := _ -> _
qux : BadFunction
qux = |x| x
BadTuple := (_, U32)
quux : BadTuple
quux = ("hello", 42)
~~~
# EXPECTED
NIL
# PROBLEMS
**Pattern in Expression Context**
at 3:12 to 3:13

**Pattern in Expression Context**
at 8:17 to 8:18

**Pattern in Expression Context**
at 13:23 to 13:24

**Pattern in Expression Context**
at 18:16 to 18:17

**Pattern in Expression Context**
at 18:21 to 18:22

**Pattern in Expression Context**
at 23:14 to 23:15

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.lookup "foo")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "foo")
    (Expr.num_literal_i32 42)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.lookup "bar")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "bar")
    (Expr.list_literal)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "field")
        (Expr.malformed)
      )
      (Expr.binop_colon
        (Expr.lookup "other")
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_colon
    (Expr.lookup "baz")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "baz")
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "field")
        (Expr.str_literal_small)
      )
      (Expr.binop_colon
        (Expr.lookup "other")
        (Expr.num_literal_i32 5)
      )
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.binop_thin_arrow
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Expr.binop_colon
    (Expr.lookup "qux")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "qux")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.tuple_literal
      (Expr.malformed)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_colon
    (Expr.lookup "quux")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "quux")
    (Expr.tuple_literal
      (Expr.str_literal_big)
      (Expr.num_literal_i32 42)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
foo : Num(_size)
bar : List(_elem)
baz : {}
qux : _a
quux : _a
~~~
