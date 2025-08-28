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
(block
  (binop_colon_equals
    (uc "BadType")
    (underscore)
  )
  (binop_colon
    (lc "foo")
    (uc "BadType")
  )
  (binop_equals
    (lc "foo")
    (num_literal_i32 42)
  )
  (binop_colon_equals
    (uc "BadList")
    (apply_uc
      (uc "List")
      (underscore)
    )
  )
  (binop_colon
    (lc "bar")
    (uc "BadList")
  )
  (binop_equals
    (lc "bar")
    (list_literal
      (num_literal_i32 1)
      (num_literal_i32 2)
      (num_literal_i32 3)
    )
  )
  (binop_colon_equals
    (uc "BadRecord")
    (record_literal
      (binop_colon
        (lc "field")
        (underscore)
      )
      (binop_colon
        (lc "other")
        (uc "U32")
      )
    )
  )
  (binop_colon
    (lc "baz")
    (uc "BadRecord")
  )
  (binop_equals
    (lc "baz")
    (record_literal
      (binop_colon
        (lc "field")
        (str_literal_small "hi")
      )
      (binop_colon
        (lc "other")
        (num_literal_i32 5)
      )
    )
  )
  (binop_colon_equals
    (uc "BadFunction")
    (binop_thin_arrow
      (underscore)
      (underscore)
    )
  )
  (binop_colon
    (lc "qux")
    (uc "BadFunction")
  )
  (binop_equals
    (lc "qux")
    (lambda
      (body
        (lc "x")
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_colon_equals
    (uc "BadTuple")
    (tuple_literal
      (underscore)
      (uc "U32")
    )
  )
  (binop_colon
    (lc "quux")
    (uc "BadTuple")
  )
  (binop_equals
    (lc "quux")
    (tuple_literal
      (str_literal_big "hello")
      (num_literal_i32 42)
    )
  )
)
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
qux = \x -> x
BadTuple := (_, U32)
quux : BadTuple
quux = ("hello", 42)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
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
  (Expr.malformed)
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
