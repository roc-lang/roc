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
KwModule OpenSquare CloseSquare BlankLine UpperIdent OpColonEqual Underscore BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Int BlankLine UpperIdent OpColonEqual UpperIdent OpenRound Underscore CloseRound BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpenSquare Int Comma Int Comma Int CloseSquare BlankLine UpperIdent OpColonEqual OpenCurly LowerIdent OpColon Underscore Comma LowerIdent OpColon UpperIdent CloseCurly BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly BlankLine UpperIdent OpColonEqual Underscore OpArrow Underscore BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine UpperIdent OpColonEqual OpenRound Underscore Comma UpperIdent CloseRound BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpenRound String Comma Int CloseRound ~~~
# PARSE
~~~clojure
(module-header)
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
    (binop_arrow_call
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
BadRecord := {field: _, other: U32}
baz : BadRecord
baz = { field: "hi", other: 5 }
BadFunction := _ -> _
qux : BadFunction
qux = |x| x
BadTuple := (_, U32)
quux : BadTuple
quux = ("hello", 42)
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:8:17:8:17
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_error_type.md:23:14:23:14
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.opaque_type)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "foo"))
    (type type_5)
  )
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.num_literal_i32 42)
  )
  (Stmt.opaque_type)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "bar"))
    (type type_16)
  )
  (Stmt.assign
    (pattern (Patt.ident "bar"))
    (Expr.list_literal)
  )
  (Stmt.opaque_type)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "baz"))
    (type type_34)
  )
  (Stmt.assign
    (pattern (Patt.ident "baz"))
    (Expr.record_literal
      (Expr.record_field
        (Expr.malformed)
        (Expr.str_literal_small)
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.num_literal_i32 5)
      )
    )
  )
  (Stmt.opaque_type)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "qux"))
    (type type_51)
  )
  (Stmt.assign
    (pattern (Patt.ident "qux"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.opaque_type)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "quux"))
    (type type_64)
  )
  (Stmt.assign
    (pattern (Patt.ident "quux"))
    (Expr.tuple_literal
      (Expr.str_literal_big)
      (Expr.num_literal_i32 42)
    )
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 76
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 -> #8)
(var #8 Num *)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 -> #22)
(var #19 Num *)
(var #20 Num *)
(var #21 Num *)
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
(var #36 -> #72)
(var #37 _)
(var #38 Str)
(var #39 _)
(var #40 _)
(var #41 Num *)
(var #42 _)
(var #43 -> #72)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 -> #74)
(var #54 _)
(var #55 _)
(var #56 -> #74)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 _)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 _)
(var #66 -> #75)
(var #67 Str)
(var #68 Num *)
(var #69 -> #75)
(var #70 _)
(var #71 _)
(var #72 {})
(var #73 _)
(var #74 fn_pure)
(var #75 tuple)
~~~
# TYPES
~~~roc
quux : (Str, Num(_size))
baz : {}
bar : _a
qux : _arg -> _ret
foo : Num(_size)
x : _a
~~~
