# META
~~~ini
description=Type alias with tag union and type parameters
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Type alias with type parameters that expands to a tag union
MyResult(ok, err) : [Good(ok), Bad(err)]

# Using the type alias
process : MyResult(Str, I32) -> Str
process = |_result| "processed"

# Another type alias with a single parameter
Option(a) : [Some(a), None]

# Using it with different types
getString : Option(Str) -> Str
getString = |_opt| "default"

getNumber : Option(I32) -> I32
getNumber = |_opt| 0

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LineComment UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent CloseSquare BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block)
      )
    )
))
(block
  (binop_colon
    (apply_uc
      (uc "MyResult")
      (tuple_literal
        (lc "ok")
        (lc "err")
      )
    )
    (list_literal
      (apply_uc
        (uc "Good")
        (lc "ok")
      )
      (apply_uc
        (uc "Bad")
        (lc "err")
      )
    )
  )
  (binop_colon
    (lc "process")
    (binop_arrow_call
      (apply_uc
        (uc "MyResult")
        (tuple_literal
          (uc "Str")
          (uc "I32")
        )
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "process")
    (lambda
      (body
        (str_literal_big "processed")
      )
      (args
        (lc "_result")
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "Option")
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
    (lc "getString")
    (binop_arrow_call
      (apply_uc
        (uc "Option")
        (uc "Str")
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "getString")
    (lambda
      (body
        (str_literal_big "default")
      )
      (args
        (lc "_opt")
      )
    )
  )
  (binop_colon
    (lc "getNumber")
    (binop_arrow_call
      (apply_uc
        (uc "Option")
        (uc "I32")
      )
      (uc "I32")
    )
  )
  (binop_equals
    (lc "getNumber")
    (lambda
      (body
        (num_literal_i32 0)
      )
      (args
        (lc "_opt")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (record_literal)
      )
      (args
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app [main!] { pf: "../basic-cli/main.roc" platform [] }

# Type alias with type parameters that expands to a tag union
MyResult((ok, err)) : [Good(ok), Bad(err)]
# Using the type alias
process : MyResult(Str, I32) -> Str
process = |_result| "processed"
# Another type alias with a single parameter
Option(a) : [Some(a), None]
# Using it with different types
getString : Option Str -> Str
getString = |_opt| "default"
getNumber : Option I32 -> I32
getNumber = |_opt| 0
main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "process"))
    (type type_27)
  )
  (Stmt.assign
    (pattern (Patt.ident "process"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_alias)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "getString"))
    (type type_48)
  )
  (Stmt.assign
    (pattern (Patt.ident "getString"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "getNumber"))
    (type type_60)
  )
  (Stmt.assign
    (pattern (Patt.ident "getNumber"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 82
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
(var #29 -> #74)
(var #30 _)
(var #31 Str)
(var #32 -> #74)
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
(var #50 -> #76)
(var #51 _)
(var #52 Str)
(var #53 -> #76)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 _)
(var #61 _)
(var #62 -> #78)
(var #63 _)
(var #64 Num *)
(var #65 -> #78)
(var #66 _)
(var #67 -> #81)
(var #68 _)
(var #69 -> #80)
(var #70 -> #81)
(var #71 _)
(var #72 _)
(var #73 _)
(var #74 fn_pure)
(var #75 _)
(var #76 fn_pure)
(var #77 _)
(var #78 fn_pure)
(var #79 _)
(var #80 {})
(var #81 fn_pure)
~~~
# TYPES
~~~roc
_opt : _b
process : _arg -> Str
_result : _b
getString : _arg -> Str
main : _arg -> {}
getNumber : _arg -> Num(_size)
~~~
