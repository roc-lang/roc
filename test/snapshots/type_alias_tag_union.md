# META
~~~ini
description=Type alias with tag union and type parameters
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LineComment UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LineComment UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent CloseSquare BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

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
  (Stmt.type_anno
    (name node:apply_uc)
    (type list_literal)
  )
  (Stmt.type_anno
    (name "process")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "process"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type list_literal)
  )
  (Stmt.type_anno
    (name "getString")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "getString"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "getNumber")
    (type binop_thin_arrow)
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
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
