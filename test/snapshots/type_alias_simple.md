# META
~~~ini
description=Simple type alias usage in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

UserId : U64

getUser : UserId -> Str
getUser = |id| if (id > 10) "big" else "small"

main! = |_| getUser(100)
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine UpperIdent OpColon UpperIdent BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwIf OpenRound LowerIdent OpGreaterThan Int CloseRound String KwElse String BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound Int CloseRound ~~~
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
~~~
# FORMATTED
~~~roc
app [main!] { pf: "../basic-cli/main.roc" platform [] }

UserId : U64

getUser : UserId -> Str
getUser = |id| if id > 10 "big" else "small"

main! = |_| getUser(100)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.type_anno
    (name "getUser")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "getUser"))
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
~~~
# TYPES
~~~roc
~~~
