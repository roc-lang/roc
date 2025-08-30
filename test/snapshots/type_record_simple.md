# META
~~~ini
description=Simple record type in function annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

get_name : { name: Str, age: U64 } -> Str
get_name = |person| person.name

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main] }

get_name : {name : Str, age : U64} -> Str
get_name = |person| person.name
main! = |_| {  }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "get_name")
    (Expr.binop_thin_arrow
      (Expr.record_literal
        (Expr.binop_colon
          (Expr.lookup "name")
          (Expr.apply_tag)
        )
        (Expr.binop_colon
          (Expr.lookup "age")
          (Expr.apply_tag)
        )
      )
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "get_name")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.not_lookup)
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
get_name : _a
~~~
