# META
~~~ini
description=Effectful function type with record parameter
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

import pf.Stdout

printName : { name: Str, age: U64 } => Str
printName = |person| {
    Stdout.line!(person.name)
    person.name
}
main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly KwImport LowerIdent Dot UpperIdent LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly OpFatArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent Dot LowerIdent CloseRound LowerIdent Dot LowerIdent CloseCurly LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (import
    (lc "pf")
    (uc "Stdout")
  )
  (binop_colon
    (lc "printName")
    (binop_thick_arrow
      (block
        (binop_colon
          (lc "name")
          (binop_colon
            (tuple_literal
              (uc "Str")
              (lc "age")
            )
            (uc "U64")
          )
        )
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "printName")
    (lambda
      (body
        (block
          (binop_pipe
            (uc "Stdout")
            (dot_lc "line")
          )
          (unary_not <unary>)
          (binop_pipe
            (lc "person")
            (dot_lc "name")
          )
        )
      )
      (args
        (lc "person")
      )
    )
  )
  (lc "main")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
    (record_literal)
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
MODULE NOT FOUND - type_record_effectful.md:3:1:3:17
# PROBLEMS
**Parse Error**
at 10:7 to 10:7

**Unsupported Node**
at 3:1 to 3:17

**Unsupported Node**
at 5:13 to 5:43

**Unsupported Node**
at 6:13 to 6:22

**Unsupported Node**
at 10:5 to 10:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "printName")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.lookup "main")
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
