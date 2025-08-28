# META
~~~ini
description=Effectful function type with record parameter
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly KwImport LowerIdent Dot UpperIdent LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly OpFatArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent Dot LowerIdent CloseRound LowerIdent Dot LowerIdent CloseCurly LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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
      (record_literal
        (binop_colon
          (lc "name")
          (uc "Str")
        )
        (binop_colon
          (lc "age")
          (uc "U64")
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
          (apply_anon
            (binop_pipe
              (uc "Stdout")
              (not_lc "line")
            )
            (binop_pipe
              (lc "person")
              (dot_lc "name")
            )
          )
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
app
{
	pf: "../basic-cli/main.roc" platform [main],
}

import pf.Stdout
printName : { name : Str, age : U64 } => Str
printName = \person -> {
	Stdout.line!(person.name)
	person.name
}
main! = \_ -> {  }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_plus)
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
