# META
~~~ini
description=Basic record type canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getName : { name: Str, age: U64 } -> Str
getName = |_person| "hello"

main! = |_| getName({name: "luke", age:21})
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "getName")
    (binop_thin_arrow
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
    (lc "getName")
    (lambda
      (body
        (str_literal_big "hello")
      )
      (args
        (lc "_person")
      )
    )
  )
  (lc "main")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
    (apply_lc
      (lc "getName")
      (block
        (binop_colon
          (lc "name")
          (binop_colon
            (tuple_literal
              (str_literal_small "luke")
              (lc "age")
            )
            (num_literal_i32 21)
          )
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
TYPE MISMATCH - type_record_basic.md:6:21:6:43
# PROBLEMS
**Parse Error**
at 6:7 to 6:7

**Unsupported Node**
at 3:11 to 3:41

**Unsupported Node**
at 4:11 to 4:21

**Unsupported Node**
at 6:5 to 6:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "getName")
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
