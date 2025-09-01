# META
~~~ini
description=Import with exposing clause and usage of exposed items
type=file
~~~
# SOURCE
~~~roc
module [main]

import json.Json exposing [decode, encode]

main = {
    data = { name: "Alice", age: 30 }
    encoded = encode(data)
    decoded = decode(encoded)
    decoded
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent Comma LowerIdent CloseSquare BlankLine LowerIdent OpAssign OpenCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "main")
))
~~~
# FORMATTED
~~~roc
module [main]

import json.Json exposing [decode, encode]

main = {
	data = { name: "Alice", age: 30 }
	encoded = encode(data)
	decoded = decode(encoded)
	decoded
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **encode** in this scope.
Is there an **import** or **exposing** missing up-top?

**import_exposing_basic.md:7:15:7:21:**
```roc
    encoded = encode(data)
```
              ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **decode** in this scope.
Is there an **import** or **exposing** missing up-top?

**import_exposing_basic.md:8:15:8:21:**
```roc
    decoded = decode(encoded)
```
              ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.block
      (Stmt.assign
        (pattern (Patt.ident "data"))
        (Expr.record_literal
          (Expr.binop_colon
            (lc "name")
            (Expr.str_literal_big)
          )
          (Expr.binop_colon
            (lc "age")
            (Expr.num_literal_i32 30)
          )
        )
      )
      (Stmt.assign
        (pattern (Patt.ident "encoded"))
        (Expr.apply_ident)
      )
      (Stmt.assign
        (pattern (Patt.ident "decoded"))
        (Expr.apply_ident)
      )
      (Expr.lookup "decoded")
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
~~~
