# META
~~~ini
description=Import with exposing clause using aliases
type=file
~~~
# SOURCE
~~~roc
module [main]

import json.Json exposing [decode as fromJson, encode as toJson]

main = {
	data = { name: "Bob", age: 25 }
	encoded = toJson(data)
	decoded = fromJson(encoded)
	decoded
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent KwAs LowerIdent Comma LowerIdent KwAs LowerIdent CloseSquare BlankLine LowerIdent OpAssign OpenCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent CloseCurly ~~~
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

import json.Json exposing [decode]
as 
fromJson
, 
encode
as 
toJson
]

main = {
	data = { name : "Bob", age : 25 }
	encoded = toJson(data)
	decoded = fromJson(encoded)
	decoded
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**import_exposing_alias.md:3:35:3:38:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]
```
                                  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**import_exposing_alias.md:3:46:3:48:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]
```
                                             ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**import_exposing_alias.md:3:55:3:58:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]
```
                                                      ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**import_exposing_alias.md:3:64:5:1:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]

main = {
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**import_exposing_alias.md:3:1:3:34:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "fromJson")
  (Expr.malformed)
  (Expr.lookup "encode")
  (Expr.malformed)
  (Expr.lookup "toJson")
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.lookup "main")
    (Expr.block
      (Expr.binop_equals
        (Expr.lookup "data")
        (Expr.record_literal
          (Expr.binop_colon
            (Expr.lookup "name")
            (Expr.str_literal_small)
          )
          (Expr.binop_colon
            (Expr.lookup "age")
            (Expr.num_literal_i32 25)
          )
        )
      )
      (Expr.binop_equals
        (Expr.lookup "encoded")
        (Expr.apply_ident)
      )
      (Expr.binop_equals
        (Expr.lookup "decoded")
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
main : _a
~~~
