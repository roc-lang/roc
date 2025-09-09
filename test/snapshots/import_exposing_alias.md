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
(block
  (import
    (binop_exposing
      (binop_dot
        (lc "json")
        (uc "Json")
      )
      (list_literal
        (lc "decode")
      )
    )
  )
  (malformed)
  (lc "fromJson")
  (malformed)
  (lc "encode")
  (malformed)
  (lc "toJson")
  (malformed)
  (binop_equals
    (lc "main")
    (block
      (binop_equals
        (lc "data")
        (record_literal
          (binop_colon
            (lc "name")
            (str_literal_small "Bob")
          )
          (binop_colon
            (lc "age")
            (num_literal_i32 25)
          )
        )
      )
      (binop_equals
        (lc "encoded")
        (apply_lc
          (lc "toJson")
          (lc "data")
        )
      )
      (binop_equals
        (lc "decoded")
        (apply_lc
          (lc "fromJson")
          (lc "encoded")
        )
      )
      (lc "decoded")
    )
  )
)
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
	data = { name: "Bob", age: 25 }
	encoded = toJson(data)
	decoded = fromJson(encoded)
	decoded
}
~~~
# EXPECTED
MODULE NOT FOUND - import_exposing_alias.md:3:1:3:65
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


**UNDEFINED VARIABLE**
Nothing is named **fromJson** in this scope.
Is there an **import** or **exposing** missing up-top?

**import_exposing_alias.md:3:38:3:46:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]
```
                                     ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **encode** in this scope.
Is there an **import** or **exposing** missing up-top?

**import_exposing_alias.md:3:48:3:54:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]
```
                                               ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **toJson** in this scope.
Is there an **import** or **exposing** missing up-top?

**import_exposing_alias.md:3:58:3:64:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]
```
                                                         ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **toJson** in this scope.
Is there an **import** or **exposing** missing up-top?

**import_exposing_alias.md:7:12:7:18:**
```roc
	encoded = toJson(data)
```
	          ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **fromJson** in this scope.
Is there an **import** or **exposing** missing up-top?

**import_exposing_alias.md:8:12:8:20:**
```roc
	decoded = fromJson(encoded)
```
	          ^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Expr.malformed)
  (Expr.lookup "fromJson")
  (Expr.malformed)
  (Expr.lookup "encode")
  (Expr.malformed)
  (Expr.lookup "toJson")
  (Expr.malformed)
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.block
      (Stmt.assign
        (pattern (Patt.ident "data"))
        (Expr.record_literal
          (Expr.record_field
            (Expr.malformed)
            (Expr.str_literal_small)
          )
          (Expr.record_field
            (Expr.malformed)
            (Expr.num_literal_i32 25)
          )
        )
      )
      (Stmt.assign
        (pattern (Patt.ident "encoded"))
        (Expr.fn_call)
      )
      (Stmt.assign
        (pattern (Patt.ident "decoded"))
        (Expr.fn_call)
      )
      (Expr.lookup "decoded")
    )
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 48
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
(var #16 -> #37)
(var #17 -> #45)
(var #18 _)
(var #19 Str)
(var #20 _)
(var #21 _)
(var #22 Num *)
(var #23 _)
(var #24 -> #45)
(var #25 _)
(var #26 -> #29)
(var #27 -> #46)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 -> #34)
(var #32 -> #47)
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
(var #44 {})
(var #45 record)
(var #46 fn_pure)
(var #47 fn_pure)
~~~
# TYPES
~~~roc
~~~
