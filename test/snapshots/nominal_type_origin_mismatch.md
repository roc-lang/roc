# META
~~~ini
description=Type mismatch showing nominal type origin from different module
type=file
~~~
# SOURCE
~~~roc
module []

import Data exposing [Person]

expectsPerson : Person -> Str
expectsPerson = |p| "Got a person"

main =
    # This will cause a type mismatch
    expectsPerson("not a person")
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine KwImport UpperIdent KwExposing OpenSquare UpperIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LowerIdent OpAssign LineComment LowerIdent OpenRound String CloseRound ~~~
# PARSE
~~~clojure
(module-header)
(block
  (import
    (binop_exposing
      (uc "Data")
      (list_literal
        (uc "Person")
      )
    )
  )
  (binop_colon
    (lc "expectsPerson")
    (binop_arrow_call
      (uc "Person")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "expectsPerson")
    (lambda
      (body
        (str_literal_big "Got a person")
      )
      (args
        (lc "p")
      )
    )
  )
  (binop_equals
    (lc "main")
    (apply_lc
      (lc "expectsPerson")
      (str_literal_big "not a person")
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

import Data exposing [Person]
expectsPerson : Person -> Str
expectsPerson = |p| "Got a person"
main = # This will cause a type mismatch
	expectsPerson("not a person")
~~~
# EXPECTED
MODULE NOT FOUND - nominal_type_origin_mismatch.md:3:1:3:30
UNDECLARED TYPE - nominal_type_origin_mismatch.md:5:17:5:23
UNUSED VARIABLE - nominal_type_origin_mismatch.md:6:18:6:19
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**nominal_type_origin_mismatch.md:5:1:5:14:**
```roc
expectsPerson : Person -> Str
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**nominal_type_origin_mismatch.md:6:1:6:14:**
```roc
expectsPerson = |p| "Got a person"
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**nominal_type_origin_mismatch.md:8:1:8:5:**
```roc
main =
```
^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "expectsPerson"))
    (type type_9)
  )
  (Stmt.assign
    (pattern (Patt.ident "expectsPerson"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.fn_call)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 25
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
(var #11 -> #23)
(var #12 _)
(var #13 Str)
(var #14 -> #23)
(var #15 _)
(var #16 -> #19)
(var #17 -> #24)
(var #18 Str)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 fn_pure)
(var #24 fn_pure)
~~~
# TYPES
~~~roc
p : _a
expectsPerson : _arg -> Str
main : _a
~~~
