# META
~~~ini
description=Error types should propagate through aliases when underscores are used
type=file
~~~
# SOURCE
~~~roc
module []

BadBase := _

BadDerived := BadBase

value : BadDerived
value = "test"

GoodBase := Str

GoodDerived := GoodBase

goodValue : GoodDerived
goodValue = "test"
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine UpperIdent OpColonEqual Underscore BlankLine UpperIdent OpColonEqual UpperIdent BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign String BlankLine UpperIdent OpColonEqual UpperIdent BlankLine UpperIdent OpColonEqual UpperIdent BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign String ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_colon_equals
    (uc "BadBase")
    (underscore)
  )
  (binop_colon_equals
    (uc "BadDerived")
    (uc "BadBase")
  )
  (binop_colon
    (lc "value")
    (uc "BadDerived")
  )
  (binop_equals
    (lc "value")
    (str_literal_small "test")
  )
  (binop_colon_equals
    (uc "GoodBase")
    (uc "Str")
  )
  (binop_colon_equals
    (uc "GoodDerived")
    (uc "GoodBase")
  )
  (binop_colon
    (lc "goodValue")
    (uc "GoodDerived")
  )
  (binop_equals
    (lc "goodValue")
    (str_literal_small "test")
  )
)
~~~
# FORMATTED
~~~roc
module []

BadBase := _
BadDerived := BadBase
value : BadDerived
value = "test"
GoodBase := Str
GoodDerived := GoodBase
goodValue : GoodDerived
goodValue = "test"
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - underscore_error_propagation.md:1:1:1:1
TYPE MISMATCH - underscore_error_propagation.md:15:13:15:19
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**underscore_error_propagation.md:7:1:7:6:**
```roc
value : BadDerived
```
^^^^^


**SHADOWING**
This definition shadows an existing one.

**underscore_error_propagation.md:8:1:8:6:**
```roc
value = "test"
```
^^^^^


**SHADOWING**
This definition shadows an existing one.

**underscore_error_propagation.md:14:1:14:10:**
```roc
goodValue : GoodDerived
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**underscore_error_propagation.md:15:1:15:10:**
```roc
goodValue = "test"
```
^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.opaque_type)
  (Stmt.opaque_type)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "value"))
    (type type_8)
  )
  (Stmt.assign
    (pattern (Patt.ident "value"))
    (Expr.str_literal_small)
  )
  (Stmt.opaque_type)
  (Stmt.opaque_type)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "goodValue"))
    (type type_20)
  )
  (Stmt.assign
    (pattern (Patt.ident "goodValue"))
    (Expr.str_literal_small)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 26
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
(var #10 -> #11)
(var #11 Str)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 -> #23)
(var #23 Str)
(var #24 _)
(var #25 _)
~~~
# TYPES
~~~roc
value : Str
goodValue : Str
~~~
