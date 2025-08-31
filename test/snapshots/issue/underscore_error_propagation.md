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
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**underscore_error_propagation.md:3:9:3:11:**
```roc
BadBase := _
```
        ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**underscore_error_propagation.md:5:12:5:14:**
```roc
BadDerived := BadBase
```
           ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**underscore_error_propagation.md:10:10:10:12:**
```roc
GoodBase := Str
```
         ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**underscore_error_propagation.md:12:13:12:15:**
```roc
GoodDerived := GoodBase
```
            ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.type_anno
    (name "value")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "value"))
    (Expr.str_literal_small)
  )
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.type_anno
    (name "goodValue")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "goodValue"))
    (Expr.str_literal_small)
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
