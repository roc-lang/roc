# META
~~~ini
description=Example of a recursive nominal tag union with payload
type=file
~~~
# SOURCE
~~~roc
module [ConsList, empty]

ConsList(a) := [Nil, Node(ConsList(a))]

empty : ConsList(_a)
empty = ConsList.Nil
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColonEqual OpenSquare UpperIdent Comma UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound CloseSquare BlankLine LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "ConsList")

    (lc "empty")
))
~~~
# FORMATTED
~~~roc
module [ConsList, empty]

ConsList(a) := [Nil, Node(ConsList(a))]

empty : ConsList _a
empty = ConsList.Nil
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_tag_recursive_payload.md:3:13:3:15:**
```roc
ConsList(a) := [Nil, Node(ConsList(a))]
```
            ^^


**UNDEFINED VARIABLE**
Nothing is named **ConsList.Nil** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_tag_recursive_payload.md:6:9:6:21:**
```roc
empty = ConsList.Nil
```
        ^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
  (Stmt.type_anno
    (name "empty")
    (type apply_uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "empty"))
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
