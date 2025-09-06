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
(block
  (binop_colon_equals
    (apply_uc
      (uc "ConsList")
      (lc "a")
    )
    (list_literal
      (uc "Nil")
      (apply_uc
        (uc "Node")
        (apply_uc
          (uc "ConsList")
          (lc "a")
        )
      )
    )
  )
  (binop_colon
    (lc "empty")
    (apply_uc
      (uc "ConsList")
      (lc "_a")
    )
  )
  (binop_equals
    (lc "empty")
    (binop_pipe
      (uc "ConsList")
      (uc "Nil")
    )
  )
)
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


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "empty"))
    (type type_17)
  )
  (Stmt.assign
    (pattern (Patt.ident "empty"))
    (Expr.module_access
      (Expr.tag_no_args)
      (Expr.tag_no_args)
    )
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
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 -> #22)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
~~~
# TYPES
~~~roc
empty : _b
~~~
