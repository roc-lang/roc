# META
~~~ini
description=Test qualified tag with type annotation
type=file
~~~
# SOURCE
~~~roc
module [value]

MyType := [TagA, TagB]

value : MyType
value = MyType.TagA
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine UpperIdent OpColonEqual OpenSquare UpperIdent Comma UpperIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "value")
))
(block
  (binop_colon_equals
    (uc "MyType")
    (list_literal
      (uc "TagA")
      (uc "TagB")
    )
  )
  (binop_colon
    (lc "value")
    (uc "MyType")
  )
  (binop_equals
    (lc "value")
    (binop_pipe
      (uc "MyType")
      (uc "TagA")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [value]

MyType := [TagA, TagB]
value : MyType
value = MyType.TagA
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**qualified_with_anno.md:3:8:3:10:**
```roc
MyType := [TagA, TagB]
```
       ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "value"))
    (type type_8)
  )
  (Stmt.assign
    (pattern (Patt.ident "value"))
    (Expr.module_access
      (Expr.tag_no_args)
      (Expr.tag_no_args)
    )
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 16
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
(var #10 -> #13)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
~~~
# TYPES
~~~roc
value : _a
~~~
