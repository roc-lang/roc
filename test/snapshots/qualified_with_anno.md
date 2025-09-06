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


**UNDEFINED VARIABLE**
Nothing is named **MyType.TagA** in this scope.
Is there an **import** or **exposing** missing up-top?

**qualified_with_anno.md:6:9:6:20:**
```roc
value = MyType.TagA
```
        ^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
  (Stmt.type_anno
    (name "value")
    (type <mutated_tag:160>)
  )
  (Stmt.assign
    (pattern (Patt.ident "value"))
    (Expr.module_access
      (Expr.lookup "MyType")
      (Expr.lookup "TagA")
    )
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
