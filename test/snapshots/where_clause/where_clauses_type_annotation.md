# META
~~~ini
description=Simple type annotation with where clause
type=file
~~~
# SOURCE
~~~roc
module [convert]

convert : a -> b where module(a).to_b : a -> b
convert = |a| a.to_b()
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent OpenRound CloseRound ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "convert")
))
~~~
# FORMATTED
~~~roc
module [convert]

convert : a -> b where module(a).to_b : a -> b
convert = |a| a.to_b()
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**where_clauses_type_annotation.md:4:15:4:21:**
```roc
convert = |a| a.to_b()
```
              ^^^^^^


**UNUSED VARIABLE**
Variable **a** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_a` to suppress this warning.
The unused variable is declared here:

**where_clauses_type_annotation.md:4:12:4:13:**
```roc
convert = |a| a.to_b()
```
           ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "convert")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "convert"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
