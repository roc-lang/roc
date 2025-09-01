# META
~~~ini
description=Simple where clause with single constraint
type=file
~~~
# SOURCE
~~~roc
module [stringify]

stringify : a -> Str where module(a).to_str : a -> Str
stringify = |value| value.to_str()
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon LowerIdent OpArrow UpperIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent OpenRound CloseRound ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "stringify")
))
~~~
# FORMATTED
~~~roc
module [stringify]

stringify : a -> Str where module(a).to_str : a -> Str
stringify = |value| value.to_str()
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**where_clauses_simple_dispatch.md:4:21:4:33:**
```roc
stringify = |value| value.to_str()
```
                    ^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **value** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_value` to suppress this warning.
The unused variable is declared here:

**where_clauses_simple_dispatch.md:4:14:4:19:**
```roc
stringify = |value| value.to_str()
```
             ^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "stringify")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "stringify"))
    (Expr.lambda (canonicalized))
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
