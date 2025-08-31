# META
~~~ini
description=Type annotation missing parentheses for type application
type=file
~~~
# SOURCE
~~~roc
module [nums]

nums : List U8
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "nums")
))
~~~
# FORMATTED
~~~roc
module [nums]


nums : List
U8
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**type_annotation_missing_parens.md:3:13:3:15:**
```roc
nums : List U8
```
            ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
