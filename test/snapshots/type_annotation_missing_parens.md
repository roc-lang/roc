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
PARSE ERROR - type_annotation_missing_parens.md:4:1:4:1
EXPOSED BUT NOT DEFINED - type_annotation_missing_parens.md:1:9:1:13
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
  (Stmt.type_anno
    (name "nums")
    (type <mutated_tag:160>)
  )
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
