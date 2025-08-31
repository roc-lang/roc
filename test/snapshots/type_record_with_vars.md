# META
~~~ini
description=Record with type variables in function annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

getField : { field: a, other: _b } -> a
getField = |record| record.field

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LowerIdent OpColon OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent CloseCurly OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }


getField : {field : a, other : _b} -> a
getField = |record| record.field
main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **record** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_record` to suppress this warning.
The unused variable is declared here:

**type_record_with_vars.md:4:21:4:27:**
```roc
getField = |record| record.field
```
                    ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
