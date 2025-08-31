# META
~~~ini
description=Simple record type in function annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

get_name : { name: Str, age: U64 } -> Str
get_name = |person| person.name

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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


get_name : {name : Str, age : U64} -> Str
get_name = |person| person.name
main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **person** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_person` to suppress this warning.
The unused variable is declared here:

**type_record_simple.md:4:21:4:27:**
```roc
get_name = |person| person.name
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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
