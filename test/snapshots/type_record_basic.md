# META
~~~ini
description=Basic record type canonicalization
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

getName : { name: Str, age: U64 } -> Str
getName = |_person| "hello"

main! = |_| getName({name: "luke", age:21})
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly CloseRound ~~~
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


getName : {name : Str, age : U64} -> Str
getName = |_person| "hello"
main! = |_| getName({ name : "luke", age : 21 })
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **_person** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `__person` to suppress this warning.
The unused variable is declared here:

**type_record_basic.md:4:12:4:19:**
```roc
getName = |_person| "hello"
```
           ^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**type_record_basic.md:6:22:6:34:**
```roc
main! = |_| getName({name: "luke", age:21})
```
                     ^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**type_record_basic.md:6:36:6:42:**
```roc
main! = |_| getName({name: "luke", age:21})
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
