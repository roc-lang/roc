# META
~~~ini
description=Type alias with tag union and type parameters
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

# Type alias with type parameters that expands to a tag union
MyResult(ok, err) : [Good(ok), Bad(err)]

# Using the type alias
process : MyResult(Str, I32) -> Str
process = |_result| "processed"

# Another type alias with a single parameter
Option(a) : [Some(a), None]

# Using it with different types
getString : Option(Str) -> Str
getString = |_opt| "default"

getNumber : Option(I32) -> I32
getNumber = |_opt| 0

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LineComment UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LineComment UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent CloseSquare BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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


# Type alias with type parameters that expands to a tag union
MyResult((ok, err)) : [Good(ok), Bad(err)]
# Using the type alias
process : MyResult(Str, I32) -> Str
process = |_result| "processed"
# Another type alias with a single parameter
Option(a) : [Some(a), None]
# Using it with different types
getString : Option Str -> Str
getString = |_opt| "default"
getNumber : Option I32 -> I32
getNumber = |_opt| 0
main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **_result** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `__result` to suppress this warning.
The unused variable is declared here:

**type_alias_tag_union.md:8:12:8:19:**
```roc
process = |_result| "processed"
```
           ^^^^^^^


**UNUSED VARIABLE**
Variable **_opt** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `__opt` to suppress this warning.
The unused variable is declared here:

**type_alias_tag_union.md:15:14:15:18:**
```roc
getString = |_opt| "default"
```
             ^^^^


**UNUSED VARIABLE**
Variable **_opt** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `__opt` to suppress this warning.
The unused variable is declared here:

**type_alias_tag_union.md:18:14:18:18:**
```roc
getNumber = |_opt| 0
```
             ^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
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
