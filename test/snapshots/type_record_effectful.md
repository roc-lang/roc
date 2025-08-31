# META
~~~ini
description=Effectful function type with record parameter
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

import pf.Stdout

printName : { name: Str, age: U64 } => Str
printName = |person| {
    Stdout.line!(person.name)
    person.name
}
main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent BlankLine LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly OpFatArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent Dot LowerIdent CloseRound LowerIdent Dot LowerIdent CloseCurly LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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


import pf.Stdout
printName : { name : Str, age : U64 } => Str
printName = |person| {
	Stdout.line!(person.name)
	person.name
}

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **Stdout.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**type_record_effectful.md:7:5:7:17:**
```roc
    Stdout.line!(person.name)
```
    ^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **person** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_person` to suppress this warning.
The unused variable is declared here:

**type_record_effectful.md:8:5:8:11:**
```roc
    person.name
```
    ^^^^^^


**UNUSED VARIABLE**
Variable **person** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_person` to suppress this warning.
The unused variable is declared here:

**type_record_effectful.md:6:14:6:20:**
```roc
printName = |person| {
```
             ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
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
