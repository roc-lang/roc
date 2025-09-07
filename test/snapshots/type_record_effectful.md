# META
~~~ini
description=Effectful function type with record parameter
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent BlankLine LowerIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent CloseCurly OpFatArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly UpperIdent Dot LowerIdent OpBang OpenRound LowerIdent Dot LowerIdent CloseRound LowerIdent Dot LowerIdent CloseCurly LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block)
      )
    )
))
(block
  (import
    (binop_dot
      (lc "pf")
      (uc "Stdout")
    )
  )
  (binop_colon
    (lc "printName")
    (binop_thick_arrow
      (record_literal
        (binop_colon
          (lc "name")
          (uc "Str")
        )
        (binop_colon
          (lc "age")
          (uc "U64")
        )
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "printName")
    (lambda
      (body
        (block
          (apply_anon
            (binop_dot
              (uc "Stdout")
              (not_lc "line")
            )
            (binop_dot
              (lc "person")
              (dot_lc "name")
            )
          )
          (binop_dot
            (lc "person")
            (dot_lc "name")
          )
        )
      )
      (args
        (lc "person")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (record_literal)
      )
      (args
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app [main!] { pf: "../basic-cli/main.roc" platform [] }

import pf.Stdout
printName : { name: Str, age: U64 } => Str
printName = |person| {
	Stdout.line!(person..name)
	person..name
}

main! = |_| {}
~~~
# EXPECTED
MODULE NOT FOUND - type_record_effectful.md:3:1:3:17
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_record_effectful.md:5:1:5:10:**
```roc
printName : { name: Str, age: U64 } => Str
```
^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**type_record_effectful.md:7:11:7:17:**
```roc
    Stdout.line!(person.name)
```
          ^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_record_effectful.md:6:1:6:10:**
```roc
printName = |person| {
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_record_effectful.md:10:1:10:6:**
```roc
main! = |_| {}
```
^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "printName"))
    (type type_20)
  )
  (Stmt.assign
    (pattern (Patt.ident "printName"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 49
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 -> #45)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 -> #44)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 -> #45)
(var #36 _)
(var #37 -> #48)
(var #38 _)
(var #39 -> #47)
(var #40 -> #48)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 fn_pure)
(var #45 fn_pure)
(var #46 _)
(var #47 {})
(var #48 fn_pure)
~~~
# TYPES
~~~roc
person : _a
main : _arg -> {}
printName : _arg -> _ret
~~~
