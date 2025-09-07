# META
~~~ini
description=Basic pattern alternatives with multiple tag patterns
type=file
~~~
# SOURCE
~~~roc
module [kind]

Color : [Red, Green, Blue, Yellow, Orange, Purple]

kind : Color -> Str
kind = |color| match color {
    Red | Green | Blue => "primary"
    Yellow | Orange | Purple => "secondary"
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpBar UpperIdent OpBar UpperIdent OpFatArrow String UpperIdent OpBar UpperIdent OpBar UpperIdent OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "kind")
))
(block
  (binop_colon
    (uc "Color")
    (list_literal
      (uc "Red")
      (uc "Green")
      (uc "Blue")
      (uc "Yellow")
      (uc "Orange")
      (uc "Purple")
    )
  )
  (binop_colon
    (lc "kind")
    (binop_arrow_call
      (uc "Color")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "kind")
    (lambda
      (body
        (match
          (scrutinee             (lc "color")
)
          (branch1             (binop_thick_arrow
              (binop_or
                (binop_or
                  (uc "Red")
                  (uc "Green")
                )
                (uc "Blue")
              )
              (str_literal_big "primary")
            )
)
          (branch2             (binop_thick_arrow
              (binop_or
                (binop_or
                  (uc "Yellow")
                  (uc "Orange")
                )
                (uc "Purple")
              )
              (str_literal_big "secondary")
            )
))
      )
      (args
        (lc "color")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [kind]

Color : [Red, Green, Blue, Yellow, Orange, Purple]
kind : Color -> Str
kind = |color| match color
	(Red || Green) || Blue => "primary"
	(Yellow || Orange) || Purple => "secondary"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**pattern_alternatives_basic.md:7:5:7:23:**
```roc
    Red | Green | Blue => "primary"
```
    ^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**pattern_alternatives_basic.md:8:5:8:29:**
```roc
    Yellow | Orange | Purple => "secondary"
```
    ^^^^^^^^^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**pattern_alternatives_basic.md:6:1:6:5:**
```roc
kind = |color| match color {
```
^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "kind"))
    (type type_14)
  )
  (Stmt.assign
    (pattern (Patt.ident "kind"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 39
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
(var #16 -> #38)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 Str)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 Str)
(var #32 _)
(var #33 _)
(var #34 -> #38)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 fn_pure)
~~~
# TYPES
~~~roc
kind : _arg -> _ret
color : _a
~~~
