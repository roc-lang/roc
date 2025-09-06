# META
~~~ini
description=Example of a simple nominal tag union
type=file
~~~
# SOURCE
~~~roc
module [Color, blue]

Color := [Red, Green, Blue]

blue : Color
blue = Color.Blue

yellow : Color
yellow = Color.Yellow
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent CloseSquare BlankLine UpperIdent OpColonEqual OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "Color")

    (lc "blue")
))
(block
  (binop_colon_equals
    (uc "Color")
    (list_literal
      (uc "Red")
      (uc "Green")
      (uc "Blue")
    )
  )
  (binop_colon
    (lc "blue")
    (uc "Color")
  )
  (binop_equals
    (lc "blue")
    (binop_dot
      (uc "Color")
      (uc "Blue")
    )
  )
  (binop_colon
    (lc "yellow")
    (uc "Color")
  )
  (binop_equals
    (lc "yellow")
    (binop_dot
      (uc "Color")
      (uc "Yellow")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [Color, blue]

Color := [Red, Green, Blue]
blue : Color
blue = (Color.Blue)
yellow : Color
yellow = (Color.Yellow)
~~~
# EXPECTED
INVALID NOMINAL TAG - nominal_tag_simple.md:9:10:9:22
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_tag_simple.md:3:7:3:9:**
```roc
Color := [Red, Green, Blue]
```
      ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "blue"))
    (type type_10)
  )
  (Stmt.assign
    (pattern (Patt.ident "blue"))
    (Expr.module_access
      (Expr.tag_no_args)
      (Expr.tag_no_args)
    )
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "yellow"))
    (type type_18)
  )
  (Stmt.assign
    (pattern (Patt.ident "yellow"))
    (Expr.module_access
      (Expr.tag_no_args)
      (Expr.tag_no_args)
    )
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 26
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
(var #12 -> #15)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 -> #23)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
~~~
# TYPES
~~~roc
yellow : _a
blue : _a
~~~
