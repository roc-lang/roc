# META
~~~ini
description=Example of a nominal tag union import from a package
type=file
~~~
# SOURCE
~~~roc
module [blue]

# import the Color module from styles package as CC
import styles.Color as CC

# instantiating an RGB nominal tab union from the styles.Color module
blue : CC.Color
blue = CC.Color.RGB(0,0,255)
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LineComment KwImport LowerIdent Dot UpperIdent KwAs UpperIdent BlankLine LineComment LowerIdent OpColon UpperIdent Dot UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent Dot UpperIdent OpenRound Int Comma Int Comma Int CloseRound ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "blue")
))
(block
  (import
    (binop_as
      (binop_pipe
        (lc "styles")
        (uc "Color")
      )
      (uc "CC")
    )
  )
  (binop_colon
    (lc "blue")
    (binop_pipe
      (uc "CC")
      (uc "Color")
    )
  )
  (binop_equals
    (lc "blue")
    (apply_anon
      (binop_pipe
        (binop_pipe
          (uc "CC")
          (uc "Color")
        )
        (uc "RGB")
      )
      (tuple_literal
        (num_literal_i32 0)
        (num_literal_i32 0)
        (num_literal_i32 255)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [blue]

# import the Color module from styles package as CC
import styles.Color as CC
# instantiating an RGB nominal tab union from the styles.Color module
blue : CC.Color
blue = CC.Color | RGB((0, 0, 255))
~~~
# EXPECTED
MODULE NOT FOUND - nominal_tag_package_import.md:4:1:4:26
# PROBLEMS
**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**nominal_tag_package_import.md:7:8:7:16:**
```roc
blue : CC.Color
```
       ^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "blue"))
    (type type_11)
  )
  (Stmt.assign
    (pattern (Patt.ident "blue"))
    (Expr.fn_call)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 28
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
(var #13 -> #23)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 -> #27)
(var #19 Num *)
(var #20 Num *)
(var #21 Num *)
(var #22 -> #26)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 tuple)
(var #27 fn_pure)
~~~
# TYPES
~~~roc
blue : _a
~~~
