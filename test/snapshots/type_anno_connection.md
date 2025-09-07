# META
~~~ini
description=Type annotation connection to definitions
type=file
~~~
# SOURCE
~~~roc
module [add_one, my_number]

add_one : U64 -> U64
add_one = |x| x + 1

my_number : U64
my_number = add_one(42)
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign LowerIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "add_one")

    (lc "my_number")
))
(block
  (binop_colon
    (lc "add_one")
    (binop_arrow_call
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "add_one")
    (lambda
      (body
        (binop_plus
          (lc "x")
          (num_literal_i32 1)
        )
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_colon
    (lc "my_number")
    (uc "U64")
  )
  (binop_equals
    (lc "my_number")
    (apply_lc
      (lc "add_one")
      (num_literal_i32 42)
    )
  )
)
~~~
# FORMATTED
~~~roc
module [add_one, my_number]

add_one : U64 -> U64
add_one = |x| x + 1
my_number : U64
my_number = add_one(42)
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_anno_connection.md:4:1:4:8:**
```roc
add_one = |x| x + 1
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_anno_connection.md:7:1:7:10:**
```roc
my_number = add_one(42)
```
^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "add_one"))
    (type type_6)
  )
  (Stmt.assign
    (pattern (Patt.ident "add_one"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "my_number"))
    (type type_16)
  )
  (Stmt.assign
    (pattern (Patt.ident "my_number"))
    (Expr.fn_call)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 27
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 -> #25)
(var #9 _)
(var #10 -> #11)
(var #11 -> #12)
(var #12 Num *)
(var #13 -> #25)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 -> #21)
(var #19 -> #26)
(var #20 Num *)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 fn_pure)
(var #26 fn_pure)
~~~
# TYPES
~~~roc
add_one : _arg -> Num(_size)
x : _a
my_number : _a
~~~
