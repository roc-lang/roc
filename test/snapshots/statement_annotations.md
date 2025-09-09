# META
~~~ini
description=Inline annotation for statements
type=file
~~~
# SOURCE
~~~roc
module []

addOneU64 = |x| {
  y : U64
  y = x + 1

  y
}

func : val -> val
func = |x| {
  y : val
  y = x

  y
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon UpperIdent LowerIdent OpAssign LowerIdent OpPlus Int BlankLine LowerIdent CloseCurly BlankLine LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent LowerIdent OpAssign LowerIdent BlankLine LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_equals
    (lc "addOneU64")
    (lambda
      (body
        (block
          (binop_colon
            (lc "y")
            (uc "U64")
          )
          (binop_equals
            (lc "y")
            (binop_plus
              (lc "x")
              (num_literal_i32 1)
            )
          )
          (lc "y")
        )
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_colon
    (lc "func")
    (binop_arrow_call
      (lc "val")
      (lc "val")
    )
  )
  (binop_equals
    (lc "func")
    (lambda
      (body
        (block
          (binop_colon
            (lc "y")
            (lc "val")
          )
          (binop_equals
            (lc "y")
            (lc "x")
          )
          (lc "y")
        )
      )
      (args
        (lc "x")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

addOneU64 = |x| {
	y : U64
	y = x + 1
	y
}

func : val -> val
func = |x| {
	y : val
	y = x
	y
}
~~~
# EXPECTED
TYPE MISMATCH - statement_annotations.md:13:7:13:8
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**statement_annotations.md:5:3:5:4:**
```roc
  y = x + 1
```
  ^


**SHADOWING**
This definition shadows an existing one.

**statement_annotations.md:3:1:3:10:**
```roc
addOneU64 = |x| {
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**statement_annotations.md:10:1:10:5:**
```roc
func : val -> val
```
^^^^


**SHADOWING**
This definition shadows an existing one.

**statement_annotations.md:11:9:11:10:**
```roc
func = |x| {
```
        ^


**SHADOWING**
This definition shadows an existing one.

**statement_annotations.md:12:3:12:4:**
```roc
  y : val
```
  ^


**SHADOWING**
This definition shadows an existing one.

**statement_annotations.md:13:3:13:4:**
```roc
  y = x
```
  ^


**SHADOWING**
This definition shadows an existing one.

**statement_annotations.md:11:1:11:5:**
```roc
func = |x| {
```
^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "addOneU64"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "func"))
    (type type_18)
  )
  (Stmt.assign
    (pattern (Patt.ident "func"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 37
(var #0 _)
(var #1 -> #34)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 -> #9)
(var #7 -> #8)
(var #8 -> #9)
(var #9 Num *)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 -> #34)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 -> #36)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 -> #26)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 -> #36)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 fn_pure)
(var #35 _)
(var #36 fn_pure)
~~~
# TYPES
~~~roc
x : _a
addOneU64 : _arg -> _ret
func : _arg -> _ret
y : _a
~~~
