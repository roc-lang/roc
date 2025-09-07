# META
~~~ini
description=Type variable connection between function annotation and body
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

identity : a -> a
identity = |x| {
    thing : a  # refers to the type var introduced in function type annotation
    thing = x  # refers to the value from the function parameter
    thing
}

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent LineComment LowerIdent OpAssign LowerIdent LineComment LowerIdent CloseCurly BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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
  (binop_colon
    (lc "identity")
    (binop_arrow_call
      (lc "a")
      (lc "a")
    )
  )
  (binop_equals
    (lc "identity")
    (lambda
      (body
        (block
          (binop_colon
            (lc "thing")
            (lc "a")
          )
          (binop_equals
            (lc "thing")
            (lc "x")
          )
          (lc "thing")
        )
      )
      (args
        (lc "x")
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

identity : a -> a
identity = |x| {
	thing : a
	# refers to the type var introduced in function type annotation
	thing = x
	# refers to the value from the function parameter
	thing
}

main! = |_| {}
~~~
# EXPECTED
TYPE MISMATCH - type_var_annotation_body_connection.md:6:13:6:14
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_var_annotation_body_connection.md:3:1:3:9:**
```roc
identity : a -> a
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_annotation_body_connection.md:6:5:6:10:**
```roc
    thing = x  # refers to the value from the function parameter
```
    ^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_annotation_body_connection.md:4:1:4:9:**
```roc
identity = |x| {
```
^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "identity"))
    (type type_10)
  )
  (Stmt.assign
    (pattern (Patt.ident "identity"))
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
; Total type variables: 35
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
(var #12 -> #31)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 -> #18)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 -> #31)
(var #23 _)
(var #24 -> #34)
(var #25 _)
(var #26 -> #33)
(var #27 -> #34)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 fn_pure)
(var #32 _)
(var #33 {})
(var #34 fn_pure)
~~~
# TYPES
~~~roc
thing : _b
identity : _arg -> _ret
x : _b
main : _arg -> {}
~~~
