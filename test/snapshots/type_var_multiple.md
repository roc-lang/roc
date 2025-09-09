# META
~~~ini
description=Multiple type variables in a single type annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Multiple type variables 'a' and 'b' introduced in annotation
swap : (a, b) -> (b, a)
swap = |pair| {
    (first, second) = pair
    (second, first)
}

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly OpenRound LowerIdent Comma LowerIdent CloseRound OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound CloseCurly BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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
        (str_literal_big "../basic-cli/platform.roc")
        (block)
      )
    )
))
(block
  (binop_colon
    (lc "swap")
    (binop_arrow_call
      (tuple_literal
        (lc "a")
        (lc "b")
      )
      (tuple_literal
        (lc "b")
        (lc "a")
      )
    )
  )
  (binop_equals
    (lc "swap")
    (lambda
      (body
        (block
          (binop_equals
            (tuple_literal
              (lc "first")
              (lc "second")
            )
            (apply_lc
              (lc "pair")
              (tuple_literal
                (lc "second")
                (lc "first")
              )
            )
          )
        )
      )
      (args
        (lc "pair")
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
app [main!] { pf: "../basic-cli/platform.roc" platform [] }

# Multiple type variables 'a' and 'b' introduced in annotation
swap : (a, b) -> (b, a)
swap = |pair| {
	(first, second) = pair((second, first))
}

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**type_var_multiple.md:6:27:7:5:**
```roc
    (first, second) = pair
    (second, first)
```


**SHADOWING**
This definition shadows an existing one.

**type_var_multiple.md:4:1:4:5:**
```roc
swap : (a, b) -> (b, a)
```
^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_multiple.md:5:1:5:5:**
```roc
swap = |pair| {
```
^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_multiple.md:10:1:10:6:**
```roc
main! = |_| {}
```
^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "swap"))
    (type type_14)
  )
  (Stmt.assign
    (pattern (Patt.ident "swap"))
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
; Total type variables: 43
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
(var #16 -> #39)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 -> #25)
(var #21 -> #38)
(var #22 _)
(var #23 _)
(var #24 -> #37)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 -> #39)
(var #29 _)
(var #30 -> #42)
(var #31 _)
(var #32 -> #41)
(var #33 -> #42)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 tuple)
(var #38 fn_pure)
(var #39 fn_pure)
(var #40 _)
(var #41 {})
(var #42 fn_pure)
~~~
# TYPES
~~~roc
second : _c
swap : _arg -> _ret
main : _arg -> {}
pair : _c
first : _c
~~~
