# META
~~~ini
description=Type variables and values exist in separate namespaces
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Type variable 'elem' introduced in annotation
process : List(elem) -> elem
process = |list| {
    # value identifier named 'elem' is allowed - different namespace from type variable
    elem = 42

    # type variable 'elem' still refers to the function annotation's type parameter
    result : elem
    result = List.first(list) |> Result.withDefault(elem)

    result
}

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LineComment LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpColon LowerIdent LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound OpBar OpGreaterThan UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound BlankLine LowerIdent CloseCurly BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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
    (lc "process")
    (binop_arrow_call
      (apply_uc
        (uc "List")
        (lc "elem")
      )
      (lc "elem")
    )
  )
  (binop_equals
    (lc "process")
    (lambda
      (body
        (block
          (binop_equals
            (lc "elem")
            (num_literal_i32 42)
          )
          (binop_colon
            (lc "result")
            (lc "elem")
          )
          (apply_anon
            (binop_pipe
              (uc "List")
              (dot_lc "first")
            )
            (lc "list")
          )
          (apply_anon
            (binop_pipe
              (malformed)
              (dot_lc "withDefault")
            )
            (lc "elem")
          )
          (lc "result")
        )
      )
      (args
        (lc "list")
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

# Type variable 'elem' introduced in annotation
process : List elem -> elem
process = |list| {
	# value identifier named 'elem' is allowed - different namespace from type variable
	elem = 42
	# type variable 'elem' still refers to the function annotation's type parameter
	result : elem
	List.first(list)
	Result | .withDefault(elem)
	result
}

main! = |_| {}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - type_var_namespace.md:11:31:11:33
UNDEFINED VARIABLE - type_var_namespace.md:11:14:11:24
UNRECOGNIZED SYNTAX - type_var_namespace.md:11:31:11:33
UNDEFINED VARIABLE - type_var_namespace.md:11:34:11:52
TYPE MISMATCH - type_var_namespace.md:5:18:14:2
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_var_namespace.md:11:32:11:34:**
```roc
    result = List.first(list) |> Result.withDefault(elem)
```
                               ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **Result** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_var_namespace.md:11:34:11:40:**
```roc
    result = List.first(list) |> Result.withDefault(elem)
```
                                 ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "process"))
    (type type_12)
  )
  (Stmt.assign
    (pattern (Patt.ident "process"))
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
; Total type variables: 51
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
(var #14 -> #47)
(var #15 _)
(var #16 -> #17)
(var #17 Num *)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 -> #45)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 -> #46)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 -> #47)
(var #37 _)
(var #38 -> #50)
(var #39 _)
(var #40 -> #49)
(var #41 -> #50)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 fn_pure)
(var #46 fn_pure)
(var #47 fn_pure)
(var #48 _)
(var #49 {})
(var #50 fn_pure)
~~~
# TYPES
~~~roc
elem : Num(_size)
process : _arg -> _ret
main : _arg -> {}
list : _a
result : _a
~~~
