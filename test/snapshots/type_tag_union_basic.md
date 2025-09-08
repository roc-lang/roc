# META
~~~ini
description=Basic tag union type canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

process : [Some(Str), None] -> Str
process = |maybe| "result"

is_ok_ret_unqualified_bool : [Ok(_ok), Err(_err)] -> Bool
is_ok_ret_unqualified_bool = |result| match result {
    Ok(_) => True
    Err(_) => False
}

is_ok_ret_bool : [Ok(_ok2), Err(_err2)] -> Bool
is_ok_ret_bool = |result| match result {
    Ok(_) => Bool.True
    Err(_) => Bool.False
}

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon OpenSquare UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent CloseSquare OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LowerIdent OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound Underscore CloseRound OpThinArrow UpperIdent UpperIdent OpenRound Underscore CloseRound OpThinArrow UpperIdent CloseCurly BlankLine LowerIdent OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound Underscore CloseRound OpThinArrow UpperIdent Dot UpperIdent UpperIdent OpenRound Underscore CloseRound OpThinArrow UpperIdent Dot UpperIdent CloseCurly BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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
    (lc "process")
    (binop_arrow_call
      (list_literal
        (apply_uc
          (uc "Some")
          (uc "Str")
        )
        (uc "None")
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "process")
    (lambda
      (body
        (str_literal_big "result")
      )
      (args
        (lc "maybe")
      )
    )
  )
  (binop_colon
    (lc "is_ok_ret_unqualified_bool")
    (binop_arrow_call
      (list_literal
        (apply_uc
          (uc "Ok")
          (lc "_ok")
        )
        (apply_uc
          (uc "Err")
          (lc "_err")
        )
      )
      (uc "Bool")
    )
  )
  (binop_equals
    (lc "is_ok_ret_unqualified_bool")
    (lambda
      (body
        (match
          (scrutinee             (lc "result")
)
          (branch1             (binop_thick_arrow
              (apply_uc
                (uc "Ok")
                (underscore)
              )
              (malformed)
            )
)
          (branch2             (binop_thick_arrow
              (apply_uc
                (uc "Err")
                (underscore)
              )
              (malformed)
            )
))
      )
      (args
        (lc "result")
      )
    )
  )
  (binop_colon
    (lc "is_ok_ret_bool")
    (binop_arrow_call
      (list_literal
        (apply_uc
          (uc "Ok")
          (lc "_ok2")
        )
        (apply_uc
          (uc "Err")
          (lc "_err2")
        )
      )
      (uc "Bool")
    )
  )
  (binop_equals
    (lc "is_ok_ret_bool")
    (lambda
      (body
        (match
          (scrutinee             (lc "result")
)
          (branch1             (binop_thick_arrow
              (apply_uc
                (uc "Ok")
                (underscore)
              )
              (malformed)
            )
))
      )
      (args
        (lc "result")
      )
    )
  )
  (apply_uc
    (uc "Err")
    (underscore)
  )
  (malformed)
  (binop_dot
    (uc "Bool")
    (uc "False")
  )
  (malformed)
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

process : [Some(Str), None] -> Str
process = |maybe| "result"
is_ok_ret_unqualified_bool : [Ok(_ok), Err(_err)] -> Bool
is_ok_ret_unqualified_bool = |result| match result
	Ok(_) => True
	Err(_) => False

is_ok_ret_bool : [Ok(_ok2), Err(_err2)] -> Bool
is_ok_ret_bool = |result| match result
	Ok(_) => Bool

Err(_)
=>
Bool.False
}

main! = |_| {}
~~~
# EXPECTED
UNUSED VARIABLE - type_tag_union_basic.md:4:12:4:17
INCOMPATIBLE MATCH PATTERNS - type_tag_union_basic.md:7:39:7:39
INCOMPATIBLE MATCH PATTERNS - type_tag_union_basic.md:13:27:13:27
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **True
    ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_tag_union_basic.md:8:14:9:5:**
```roc
    Ok(_) => True
    Err(_) => False
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **False
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_tag_union_basic.md:9:15:10:1:**
```roc
    Err(_) => False
}
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **Bool** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_tag_union_basic.md:14:14:14:18:**
```roc
    Ok(_) => Bool.True
```
             ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **.** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**type_tag_union_basic.md:14:18:14:19:**
```roc
    Ok(_) => Bool.True
```
                 ^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**type_tag_union_basic.md:14:19:15:5:**
```roc
    Ok(_) => Bool.True
    Err(_) => Bool.False
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_tag_union_basic.md:15:12:15:15:**
```roc
    Err(_) => Bool.False
```
           ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**type_tag_union_basic.md:16:1:18:1:**
```roc
}

main! = |_| {}
```


**SHADOWING**
This definition shadows an existing one.

**type_tag_union_basic.md:3:1:3:8:**
```roc
process : [Some(Str), None] -> Str
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_tag_union_basic.md:4:1:4:8:**
```roc
process = |maybe| "result"
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_tag_union_basic.md:6:1:6:27:**
```roc
is_ok_ret_unqualified_bool : [Ok(_ok), Err(_err)] -> Bool
```
^^^^^^^^^^^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_tag_union_basic.md:7:1:7:27:**
```roc
is_ok_ret_unqualified_bool = |result| match result {
```
^^^^^^^^^^^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_tag_union_basic.md:12:1:12:15:**
```roc
is_ok_ret_bool : [Ok(_ok2), Err(_err2)] -> Bool
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_tag_union_basic.md:13:19:13:25:**
```roc
is_ok_ret_bool = |result| match result {
```
                  ^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_tag_union_basic.md:13:1:13:15:**
```roc
is_ok_ret_bool = |result| match result {
```
^^^^^^^^^^^^^^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**type_tag_union_basic.md:15:9:15:10:**
```roc
    Err(_) => Bool.False
```
        ^


**SHADOWING**
This definition shadows an existing one.

**type_tag_union_basic.md:18:1:18:6:**
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
    (pattern (Patt.ident "process"))
    (type type_14)
  )
  (Stmt.assign
    (pattern (Patt.ident "process"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "is_ok_ret_unqualified_bool"))
    (type type_30)
  )
  (Stmt.assign
    (pattern (Patt.ident "is_ok_ret_unqualified_bool"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "is_ok_ret_bool"))
    (type type_57)
  )
  (Stmt.assign
    (pattern (Patt.ident "is_ok_ret_bool"))
    (Expr.lambda (canonicalized))
  )
  (Expr.tag_applied)
  (Expr.malformed)
  (Expr.module_access
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.malformed)
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 100
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
(var #16 -> #87)
(var #17 _)
(var #18 Str)
(var #19 -> #87)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 -> #89)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 -> #89)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 -> #91)
(var #60 _)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 _)
(var #66 _)
(var #67 _)
(var #68 _)
(var #69 _)
(var #70 -> #91)
(var #71 _)
(var #72 -> #93)
(var #73 _)
(var #74 _)
(var #75 _)
(var #76 _)
(var #77 _)
(var #78 -> #95)
(var #79 _)
(var #80 -> #99)
(var #81 _)
(var #82 -> #98)
(var #83 -> #99)
(var #84 _)
(var #85 _)
(var #86 _)
(var #87 fn_pure)
(var #88 _)
(var #89 fn_pure)
(var #90 _)
(var #91 fn_pure)
(var #92 _)
(var #93 fn_pure)
(var #94 _)
(var #95 _)
(var #96 _)
(var #97 _)
(var #98 {})
(var #99 fn_pure)
~~~
# TYPES
~~~roc
is_ok_ret_unqualified_bool : _arg -> _ret
is_ok_ret_bool : _arg -> _ret
process : _arg -> Str
main : _arg -> {}
maybe : _a
result : _a
~~~
