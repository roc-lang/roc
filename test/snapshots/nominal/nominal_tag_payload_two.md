# META
~~~ini
description=Example of a more complex nominal tag union with two payload types
type=file
~~~
# SOURCE
~~~roc
module [MyResult, ok, is_ok]

MyResult(ok, err) := [Ok(ok), Err(err)]

ok : ok -> MyResult(ok, _)
ok = |a| MyResult.Ok(a)

is_ok : MyResult(_ok, _err) -> Bool
is_ok = |result| match result {
    MyResult.Ok(_) => Bool.True
    MyResult.Err(_) => Bool.False
}
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent Comma LowerIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColonEqual OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare BlankLine LowerIdent OpColon LowerIdent OpArrow UpperIdent OpenRound LowerIdent Comma Underscore CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound BlankLine LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent Dot UpperIdent OpenRound Underscore CloseRound OpThinArrow UpperIdent Dot UpperIdent UpperIdent Dot UpperIdent OpenRound Underscore CloseRound OpThinArrow UpperIdent Dot UpperIdent CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "MyResult")

    (lc "ok")

    (lc "is_ok")
))
(block
  (binop_colon_equals
    (apply_uc
      (uc "MyResult")
      (tuple_literal
        (lc "ok")
        (lc "err")
      )
    )
    (list_literal
      (apply_uc
        (uc "Ok")
        (lc "ok")
      )
      (apply_uc
        (uc "Err")
        (lc "err")
      )
    )
  )
  (binop_colon
    (lc "ok")
    (binop_arrow_call
      (lc "ok")
      (apply_uc
        (uc "MyResult")
        (tuple_literal
          (lc "ok")
          (underscore)
        )
      )
    )
  )
  (binop_equals
    (lc "ok")
    (lambda
      (body
        (apply_anon
          (binop_dot
            (uc "MyResult")
            (uc "Ok")
          )
          (lc "a")
        )
      )
      (args
        (lc "a")
      )
    )
  )
  (binop_colon
    (lc "is_ok")
    (binop_arrow_call
      (apply_uc
        (uc "MyResult")
        (tuple_literal
          (lc "_ok")
          (lc "_err")
        )
      )
      (uc "Bool")
    )
  )
  (binop_equals
    (lc "is_ok")
    (lambda
      (body
        (match
          (scrutinee             (lc "result")
))
      )
      (args
        (lc "result")
      )
    )
  )
  (apply_uc
    (uc "Ok")
    (underscore)
  )
  (malformed)
  (binop_dot
    (uc "Bool")
    (uc "True")
  )
  (apply_anon
    (binop_dot
      (uc "MyResult")
      (uc "Err")
    )
    (underscore)
  )
  (malformed)
  (binop_dot
    (uc "Bool")
    (uc "False")
  )
  (malformed)
)
~~~
# FORMATTED
~~~roc
module [MyResult, ok, is_ok]

MyResult((ok, err)) := [Ok(ok), Err(err)]
ok : ok -> MyResult(ok, _)
ok = |a| MyResult.Ok(a)
is_ok : MyResult(_ok, _err) -> Bool
is_ok = |result| match result

Ok(_)
=>
Bool.True
MyResult.Err(_)
=>
Bool.False
}
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**nominal_tag_payload_two.md:10:13:10:14:**
```roc
    MyResult.Ok(_) => Bool.True
```
            ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_tag_payload_two.md:10:20:10:23:**
```roc
    MyResult.Ok(_) => Bool.True
```
                   ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_tag_payload_two.md:11:21:11:24:**
```roc
    MyResult.Err(_) => Bool.False
```
                    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_tag_payload_two.md:12:1:12:2:**
```roc
}
```
^


**SHADOWING**
This definition shadows an existing one.

**nominal_tag_payload_two.md:6:1:6:3:**
```roc
ok = |a| MyResult.Ok(a)
```
^^


**SHADOWING**
This definition shadows an existing one.

**nominal_tag_payload_two.md:9:1:9:6:**
```roc
is_ok = |result| match result {
```
^^^^^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**nominal_tag_payload_two.md:10:17:10:18:**
```roc
    MyResult.Ok(_) => Bool.True
```
                ^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**nominal_tag_payload_two.md:11:18:11:19:**
```roc
    MyResult.Err(_) => Bool.False
```
                 ^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.opaque_type)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "ok"))
    (type type_24)
  )
  (Stmt.assign
    (pattern (Patt.ident "ok"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "is_ok"))
    (type type_42)
  )
  (Stmt.assign
    (pattern (Patt.ident "is_ok"))
    (Expr.lambda (canonicalized))
  )
  (Expr.tag_applied)
  (Expr.malformed)
  (Expr.module_access
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.fn_call)
  (Expr.malformed)
  (Expr.module_access
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 86
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
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 -> #73)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 -> #71)
(var #31 _)
(var #32 _)
(var #33 -> #73)
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
(var #44 -> #75)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 -> #75)
(var #51 _)
(var #52 -> #77)
(var #53 _)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 -> #79)
(var #59 _)
(var #60 _)
(var #61 -> #80)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 _)
(var #66 _)
(var #67 -> #84)
(var #68 _)
(var #69 _)
(var #70 _)
(var #71 -> #72)
(var #72 fn_pure)
(var #73 fn_pure)
(var #74 _)
(var #75 fn_pure)
(var #76 _)
(var #77 fn_pure)
(var #78 _)
(var #79 _)
(var #80 -> #82)
(var #81 _)
(var #82 fn_pure)
(var #83 _)
(var #84 _)
(var #85 _)
~~~
# TYPES
~~~roc
result : _b
ok : _arg -> _ret
is_ok : _arg -> _ret
a : _b
~~~
