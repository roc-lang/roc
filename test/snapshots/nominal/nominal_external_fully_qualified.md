# META
~~~ini
description=Example of external nominal tag union fully qualified name
type=file
~~~
# SOURCE
~~~roc
module [handleResult]

import MyResultModule

handleResult : MyResultModule.MyResultType(Str, I32) -> Str
handleResult = |result| {
    match result {
        MyResultModule.MyResultType.Ok(value) => value
        MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
    }
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine KwImport UpperIdent BlankLine LowerIdent OpColon UpperIdent Dot UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly KwMatch LowerIdent OpenCurly UpperIdent Dot UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound OpThinArrow LowerIdent UpperIdent Dot UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound OpThinArrow String CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "handleResult")
))
(block
  (import
    (uc "MyResultModule")
  )
  (binop_colon
    (lc "handleResult")
    (binop_arrow_call
      (apply_anon
        (binop_dot
          (uc "MyResultModule")
          (uc "MyResultType")
        )
        (tuple_literal
          (uc "Str")
          (uc "I32")
        )
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "handleResult")
    (lambda
      (body
        (block
          (match
            (scrutinee               (lc "result")
))
          (apply_anon
            (binop_dot
              (uc "MyResultType")
              (uc "Ok")
            )
            (lc "value")
          )
          (malformed)
          (lc "value")
          (apply_anon
            (binop_dot
              (binop_dot
                (uc "MyResultModule")
                (uc "MyResultType")
              )
              (uc "Err")
            )
            (lc "code")
          )
          (malformed)
          (str_literal_big "Error: $(code.toStr())")
        )
      )
      (args
        (lc "result")
      )
    )
  )
  (malformed)
)
~~~
# FORMATTED
~~~roc
module [handleResult]

import MyResultModule
handleResult : MyResultModule.MyResultType((Str, I32)) -> Str
handleResult = |result| {
	match result
	MyResultType.Ok(value)
	=>
	value
	MyResultModule.MyResultType.Err(code)
	=>
	"Error: $(code.toStr())"
}

}
~~~
# EXPECTED
MODULE NOT FOUND - nominal_external_fully_qualified.md:3:1:3:22
UNUSED VARIABLE - nominal_external_fully_qualified.md:9:41:9:45
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**nominal_external_fully_qualified.md:8:23:8:24:**
```roc
        MyResultModule.MyResultType.Ok(value) => value
```
                      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_external_fully_qualified.md:8:47:8:50:**
```roc
        MyResultModule.MyResultType.Ok(value) => value
```
                                              ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_external_fully_qualified.md:9:47:9:50:**
```roc
        MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
```
                                              ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_external_fully_qualified.md:11:1:11:2:**
```roc
}
```
^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**nominal_external_fully_qualified.md:5:16:5:53:**
```roc
handleResult : MyResultModule.MyResultType(Str, I32) -> Str
```
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **value** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_external_fully_qualified.md:8:40:8:45:**
```roc
        MyResultModule.MyResultType.Ok(value) => value
```
                                       ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **value** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_external_fully_qualified.md:8:50:8:55:**
```roc
        MyResultModule.MyResultType.Ok(value) => value
```
                                                 ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **code** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_external_fully_qualified.md:9:41:9:45:**
```roc
        MyResultModule.MyResultType.Err(code) => "Error: $(code.toStr())"
```
                                        ^^^^


**SHADOWING**
This definition shadows an existing one.

**nominal_external_fully_qualified.md:6:1:6:13:**
```roc
handleResult = |result| {
```
^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "handleResult"))
    (type type_13)
  )
  (Stmt.assign
    (pattern (Patt.ident "handleResult"))
    (Expr.lambda (canonicalized))
  )
  (Expr.malformed)
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
(var #14 _)
(var #15 -> #49)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 -> #43)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 -> #46)
(var #31 _)
(var #32 -> #47)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 Str)
(var #37 _)
(var #38 -> #49)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 -> #44)
(var #44 fn_pure)
(var #45 _)
(var #46 _)
(var #47 fn_pure)
(var #48 _)
(var #49 fn_pure)
(var #50 _)
~~~
# TYPES
~~~roc
handleResult : _arg -> _ret
result : _a
~~~
