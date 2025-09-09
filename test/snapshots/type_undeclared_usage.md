# META
~~~ini
description=Undeclared type usage should produce error
type=file
~~~
# SOURCE
~~~roc
module [MyType, processValue]

MyType : UnknownType

processValue : UndeclaredResult -> Str
processValue = |value| {
    "processed"
}

AnotherType : SomeModule.MissingType
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent CloseSquare BlankLine UpperIdent OpColon UpperIdent BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly String CloseCurly BlankLine UpperIdent OpColon UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "MyType")

    (lc "processValue")
))
(block
  (binop_colon
    (uc "MyType")
    (uc "UnknownType")
  )
  (binop_colon
    (lc "processValue")
    (binop_arrow_call
      (uc "UndeclaredResult")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "processValue")
    (lambda
      (body
        (block
          (str_literal_big "processed")
        )
      )
      (args
        (lc "value")
      )
    )
  )
  (binop_colon
    (uc "AnotherType")
    (binop_dot
      (uc "SomeModule")
      (uc "MissingType")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [MyType, processValue]

MyType : UnknownType
processValue : UndeclaredResult -> Str
processValue = |value| {
	"processed"
}

AnotherType : SomeModule.MissingType
~~~
# EXPECTED
UNDECLARED TYPE - type_undeclared_usage.md:3:10:3:21
MODULE NOT IMPORTED - type_undeclared_usage.md:10:15:10:37
UNDECLARED TYPE - type_undeclared_usage.md:5:16:5:32
UNUSED VARIABLE - type_undeclared_usage.md:6:17:6:22
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_undeclared_usage.md:6:1:6:13:**
```roc
processValue = |value| {
```
^^^^^^^^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "processValue"))
    (type type_9)
  )
  (Stmt.assign
    (pattern (Patt.ident "processValue"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_alias)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 25
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
(var #11 -> #24)
(var #12 _)
(var #13 Str)
(var #14 _)
(var #15 -> #24)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 fn_pure)
~~~
# TYPES
~~~roc
value : _a
processValue : _arg -> _ret
~~~
