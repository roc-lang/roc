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
KwModule OpenSquare UpperIdent Comma LowerIdent CloseSquare UpperIdent OpColon UpperIdent LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly String CloseCurly UpperIdent OpColon UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (uc "MyType")
    (uc "UnknownType")
  )
  (binop_colon
    (lc "processValue")
    (binop_thin_arrow
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
    (binop_pipe
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
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.lookup "processValue")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "processValue")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
processValue : _a
~~~
