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
module [
	MyType, processValue
]


MyType: UnknownType

processValue: (UndeclaredResult -> Str)
processValue = \value -> {
	"processed"
}
AnotherType: SomeModule.MissingType
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 5:16 to 5:39

**Unsupported Node**
at 6:16 to 6:24

**Unsupported Node**
at 10:15 to 10:25

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.lookup "processValue")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "[]_others")
~~~
# TYPES
~~~roc
~~~
