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
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
