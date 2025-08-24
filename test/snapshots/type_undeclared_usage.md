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
NO CHANGE
~~~
# EXPECTED
UNDECLARED TYPE - type_undeclared_usage.md:3:10:3:21
MODULE NOT IMPORTED - type_undeclared_usage.md:10:15:10:37
UNDECLARED TYPE - type_undeclared_usage.md:5:16:5:32
UNUSED VARIABLE - type_undeclared_usage.md:6:17:6:22
# PROBLEMS
**Pattern in Expression Context**
at 3:1 to 3:7

**Pattern in Expression Context**
at 3:10 to 3:21

**Unsupported Node**
at 5:16 to 5:39

**Unsupported Node**
at 6:16 to 6:24

**Pattern in Expression Context**
at 10:1 to 10:12

**Unsupported Node**
at 10:15 to 10:25

**Pattern in Expression Context**
at 10:25 to 10:36

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.lookup "processValue")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
