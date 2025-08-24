# META
~~~ini
description=Record with special character fields (error cases)
type=expr
~~~
# SOURCE
~~~roc
{
    _privateField: "leading underscore",
    field_: "trailing underscore",
    PascalCase: "pascal",
    kebab-case: "kebab",
    field$special: "dollar",
    field@symbol: "at symbol",
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String Comma UpperIdent OpColon String Comma LowerIdent OpUnaryMinus LowerIdent OpColon String Comma LowerIdent MalformedUnknownToken LowerIdent OpColon String Comma LowerIdent MalformedOpaqueNameWithoutName LowerIdent OpColon String Comma CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "_privateField")
    (tuple_literal
      (binop_colon
        (tuple_literal
          (binop_colon
            (tuple_literal
              (binop_colon
                (tuple_literal
                  (str_literal_big "leading underscore")
                  (lc "field_")
                )
                (str_literal_big "trailing underscore")
              )
              (uc "PascalCase")
            )
            (str_literal_big "pascal")
          )
          (binop_minus
            (lc "kebab")
            (lc "case")
          )
        )
        (str_literal_big "kebab")
      )
      (lc "field")
    )
  )
  (malformed malformed:expr_unexpected_token)
  (binop_colon
    (lc "special")
    (tuple_literal
      (str_literal_big "dollar")
      (lc "field")
    )
  )
  (malformed malformed:expr_unexpected_token)
  (binop_colon
    (lc "symbol")
    (tuple_literal
      (str_literal_big "at symbol")
      (malformed malformed:expr_unexpected_token)
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_different_fields_error.md:2:20:2:21
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:2:21:2:39
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:2:39:2:40
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:2:40:2:41
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_different_fields_error.md:3:13:3:14
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:3:14:3:33
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:3:33:3:34
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:3:34:3:35
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:4:15:4:16
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:4:25:4:26
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:5:15:5:16
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:5:24:5:25
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:6:10:6:11
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_different_fields_error.md:6:20:6:21
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:6:21:6:27
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:6:27:6:28
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:6:28:6:29
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:7:10:7:17
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:7:17:7:18
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:7:30:7:31
MALFORMED TYPE - record_different_fields_error.md:2:20:2:21
MALFORMED TYPE - record_different_fields_error.md:3:13:3:14
UNDEFINED VARIABLE - record_different_fields_error.md:5:5:5:10
UNDEFINED VARIABLE - record_different_fields_error.md:5:11:5:15
UNDEFINED VARIABLE - record_different_fields_error.md:6:5:6:10
MALFORMED TYPE - record_different_fields_error.md:6:20:6:21
UNDEFINED VARIABLE - record_different_fields_error.md:7:5:7:10
# PROBLEMS
**Parse Error**
at 6:10 to 6:10

**Parse Error**
at 7:10 to 7:10

**Parse Error**
at 8:1 to 8:1

**Parse Error**
at 1:1 to 8:2

**Unsupported Node**
at 6:10 to 6:11

**Unsupported Node**
at 6:10 to 6:10

**Unsupported Node**
at 7:10 to 7:11

**Unsupported Node**
at 7:10 to 7:10

**Unsupported Node**
at 1:1 to 1:1

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "_privateField")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "special")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "symbol")
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
