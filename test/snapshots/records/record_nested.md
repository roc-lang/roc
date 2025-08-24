# META
~~~ini
description=Nested record creation
type=expr
~~~
# SOURCE
~~~roc
{
    person: { name: "Alice", age: 30 },
    address: {
        street: "123 Main St",
        city: "Springfield",
        coordinates: { lat: 42.1234, lng: -71.5678 },
    },
    contact: {
        email: "alice@example.com",
        phone: { home: "555-1234", work: "555-5678" },
    },
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly Comma LowerIdent OpColon OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String Comma LowerIdent OpColon OpenCurly LowerIdent OpColon Float Comma LowerIdent OpColon OpUnaryMinus Float CloseCurly Comma CloseCurly Comma LowerIdent OpColon OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String CloseCurly Comma CloseCurly Comma CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "person")
    (binop_colon
      (tuple_literal
        (block
          (binop_colon
            (lc "name")
            (binop_colon
              (tuple_literal
                (str_literal_big "Alice")
                (lc "age")
              )
              (num_literal_i32 30)
            )
          )
        )
        (lc "address")
      )
      (block
        (binop_colon
          (lc "street")
          (binop_colon
            (tuple_literal
              (binop_colon
                (tuple_literal
                  (binop_colon
                    (tuple_literal
                      (str_literal_big "123 Main St")
                      (lc "city")
                    )
                    (str_literal_big "Springfield")
                  )
                  (lc "coordinates")
                )
                (block
                  (binop_colon
                    (lc "lat")
                    (binop_colon
                      (tuple_literal
                        (frac_literal_big big:alice@example.com)
                        (lc "lng")
                      )
                      (unary_neg <unary>)
                    )
                  )
                )
              )
              (malformed malformed:expr_unexpected_token)
              (lc "contact")
            )
            (block
              (binop_colon
                (lc "email")
                (tuple_literal
                  (binop_colon
                    (tuple_literal
                      (str_literal_big "alice@example.com")
                      (lc "phone")
                    )
                    (block
                      (binop_colon
                        (lc "home")
                        (binop_colon
                          (tuple_literal
                            (str_literal_big "555-1234")
                            (lc "work")
                          )
                          (str_literal_big "555-5678")
                        )
                      )
                    )
                  )
                  (malformed malformed:expr_unexpected_token)
                  (malformed malformed:expr_unexpected_token)
                )
              )
            )
          )
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 7:5 to 7:5

**Parse Error**
at 11:5 to 11:5

**Parse Error**
at 12:1 to 12:1

**Parse Error**
at 8:14 to 12:2

**Parse Error**
at 3:14 to 12:2

**Parse Error**
at 1:1 to 12:2

**Unsupported Node**
at 3:12 to 3:13

**Unsupported Node**
at 8:12 to 8:13

**Unsupported Node**
at 1:1 to 1:1

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "person")
    (Expr.binop_colon
      (Expr.malformed)
      (Expr.record_literal
        (Expr.binop_colon
          (Expr.lookup "street")
          (Expr.binop_colon
            (Expr.malformed)
            (Expr.record_literal
              (Expr.binop_colon
                (Expr.lookup "email")
                (Expr.malformed)
              )
            )
          )
        )
      )
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag record_literal :type "{}")
~~~
# TYPES
~~~roc
{}
~~~
