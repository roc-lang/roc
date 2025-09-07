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
(record_literal
  (binop_colon
    (lc "person")
    (record_literal
      (binop_colon
        (lc "name")
        (str_literal_big "Alice")
      )
      (binop_colon
        (lc "age")
        (num_literal_i32 30)
      )
    )
  )
  (binop_colon
    (lc "address")
    (record_literal
      (binop_colon
        (lc "street")
        (str_literal_big "123 Main St")
      )
      (binop_colon
        (lc "city")
        (str_literal_big "Springfield")
      )
      (binop_colon
        (lc "coordinates")
        (record_literal
          (binop_colon
            (lc "lat")
            (frac_literal_big frac:<idx:112>)
          )
          (binop_colon
            (lc "lng")
            (unary_neg <unary_op>)
          )
        )
      )
    )
  )
  (binop_colon
    (lc "contact")
    (record_literal
      (binop_colon
        (lc "email")
        (str_literal_big "alice@example.com")
      )
      (binop_colon
        (lc "phone")
        (record_literal
          (binop_colon
            (lc "home")
            (str_literal_big "555-1234")
          )
          (binop_colon
            (lc "work")
            (str_literal_big "555-5678")
          )
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
{ person: { name: "Alice", age: 30 }, address: { street: "123 Main St", city: "Springfield", coordinates: { lat: 42.1234, lng: -71.5678 } }, contact: { email: "alice@example.com", phone: { home: "555-1234", work: "555-5678" } } }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.record_field
    (Expr.malformed)
    (Expr.record_literal
      (Expr.record_field
        (Expr.malformed)
        (Expr.str_literal_big)
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.num_literal_i32 30)
      )
    )
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.record_literal
      (Expr.record_field
        (Expr.malformed)
        (Expr.str_literal_big)
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.str_literal_big)
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.record_literal
          (Expr.record_field
            (Expr.malformed)
            (Expr.frac_literal_big big:<idx:112>)
          )
          (Expr.record_field
            (Expr.malformed)
            (Expr.unary_neg)
          )
        )
      )
    )
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.record_literal
      (Expr.record_field
        (Expr.malformed)
        (Expr.str_literal_big)
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.record_literal
          (Expr.record_field
            (Expr.malformed)
            (Expr.str_literal_big)
          )
          (Expr.record_field
            (Expr.malformed)
            (Expr.str_literal_big)
          )
        )
      )
    )
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 57
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 Str)
(var #4 _)
(var #5 _)
(var #6 Num *)
(var #7 _)
(var #8 -> #46)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 Str)
(var #13 _)
(var #14 _)
(var #15 Str)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 F64)
(var #20 _)
(var #21 _)
(var #22 F64)
(var #23 -> #22)
(var #24 _)
(var #25 -> #48)
(var #26 _)
(var #27 -> #50)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 Str)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 Str)
(var #36 _)
(var #37 _)
(var #38 Str)
(var #39 _)
(var #40 -> #52)
(var #41 _)
(var #42 -> #54)
(var #43 _)
(var #44 -> #56)
(var #45 {})
(var #46 record)
(var #47 {})
(var #48 record)
(var #49 {})
(var #50 record)
(var #51 {})
(var #52 record)
(var #53 {})
(var #54 record)
(var #55 {})
(var #56 record)
~~~
# TYPES
~~~roc
~~~
