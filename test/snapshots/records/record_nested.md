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
            (frac_literal_big big:<idx:112>)
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
  (Expr.binop_colon
    (lc "person")
    (Expr.record_literal
      (Expr.binop_colon
        (lc "name")
        (Expr.str_literal_big)
      )
      (Expr.binop_colon
        (lc "age")
        (Expr.num_literal_i32 30)
      )
    )
  )
  (Expr.binop_colon
    (lc "address")
    (Expr.record_literal
      (Expr.binop_colon
        (lc "street")
        (Expr.str_literal_big)
      )
      (Expr.binop_colon
        (lc "city")
        (Expr.str_literal_big)
      )
      (Expr.binop_colon
        (lc "coordinates")
        (Expr.record_literal
          (Expr.binop_colon
            (lc "lat")
            (Expr.frac_literal_big big:<idx:112>)
          )
          (Expr.binop_colon
            (lc "lng")
            (Expr.unary_neg)
          )
        )
      )
    )
  )
  (Expr.binop_colon
    (lc "contact")
    (Expr.record_literal
      (Expr.binop_colon
        (lc "email")
        (Expr.str_literal_big)
      )
      (Expr.binop_colon
        (lc "phone")
        (Expr.record_literal
          (Expr.binop_colon
            (lc "home")
            (Expr.str_literal_big)
          )
          (Expr.binop_colon
            (lc "work")
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
(expr :tag record_literal :type "{ person:{ name:Str, age:Num(_size) }, address:{ street:Str, city:Str, coordinates:{ lat:F64, lng:F64 } }, contact:{ email:Str, phone:{ home:Str, work:Str } } }")
~~~
# TYPES
~~~roc
{ person:{ name:Str, age:Num(_size) }, address:{ street:Str, city:Str, coordinates:{ lat:F64, lng:F64 } }, contact:{ email:Str, phone:{ home:Str, work:Str } } }
~~~
