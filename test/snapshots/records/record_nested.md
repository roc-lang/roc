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
{ person : {name : "Alice", age : 30}, address : {street : "123 Main St", city : "Springfield", coordinates : {lat : 42.1234, lng : -71.5678}}, contact : {email : "alice@example.com", phone : {home : "555-1234", work : "555-5678"}} }
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **person** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_nested.md:2:5:2:11:**
```roc
    person: { name: "Alice", age: 30 },
```
    ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **name** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_nested.md:2:15:2:19:**
```roc
    person: { name: "Alice", age: 30 },
```
              ^^^^


**UNDEFINED VARIABLE**
Nothing is named **age** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_nested.md:2:30:2:33:**
```roc
    person: { name: "Alice", age: 30 },
```
                             ^^^


**UNDEFINED VARIABLE**
Nothing is named **address** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_nested.md:3:5:3:12:**
```roc
    address: {
```
    ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **street** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_nested.md:4:9:4:15:**
```roc
        street: "123 Main St",
```
        ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **city** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_nested.md:5:9:5:13:**
```roc
        city: "Springfield",
```
        ^^^^


**UNDEFINED VARIABLE**
Nothing is named **coordinates** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_nested.md:6:9:6:20:**
```roc
        coordinates: { lat: 42.1234, lng: -71.5678 },
```
        ^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **lat** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_nested.md:6:24:6:27:**
```roc
        coordinates: { lat: 42.1234, lng: -71.5678 },
```
                       ^^^


**UNDEFINED VARIABLE**
Nothing is named **lng** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_nested.md:6:38:6:41:**
```roc
        coordinates: { lat: 42.1234, lng: -71.5678 },
```
                                     ^^^


**UNDEFINED VARIABLE**
Nothing is named **contact** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_nested.md:8:5:8:12:**
```roc
    contact: {
```
    ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **email** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_nested.md:9:9:9:14:**
```roc
        email: "alice@example.com",
```
        ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **phone** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_nested.md:10:9:10:14:**
```roc
        phone: { home: "555-1234", work: "555-5678" },
```
        ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **home** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_nested.md:10:18:10:22:**
```roc
        phone: { home: "555-1234", work: "555-5678" },
```
                 ^^^^


**UNDEFINED VARIABLE**
Nothing is named **work** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_nested.md:10:36:10:40:**
```roc
        phone: { home: "555-1234", work: "555-5678" },
```
                                   ^^^^


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "person")
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "name")
        (Expr.str_literal_big)
      )
      (Expr.binop_colon
        (Expr.lookup "age")
        (Expr.num_literal_i32 30)
      )
    )
  )
  (Expr.binop_colon
    (Expr.lookup "address")
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "street")
        (Expr.str_literal_big)
      )
      (Expr.binop_colon
        (Expr.lookup "city")
        (Expr.str_literal_big)
      )
      (Expr.binop_colon
        (Expr.lookup "coordinates")
        (Expr.record_literal
          (Expr.binop_colon
            (Expr.lookup "lat")
            (Expr.frac_literal_big big:<idx:112>)
          )
          (Expr.binop_colon
            (Expr.lookup "lng")
            (Expr.unary_neg)
          )
        )
      )
    )
  )
  (Expr.binop_colon
    (Expr.lookup "contact")
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "email")
        (Expr.str_literal_big)
      )
      (Expr.binop_colon
        (Expr.lookup "phone")
        (Expr.record_literal
          (Expr.binop_colon
            (Expr.lookup "home")
            (Expr.str_literal_big)
          )
          (Expr.binop_colon
            (Expr.lookup "work")
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
(expr :tag record_literal :type "{}")
~~~
# TYPES
~~~roc
{}
~~~
