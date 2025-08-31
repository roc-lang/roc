# META
~~~ini
description=Chained record updater expressions
type=file
~~~
# SOURCE
~~~roc
module [person, final]

person = { name: "Alice", age: 30, city: "Boston" }
updated_one = { ..person, age: 31 }
updated2 = { ..updated_one, city: "New York" }
final = { ..updated2, name: "Alice Smith", age: 32 }
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare BlankLine LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int Comma LowerIdent OpColon String CloseCurly LowerIdent OpAssign OpenCurly DoubleDot LowerIdent Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign OpenCurly DoubleDot LowerIdent Comma LowerIdent OpColon String CloseCurly LowerIdent OpAssign OpenCurly DoubleDot LowerIdent Comma LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "person")

    (lc "final")
))
~~~
# FORMATTED
~~~roc
module [person, final]

person = { name : "Alice", age : 30, city : "Boston" }
updated_one = { ..person }
age : 31
}
updated2 = { ..updated_one }
city : "New York"
}
final = { ..updated2 }
name : ("Alice Smith", age) : 32
}
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**record_updater_chained.md:4:15:4:27:**
```roc
updated_one = { ..person, age: 31 }
```
              ^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_updater_chained.md:4:35:5:1:**
```roc
updated_one = { ..person, age: 31 }
updated2 = { ..updated_one, city: "New York" }
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**record_updater_chained.md:5:12:5:29:**
```roc
updated2 = { ..updated_one, city: "New York" }
```
           ^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_updater_chained.md:5:46:6:1:**
```roc
updated2 = { ..updated_one, city: "New York" }
final = { ..updated2, name: "Alice Smith", age: 32 }
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**record_updater_chained.md:6:9:6:23:**
```roc
final = { ..updated2, name: "Alice Smith", age: 32 }
```
        ^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_updater_chained.md:6:52:6:53:**
```roc
final = { ..updated2, name: "Alice Smith", age: 32 }
```
                                                   ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
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
      (Expr.binop_colon
        (Expr.lookup "city")
        (Expr.str_literal_big)
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "updated_one")
    (Expr.record_literal
      (Expr.unary_double_dot)
    )
  )
  (Expr.binop_colon
    (Expr.lookup "age")
    (Expr.num_literal_i32 31)
  )
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.lookup "updated2")
    (Expr.record_literal
      (Expr.unary_double_dot)
    )
  )
  (Expr.binop_colon
    (Expr.lookup "city")
    (Expr.str_literal_big)
  )
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.lookup "final")
    (Expr.record_literal
      (Expr.unary_double_dot)
    )
  )
  (Expr.binop_colon
    (Expr.lookup "name")
    (Expr.binop_colon
      (Expr.tuple_literal
        (Expr.str_literal_big)
        (Expr.lookup "age")
      )
      (Expr.num_literal_i32 32)
    )
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
person : {}
updated_one : {}
updated2 : {}
final : {}
~~~
