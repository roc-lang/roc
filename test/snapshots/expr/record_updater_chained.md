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
(block
  (binop_equals
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
      (binop_colon
        (lc "city")
        (str_literal_big "Boston")
      )
    )
  )
  (binop_equals
    (lc "updated_one")
    (record_literal
      (double_dot_lc "person")
    )
  )
  (binop_colon
    (lc "age")
    (num_literal_i32 31)
  )
  (malformed)
  (binop_equals
    (lc "updated2")
    (record_literal
      (double_dot_lc "updated_one")
    )
  )
  (binop_colon
    (lc "city")
    (str_literal_big "New York")
  )
  (malformed)
  (binop_equals
    (lc "final")
    (record_literal
      (double_dot_lc "updated2")
    )
  )
  (binop_colon
    (lc "name")
    (binop_colon
      (tuple_literal
        (str_literal_big "Alice Smith")
        (lc "age")
      )
      (num_literal_i32 32)
    )
  )
  (malformed)
)
~~~
# FORMATTED
~~~roc
module [person, final]

person = { name: "Alice", age: 30, city: "Boston" }
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


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**record_updater_chained.md:4:32:4:34:**
```roc
updated_one = { ..person, age: 31 }
```
                               ^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**record_updater_chained.md:5:35:5:45:**
```roc
updated2 = { ..updated_one, city: "New York" }
```
                                  ^^^^^^^^^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**record_updater_chained.md:6:29:6:51:**
```roc
final = { ..updated2, name: "Alice Smith", age: 32 }
```
                            ^^^^^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "person"))
    (Expr.record_literal
      (Expr.record_field
        (Expr.malformed)
        (Expr.str_literal_big)
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.num_literal_i32 30)
      )
      (Expr.record_field
        (Expr.malformed)
        (Expr.str_literal_big)
      )
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "updated_one"))
    (Expr.record_literal
      (Expr.unary_double_dot)
    )
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "age"))
    (type type_20)
  )
  (Expr.malformed)
  (Stmt.assign
    (pattern (Patt.ident "updated2"))
    (Expr.record_literal
      (Expr.unary_double_dot)
    )
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "city"))
    (type type_28)
  )
  (Expr.malformed)
  (Stmt.assign
    (pattern (Patt.ident "final"))
    (Expr.record_literal
      (Expr.unary_double_dot)
    )
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "name"))
    (type type_40)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 51
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 -> #44)
(var #4 _)
(var #5 Str)
(var #6 _)
(var #7 _)
(var #8 Num *)
(var #9 _)
(var #10 _)
(var #11 Str)
(var #12 _)
(var #13 -> #44)
(var #14 _)
(var #15 -> #45)
(var #16 _)
(var #17 -> #45)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 -> #47)
(var #24 _)
(var #25 -> #47)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 -> #49)
(var #32 _)
(var #33 -> #49)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 {})
(var #45 {})
(var #46 _)
(var #47 {})
(var #48 _)
(var #49 {})
(var #50 _)
~~~
# TYPES
~~~roc
updated_one : {}
updated2 : {}
city : _a
person : {}
age : _a
name : _a
final : {}
~~~
