# META
~~~ini
description=Basic record updater with field override
type=file
~~~
# SOURCE
~~~roc
module [person, updated]

person = { name: "Alice", age: 30 }
updated = { ..person, age: 31 }
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare BlankLine LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign OpenCurly DoubleDot LowerIdent Comma LowerIdent OpColon Int CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "person")

    (lc "updated")
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
    )
  )
  (binop_equals
    (lc "updated")
    (record_literal
      (double_dot_lc "person")
    )
  )
  (binop_colon
    (lc "age")
    (num_literal_i32 31)
  )
  (malformed)
)
~~~
# FORMATTED
~~~roc
module [person, updated]

person = { name: "Alice", age: 30 }
updated = { ..person }
age : 31
}
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**record_updater_basic.md:4:11:4:23:**
```roc
updated = { ..person, age: 31 }
```
          ^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_updater_basic.md:4:31:4:32:**
```roc
updated = { ..person, age: 31 }
```
                              ^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**record_updater_basic.md:4:28:4:30:**
```roc
updated = { ..person, age: 31 }
```
                           ^^


**SHADOWING**
This definition shadows an existing one.

**record_updater_basic.md:4:23:4:26:**
```roc
updated = { ..person, age: 31 }
```
                      ^^^


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
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "updated"))
    (Expr.record_literal
      (Expr.unary_double_dot)
    )
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "age"))
    (type type_17)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 25
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 -> #22)
(var #4 _)
(var #5 Str)
(var #6 _)
(var #7 _)
(var #8 Num *)
(var #9 _)
(var #10 -> #22)
(var #11 _)
(var #12 -> #23)
(var #13 _)
(var #14 -> #23)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 {})
(var #22 record)
(var #23 {})
(var #24 _)
~~~
# TYPES
~~~roc
age : _a
~~~
