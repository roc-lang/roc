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


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**record_updater_chained.md:4:32:4:34:**
```roc
updated_one = { ..person, age: 31 }
```
                               ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**record_updater_chained.md:4:35:5:1:**
```roc
updated_one = { ..person, age: 31 }
updated2 = { ..updated_one, city: "New York" }
```


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**record_updater_chained.md:5:35:5:45:**
```roc
updated2 = { ..updated_one, city: "New York" }
```
                                  ^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**record_updater_chained.md:5:46:6:1:**
```roc
updated2 = { ..updated_one, city: "New York" }
final = { ..updated2, name: "Alice Smith", age: 32 }
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**record_updater_chained.md:6:52:6:53:**
```roc
final = { ..updated2, name: "Alice Smith", age: 32 }
```
                                                   ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "person"))
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.malformed)
        (Expr.str_literal_big)
      )
      (Expr.binop_colon
        (Expr.malformed)
        (Expr.num_literal_i32 30)
      )
      (Expr.binop_colon
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
  (Stmt.type_anno
    (name "age")
    (type num_literal_i32)
  )
  (Stmt.malformed)
  (Stmt.assign
    (pattern (Patt.ident "updated2"))
    (Expr.record_literal
      (Expr.unary_double_dot)
    )
  )
  (Stmt.type_anno
    (name "city")
    (type str_literal_big)
  )
  (Stmt.malformed)
  (Stmt.assign
    (pattern (Patt.ident "final"))
    (Expr.record_literal
      (Expr.unary_double_dot)
    )
  )
  (Stmt.type_anno
    (name "name")
    (type binop_colon)
  )
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
