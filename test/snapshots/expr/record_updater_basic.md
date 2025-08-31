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
~~~
# FORMATTED
~~~roc
module [person, updated]

person = { name : "Alice", age : 30 }
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


**UNDEFINED VARIABLE**
Nothing is named **name** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_updater_basic.md:3:12:3:16:**
```roc
person = { name: "Alice", age: 30 }
```
           ^^^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**record_updater_basic.md:4:28:4:30:**
```roc
updated = { ..person, age: 31 }
```
                           ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**record_updater_basic.md:4:31:4:32:**
```roc
updated = { ..person, age: 31 }
```
                              ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "person"))
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
  (Stmt.assign
    (pattern (Patt.ident "updated"))
    (Expr.record_literal
      (Expr.unary_double_dot)
    )
  )
  (Stmt.type_anno
    (name "age")
    (type num_literal_i32)
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
