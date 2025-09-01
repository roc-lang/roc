# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module [person, updated]

person = { name: "Alice", age: 30 }
updated = { ..person,
 age: 31 }
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

**fuzz_crash_047.md:4:11:5:2:**
```roc
updated = { ..person,
 age: 31 }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_047.md:5:10:5:11:**
```roc
 age: 31 }
```
         ^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**fuzz_crash_047.md:5:7:5:9:**
```roc
 age: 31 }
```
      ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_047.md:5:10:5:11:**
```roc
 age: 31 }
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
