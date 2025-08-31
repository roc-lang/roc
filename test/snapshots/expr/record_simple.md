# META
~~~ini
description=Record expression
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: 30 }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly ~~~
# PARSE
~~~clojure
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
~~~
# FORMATTED
~~~roc
{ name : "Alice", age : 30 }
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_simple.md:1:3:1:16:**
```roc
{ name: "Alice", age: 30 }
```
  ^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_simple.md:1:18:1:25:**
```roc
{ name: "Alice", age: 30 }
```
                 ^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.malformed)
  (Expr.malformed)
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
