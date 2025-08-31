# META
~~~ini
description=Record with duplicate field names (error case)
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int Comma LowerIdent OpColon String Comma LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly ~~~
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
  (binop_colon
    (lc "name")
    (str_literal_small "Bob")
  )
  (binop_colon
    (lc "email")
    (str_literal_big "alice@example.com")
  )
  (binop_colon
    (lc "age")
    (num_literal_i32 25)
  )
)
~~~
# FORMATTED
~~~roc
{ name : "Alice", age : 30, name : "Bob", email : "alice@example.com", age : 25 }
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**error_duplicate_fields.md:1:3:1:16:**
```roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
```
  ^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**error_duplicate_fields.md:1:18:1:25:**
```roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
```
                 ^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**error_duplicate_fields.md:1:27:1:38:**
```roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
```
                          ^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**error_duplicate_fields.md:1:40:1:66:**
```roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
```
                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**error_duplicate_fields.md:1:68:1:75:**
```roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
```
                                                                   ^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
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
