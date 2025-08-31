# META
~~~ini
description=Malformed record syntax using equals instead of colon (error case)
type=expr
~~~
# SOURCE
~~~roc
{ age: 42, name = "Alice" }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon Int Comma LowerIdent OpAssign String CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (binop_colon
    (lc "age")
    (num_literal_i32 42)
  )
  (binop_equals
    (lc "name")
    (str_literal_big "Alice")
  )
)
~~~
# FORMATTED
~~~roc
{ age : 42, name = "Alice" }
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**error_malformed_syntax_2.md:1:3:1:10:**
```roc
{ age: 42, name = "Alice" }
```
  ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **name** in this scope.
Is there an **import** or **exposing** missing up-top?

**error_malformed_syntax_2.md:1:12:1:16:**
```roc
{ age: 42, name = "Alice" }
```
           ^^^^


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.lookup "name")
    (Expr.str_literal_big)
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
