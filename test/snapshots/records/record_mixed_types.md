# META
~~~ini
description=Record with mixed field types
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: 30, active: Bool.true, scores: [95, 87, 92], balance: 1250.75 }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int Comma LowerIdent OpColon UpperIdent Dot LowerIdent Comma LowerIdent OpColon OpenSquare Int Comma Int Comma Int CloseSquare Comma LowerIdent OpColon Float CloseCurly ~~~
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
    (lc "active")
    (binop_pipe
      (uc "Bool")
      (dot_lc "true")
    )
  )
  (binop_colon
    (lc "scores")
    (list_literal
      (num_literal_i32 95)
      (num_literal_i32 87)
      (num_literal_i32 92)
    )
  )
  (binop_colon
    (lc "balance")
    (frac_literal_big big:<idx:6>)
  )
)
~~~
# FORMATTED
~~~roc
{ name : "Alice", age : 30, active : Bool.true, scores : [95, 87, 92], balance : 1250.75 }
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_mixed_types.md:1:3:1:16:**
```roc
{ name: "Alice", age: 30, active: Bool.true, scores: [95, 87, 92], balance: 1250.75 }
```
  ^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_mixed_types.md:1:18:1:25:**
```roc
{ name: "Alice", age: 30, active: Bool.true, scores: [95, 87, 92], balance: 1250.75 }
```
                 ^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_mixed_types.md:1:27:1:44:**
```roc
{ name: "Alice", age: 30, active: Bool.true, scores: [95, 87, 92], balance: 1250.75 }
```
                          ^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_mixed_types.md:1:46:1:66:**
```roc
{ name: "Alice", age: 30, active: Bool.true, scores: [95, 87, 92], balance: 1250.75 }
```
                                             ^^^^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_mixed_types.md:1:68:1:84:**
```roc
{ name: "Alice", age: 30, active: Bool.true, scores: [95, 87, 92], balance: 1250.75 }
```
                                                                   ^^^^^^^^^^^^^^^^


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
