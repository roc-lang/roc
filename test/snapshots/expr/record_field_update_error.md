# META
~~~ini
description=Record with field update using old syntax (should give nice error message)
type=expr
~~~
# SOURCE
~~~roc
{ person & age: 31 }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAmpersand LowerIdent OpColon Int CloseCurly ~~~
# PARSE
~~~clojure
(block
  (lc "person")
  (malformed)
  (binop_colon
    (lc "age")
    (num_literal_i32 31)
  )
)
~~~
# FORMATTED
~~~roc
person
& 
age : 31
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - record_field_update_error.md:1:10:1:11
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_field_update_error.md:1:17:1:19
UNDEFINED VARIABLE - record_field_update_error.md:1:3:1:9
UNRECOGNIZED SYNTAX - record_field_update_error.md:1:10:1:11
MALFORMED TYPE - record_field_update_error.md:1:17:1:19
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **& ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_field_update_error.md:1:10:1:12:**
```roc
{ person & age: 31 }
```
         ^^


**UNDEFINED VARIABLE**
Nothing is named **person** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_field_update_error.md:1:3:1:9:**
```roc
{ person & age: 31 }
```
  ^^^^^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**record_field_update_error.md:1:17:1:19:**
```roc
{ person & age: 31 }
```
                ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lookup "person")
  (Expr.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "age"))
    (type type_4)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 8
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
~~~
# TYPES
~~~roc
age : _a
~~~
