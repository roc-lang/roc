# META
~~~ini
description=Record with reserved keyword fields (error case)
type=expr
~~~
# SOURCE
~~~roc
{
    if: "conditional",
    when: "pattern match",
    expect: "test assertion",
    import: "module load",
    and: Bool.true,
    or: Bool.false,
}
~~~
# TOKENS
~~~text
OpenCurly KwIf OpColon String Comma LowerIdent OpColon String Comma KwExpect OpColon String Comma KwImport OpColon String Comma OpAnd OpColon UpperIdent Dot LowerIdent Comma OpOr OpColon UpperIdent Dot LowerIdent Comma CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (if_without_else
    (condition       (malformed malformed:expr_unexpected_token)
)
    (then       (str_literal_big "conditional")
))
  (binop_colon
    (lc "when")
    (str_literal_big "pattern match")
  )
  (binop_colon
    (malformed malformed:expr_unexpected_token)
    (str_literal_big "test assertion")
  )
  (binop_colon
    (malformed malformed:expr_unexpected_token)
    (str_literal_big "module load")
  )
  (binop_colon
    (malformed malformed:expr_unexpected_token)
    (binop_pipe
      (uc "Bool")
      (dot_lc "true")
    )
  )
  (binop_colon
    (malformed malformed:expr_unexpected_token)
    (binop_pipe
      (uc "Bool")
      (dot_lc "false")
    )
  )
)
~~~
# FORMATTED
~~~roc
{
	if : "conditional",
	when : "pattern match",
	expect : "test assertion",
	import : "module load",
	and : Bool.true,
	or : Bool.false,
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **: ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:2:7:2:9:**
```roc
    if: "conditional",
```
      ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **expect** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:4:5:4:11:**
```roc
    expect: "test assertion",
```
    ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **import** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:5:5:5:11:**
```roc
    import: "module load",
```
    ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **and** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:6:5:6:8:**
```roc
    and: Bool.true,
```
    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **or** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:7:5:7:7:**
```roc
    or: Bool.false,
```
    ^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_different_fields_reserved_error.md:3:5:3:26:**
```roc
    when: "pattern match",
```
    ^^^^^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_different_fields_reserved_error.md:4:5:4:29:**
```roc
    expect: "test assertion",
```
    ^^^^^^^^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_different_fields_reserved_error.md:5:5:5:26:**
```roc
    import: "module load",
```
    ^^^^^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_different_fields_reserved_error.md:6:5:6:19:**
```roc
    and: Bool.true,
```
    ^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_different_fields_reserved_error.md:7:5:7:19:**
```roc
    or: Bool.false,
```
    ^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.if_else)
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
