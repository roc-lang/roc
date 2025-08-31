# META
~~~ini
description=Record with special character fields (ok case)
type=expr
~~~
# SOURCE
~~~roc
{
    field_with_underscores: "underscore",
    field123: "numbers",
    camelCase: "camel",
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String Comma LowerIdent OpColon String Comma CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (binop_colon
    (lc "field_with_underscores")
    (str_literal_big "underscore")
  )
  (binop_colon
    (lc "field123")
    (str_literal_big "numbers")
  )
  (binop_colon
    (lc "camelCase")
    (str_literal_big "camel")
  )
)
~~~
# FORMATTED
~~~roc
{
	field_with_underscores : "underscore",
	field123 : "numbers",
	camelCase : "camel",
}
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_different_fields_ok.md:2:5:2:41:**
```roc
    field_with_underscores: "underscore",
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_different_fields_ok.md:3:5:3:24:**
```roc
    field123: "numbers",
```
    ^^^^^^^^^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**record_different_fields_ok.md:4:5:4:23:**
```roc
    camelCase: "camel",
```
    ^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.record_literal
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
