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
**UNDEFINED VARIABLE**
Nothing is named **field_with_underscores** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_different_fields_ok.md:2:5:2:27:**
```roc
    field_with_underscores: "underscore",
```
    ^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **field123** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_different_fields_ok.md:3:5:3:13:**
```roc
    field123: "numbers",
```
    ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **camelCase** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_different_fields_ok.md:4:5:4:14:**
```roc
    camelCase: "camel",
```
    ^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "field_with_underscores")
    (Expr.str_literal_big)
  )
  (Expr.binop_colon
    (Expr.lookup "field123")
    (Expr.str_literal_big)
  )
  (Expr.binop_colon
    (Expr.lookup "camelCase")
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
