# META
~~~ini
description=Record with special character fields (error cases)
type=expr
~~~
# SOURCE
~~~roc
{
    _privateField: "leading underscore",
    field_: "trailing underscore",
    PascalCase: "pascal",
    kebab-case: "kebab",
    field$special: "dollar",
    field@symbol: "at symbol",
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String Comma UpperIdent OpColon String Comma LowerIdent OpUnaryMinus LowerIdent OpColon String Comma LowerIdent MalformedUnknownToken LowerIdent OpColon String Comma LowerIdent MalformedNominalNameWithoutName LowerIdent OpColon String Comma CloseCurly ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
$
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**record_different_fields_error.md:1:1:6:10:**
```roc
{
    _privateField: "leading underscore",
    field_: "trailing underscore",
    PascalCase: "pascal",
    kebab-case: "kebab",
    field$special: "dollar",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **$** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_error.md:6:10:6:11:**
```roc
    field$special: "dollar",
```
         ^


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
(expr :tag malformed :type "Error")
~~~
# TYPES
~~~roc
Error
~~~
