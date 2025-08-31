# META
~~~ini
description=Match expression with f64 literal pattern (should error)
type=expr
~~~
# SOURCE
~~~roc
match x {
    3.14f64 => "pi"
    0.0f64 => "zero"
    value => "other"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly Float LowerIdent OpFatArrow String Float LowerIdent OpFatArrow String LowerIdent OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
0.0
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**f64_pattern_literal_error.md:2:9:2:13:**
```roc
    3.14f64 => "pi"
```
        ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **0.0** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**f64_pattern_literal_error.md:3:5:3:8:**
```roc
    0.0f64 => "zero"
```
    ^^^


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
