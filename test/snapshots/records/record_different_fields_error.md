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
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_different_fields_error.md:2:20:2:21
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:2:21:2:39
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:2:39:2:40
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:2:40:2:41
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_different_fields_error.md:3:13:3:14
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:3:14:3:33
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:3:33:3:34
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:3:34:3:35
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:4:15:4:16
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:4:25:4:26
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:5:15:5:16
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:5:24:5:25
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:6:10:6:11
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_different_fields_error.md:6:20:6:21
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:6:21:6:27
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:6:27:6:28
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:6:28:6:29
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:7:10:7:17
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:7:17:7:18
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:7:30:7:31
MALFORMED TYPE - record_different_fields_error.md:2:20:2:21
UNRECOGNIZED SYNTAX - record_different_fields_error.md:2:21:2:39
UNRECOGNIZED SYNTAX - record_different_fields_error.md:2:39:2:40
UNRECOGNIZED SYNTAX - record_different_fields_error.md:2:40:2:41
MALFORMED TYPE - record_different_fields_error.md:3:13:3:14
UNRECOGNIZED SYNTAX - record_different_fields_error.md:3:14:3:33
UNRECOGNIZED SYNTAX - record_different_fields_error.md:3:33:3:34
UNRECOGNIZED SYNTAX - record_different_fields_error.md:3:34:3:35
UNRECOGNIZED SYNTAX - record_different_fields_error.md:4:15:4:16
UNRECOGNIZED SYNTAX - record_different_fields_error.md:4:25:4:26
UNDEFINED VARIABLE - record_different_fields_error.md:5:5:5:10
UNDEFINED VARIABLE - record_different_fields_error.md:5:11:5:15
UNRECOGNIZED SYNTAX - record_different_fields_error.md:5:15:5:16
UNRECOGNIZED SYNTAX - record_different_fields_error.md:5:24:5:25
UNDEFINED VARIABLE - record_different_fields_error.md:6:5:6:10
UNRECOGNIZED SYNTAX - record_different_fields_error.md:6:10:6:11
MALFORMED TYPE - record_different_fields_error.md:6:20:6:21
UNRECOGNIZED SYNTAX - record_different_fields_error.md:6:21:6:27
UNRECOGNIZED SYNTAX - record_different_fields_error.md:6:27:6:28
UNRECOGNIZED SYNTAX - record_different_fields_error.md:6:28:6:29
UNDEFINED VARIABLE - record_different_fields_error.md:7:5:7:10
UNRECOGNIZED SYNTAX - record_different_fields_error.md:7:10:7:17
UNRECOGNIZED SYNTAX - record_different_fields_error.md:7:17:7:18
UNRECOGNIZED SYNTAX - record_different_fields_error.md:7:30:7:31
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
; Total type variables: 20
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
~~~
# TYPES
~~~roc
~~~
