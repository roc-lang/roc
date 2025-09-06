# META
~~~ini
description=Function with record parameter destructuring and rest pattern, capture whole record using `as`
type=expr
~~~
# SOURCE
~~~roc
|{ name, age, ..a } as person| { greeting: "Hello ${name}", full_record: person, is_adult: age >= 18 }
~~~
# TOKENS
~~~text
OpBar OpenCurly LowerIdent Comma LowerIdent Comma DoubleDot LowerIdent CloseCurly KwAs LowerIdent OpBar OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent OpGreaterThanOrEq Int CloseCurly ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
person
~~~
# EXPECTED
UNUSED VARIABLE - function_record_parameter_capture.md:1:15:1:18
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**function_record_parameter_capture.md:1:21:1:24:**
```roc
|{ name, age, ..a } as person| { greeting: "Hello ${name}", full_record: person, is_adult: age >= 18 }
```
                    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **person** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**function_record_parameter_capture.md:1:24:1:30:**
```roc
|{ name, age, ..a } as person| { greeting: "Hello ${name}", full_record: person, is_adult: age >= 18 }
```
                       ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
; Total type variables: 9
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
~~~
# TYPES
~~~roc
~~~
