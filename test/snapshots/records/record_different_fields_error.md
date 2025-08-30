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
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
$
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 6:10

**Parse Error**
at 6:10 to 6:11

**Unsupported Node**
at 6:10 to 6:11

# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~
