# META
~~~ini
description=Record destructuring with rest pattern
type=expr
~~~
# SOURCE
~~~roc
match person {
    { first_name, ..others } => Str.len(first_name) > Str.len(others.last_name)
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenCurly LowerIdent Comma DoubleDot LowerIdent CloseCurly OpFatArrow UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound OpGreaterThan UpperIdent Dot LowerIdent OpenRound LowerIdent Dot LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **Str** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**pattern_destructure_with_rest.md:2:33:2:36:**
```roc
    { first_name, ..others } => Str.len(first_name) > Str.len(others.last_name)
```
                                ^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **.** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**pattern_destructure_with_rest.md:2:36:2:37:**
```roc
    { first_name, ..others } => Str.len(first_name) > Str.len(others.last_name)
```
                                   ^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**pattern_destructure_with_rest.md:2:37:2:40:**
```roc
    { first_name, ..others } => Str.len(first_name) > Str.len(others.last_name)
```
                                    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**pattern_destructure_with_rest.md:3:1:3:2:**
```roc
}
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
