# META
~~~ini
description=multiline_string_expr
type=expr
~~~
# SOURCE
~~~roc
"""This is a string
"""With multiple lines
~~~
# TOKENS
~~~text
MultilineString UpperIdent LowerIdent LowerIdent LowerIdent MultilineString UpperIdent LowerIdent LowerIdent ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
This 
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **This ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_string_expr.md:1:4:1:9:**
```roc
"""This is a string
```
   ^^^^^


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
