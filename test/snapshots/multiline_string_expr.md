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
(str_literal_small "")
~~~
# FORMATTED
~~~roc
""
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_not_equals)
~~~
# SOLVED
~~~clojure
(expr :tag binop_not_equals :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~
