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
(Expr.str_literal_small)
~~~
# SOLVED
~~~clojure
(expr :tag str_literal_small :type "Str")
~~~
# TYPES
~~~roc
Str
~~~
