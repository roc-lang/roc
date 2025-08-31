# META
~~~ini
description=record_access_multiline_formatting (4)
type=expr
~~~
# SOURCE
~~~roc
some_fn(arg1)? # Comment 1
	.static_dispatch_method()? # Comment 2
	.next_static_dispatch_method()? # Comment 3
	.record_field?
~~~
# TOKENS
~~~text
LowerIdent OpenRound LowerIdent CloseRound OpQuestion LineComment Dot LowerIdent OpenRound CloseRound OpQuestion LineComment Dot LowerIdent OpenRound CloseRound OpQuestion LineComment Dot LowerIdent OpQuestion ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
? # Comment 1
	# Comment 2
# Comment 3
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **? # Comment 1
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_access_multiline_formatting_4.md:1:14:2:2:**
```roc
some_fn(arg1)? # Comment 1
	.static_dispatch_method()? # Comment 2
```


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
