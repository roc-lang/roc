# META
~~~ini
description=record_access_multiline_formatting (1)
type=expr
~~~
# SOURCE
~~~roc
some_fn(arg1)?
	.static_dispatch_method()?
	.next_static_dispatch_method()?
	.record_field?
~~~
# TOKENS
~~~text
LowerIdent OpenRound LowerIdent CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpQuestion ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
?
	
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **?
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_access_multiline_formatting_1.md:1:14:2:2:**
```roc
some_fn(arg1)?
	.static_dispatch_method()?
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
