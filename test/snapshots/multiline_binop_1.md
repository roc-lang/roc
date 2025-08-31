# META
~~~ini
description=multiline_binop (1)
type=expr
~~~
# SOURCE
~~~roc
1 # One
	+ # Plus

	# A comment in between

	2 # Two
		* # Times
		3
~~~
# TOKENS
~~~text
Int LineComment OpPlus LineComment BlankLine LineComment BlankLine Int LineComment OpStar LineComment Int ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
# One
	# Plus
# A comment in between
# Two
# Times
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **# One
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_binop_1.md:1:3:2:2:**
```roc
1 # One
	+ # Plus
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_binop_1.md:1:3:2:2:**
```roc
1 # One
	+ # Plus
```


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
