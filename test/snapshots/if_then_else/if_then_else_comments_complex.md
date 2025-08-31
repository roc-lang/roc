# META
~~~ini
description=if_then_else (15)
type=expr
~~~
# SOURCE
~~~roc
if # Comment after if
	bool # Comment after cond
		{ # Comment after then open
			1
		} # Comment after then close
			else # Comment after else
				{ # Comment else open
					2
				}
~~~
# TOKENS
~~~text
KwIf LineComment LowerIdent LineComment OpenCurly LineComment Int CloseCurly LineComment KwElse LineComment OpenCurly LineComment Int CloseCurly ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
{ # Comment after then open
			# Comment after then close
# Comment after else
# Comment else open
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **# Comment after if
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**if_then_else_comments_complex.md:1:4:2:2:**
```roc
if # Comment after if
	bool # Comment after cond
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **{ # Comment after then open
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**if_then_else_comments_complex.md:3:3:4:4:**
```roc
		{ # Comment after then open
			1
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**if_then_else_comments_complex.md:3:3:4:4:**
```roc
		{ # Comment after then open
			1
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
