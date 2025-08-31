# META
~~~ini
description=if_then_else (11)
type=expr
~~~
# SOURCE
~~~roc
if # Comment after if
	bool
		{
			1
		} else {
			2
		}
~~~
# TOKENS
~~~text
KwIf LineComment LowerIdent OpenCurly Int CloseCurly KwElse OpenCurly Int CloseCurly ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
# Comment after if
{
			
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **# Comment after if
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**if_then_else_comments_block.md:1:4:2:2:**
```roc
if # Comment after if
	bool
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **{
			** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**if_then_else_comments_block.md:3:3:4:4:**
```roc
		{
			1
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
