# META
~~~ini
description=string_multiline_formatting_(due_to_templating_not_multiline_string_literal) (3)
type=expr
~~~
# SOURCE
~~~roc
"This is a string with ${
	some_func(
		a, # This is a comment
		b,
	)
} lines of text due to the template parts"
~~~
# TOKENS
~~~text
MalformedString LowerIdent OpenRound LowerIdent Comma LineComment LowerIdent Comma CloseRound CloseCurly LowerIdent LowerIdent LowerIdent LowerIdent LowerIdent LowerIdent LowerIdent LowerIdent MalformedString ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
some_func# This is a comment
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_3.md:1:26:2:2:**
```roc
"This is a string with ${
	some_func(
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **some_func** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_3.md:2:2:2:11:**
```roc
	some_func(
```
	^^^^^^^^^


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
