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
(malformed malformed:expr_unexpected_token)
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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_3.md:2:2:2:11:**
```roc
	some_func(
```
	^^^^^^^^^


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
