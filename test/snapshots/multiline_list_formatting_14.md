# META
~~~ini
description=multiline_list_formatting (14)
type=expr
~~~
# SOURCE
~~~roc
[ # Open
	1, # First

	# A comment in the middle

	2, # Second
	# This comment has no blanks around it
	3, # Third
]
~~~
# TOKENS
~~~text
OpenSquare LineComment Int Comma LineComment BlankLine LineComment BlankLine Int Comma LineComment LineComment Int Comma LineComment CloseSquare ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
1# First
# A comment in the middle
# Second
# This comment has no blanks around it
# Third
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **# Open
	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_list_formatting_14.md:1:3:2:2:**
```roc
[ # Open
	1, # First
```


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**multiline_list_formatting_14.md:1:1:2:2:**
```roc
[ # Open
	1, # First
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **1** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**multiline_list_formatting_14.md:2:2:2:3:**
```roc
	1, # First
```
	^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multiline_list_formatting_14.md:2:2:2:3:**
```roc
	1, # First
```
	^


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
