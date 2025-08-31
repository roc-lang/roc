# META
~~~ini
description=string_multiline_formatting_(due_to_templating_not_multiline_string_literal) (1)
type=expr
~~~
# SOURCE
~~~roc
"This is a string with ${some_func(a, #This is a comment
b)} lines of text due to the template parts"
~~~
# TOKENS
~~~text
MalformedString LowerIdent CloseRound CloseCurly LowerIdent LowerIdent LowerIdent LowerIdent LowerIdent LowerIdent LowerIdent LowerIdent MalformedString ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
b
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_1.md:1:57:2:1:**
```roc
"This is a string with ${some_func(a, #This is a comment
b)} lines of text due to the template parts"
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **b** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_1.md:2:1:2:2:**
```roc
b)} lines of text due to the template parts"
```
^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_1.md:2:1:2:2:**
```roc
b)} lines of text due to the template parts"
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
