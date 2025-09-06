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
(malformed)
~~~
# FORMATTED
~~~roc
b
~~~
# EXPECTED
UNDEFINED VARIABLE - string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_1.md:1:26:1:35
UNDEFINED VARIABLE - string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_1.md:1:36:1:37
UNDEFINED VARIABLE - string_multiline_formatting_(due_to_templating_not_multiline_string_literal)_1.md:2:1:2:2
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


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
