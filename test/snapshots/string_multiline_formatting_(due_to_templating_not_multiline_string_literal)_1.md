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
**Parse Error**
at 1:57 to 2:1

**Parse Error**
at 2:1 to 2:2

**Unsupported Node**
at 2:1 to 2:2

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
