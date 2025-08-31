# META
~~~ini
description=num_bang_amp_z_dot_t
type=expr
~~~
# SOURCE
~~~roc
4
!
&z.t
~~~
# TOKENS
~~~text
Int OpBang OpAmpersand LowerIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
!
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **!
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**num_bang_amp_z_dot_t.md:2:1:3:1:**
```roc
!
&z.t
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**num_bang_amp_z_dot_t.md:2:1:3:1:**
```roc
!
&z.t
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
