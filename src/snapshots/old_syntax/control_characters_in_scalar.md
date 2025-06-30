# META
~~~ini
description=control_characters_in_scalar
type=expr
~~~
# SOURCE
~~~roc
''
~~~
# PROBLEMS
**UNCLOSED SINGLE QUOTE**
This character literal is missing a closing single quote.

**UNEXPECTED TOKEN IN EXPRESSION**
The token **''** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**control_characters_in_scalar.md:1:1:1:4:**
```roc
''
```
^^^


# TOKENS
~~~zig
SingleQuote(1:1-1:4),EndOfFile(1:4-1:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.4 (reason "expr_unexpected_token"))
~~~
# FORMATTED
~~~roc

~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
