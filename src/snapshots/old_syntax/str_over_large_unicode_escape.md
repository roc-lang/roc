# META
~~~ini
description=str_over_large_unicode_escape fail
type=expr
~~~
# SOURCE
~~~roc
'\u(FFFFFFFFF)'
~~~
# PROBLEMS
**UNCLOSED SINGLE QUOTE**
This character literal is missing a closing single quote.

**UNEXPECTED TOKEN IN EXPRESSION**
The token **'\u(FFFFFFFFF)'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**str_over_large_unicode_escape.md:1:1:1:16:**
```roc
'\u(FFFFFFFFF)'
```
^^^^^^^^^^^^^^^


# TOKENS
~~~zig
SingleQuote(1:1-1:16),EndOfFile(1:16-1:16),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.16 (reason "expr_unexpected_token"))
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
