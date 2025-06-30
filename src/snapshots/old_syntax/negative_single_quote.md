# META
~~~ini
description=negative_single_quote
type=expr
~~~
# SOURCE
~~~roc
-'i'
~~~
# PROBLEMS
**UNCLOSED SINGLE QUOTE**
This character literal is missing a closing single quote.

**UNEXPECTED TOKEN IN EXPRESSION**
The token **-'i'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**negative_single_quote.md:1:1:1:5:**
```roc
-'i'
```
^^^^


# TOKENS
~~~zig
OpUnaryMinus(1:1-1:2),SingleQuote(1:2-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.5 (reason "expr_unexpected_token"))
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
