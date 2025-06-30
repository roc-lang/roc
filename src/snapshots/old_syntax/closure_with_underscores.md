# META
~~~ini
description=closure_with_underscores
type=expr
~~~
# SOURCE
~~~roc
\_, _name -> 42
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\_** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**closure_with_underscores.md:1:1:1:3:**
```roc
\_, _name -> 42
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),Underscore(1:2-1:3),Comma(1:3-1:4),NamedUnderscore(1:5-1:10),OpArrow(1:11-1:13),Int(1:14-1:16),EndOfFile(1:16-1:16),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.3 (reason "expr_unexpected_token"))
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
