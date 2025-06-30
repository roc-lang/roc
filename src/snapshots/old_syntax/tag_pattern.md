# META
~~~ini
description=tag_pattern
type=expr
~~~
# SOURCE
~~~roc
\Thing -> 42
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\Thing** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**tag_pattern.md:1:1:1:7:**
```roc
\Thing -> 42
```
^^^^^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),UpperIdent(1:2-1:7),OpArrow(1:8-1:10),Int(1:11-1:13),EndOfFile(1:13-1:13),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.7 (reason "expr_unexpected_token"))
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
