# META
~~~ini
description=comment_with_tab fail
type=expr
~~~
# SOURCE
~~~roc
# comment with a 	
4
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - comment_with_tab.md:1:2:2:2
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token ** comment with a 	
4** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**comment_with_tab.md:1:2:2:2:**
```roc
# comment with a 	
4
```


# TOKENS
~~~zig
Newline(1:2-1:19),
Int(2:1-2:2),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.2-2.2 (reason "expr_unexpected_token"))
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
