# META
~~~ini
description=mixed_docs
type=expr
~~~
# SOURCE
~~~roc
### not docs!
## docs, but with a problem
## (namely that this is a mix of docs and regular comments)
# not docs
x = 5

42
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - mixed_docs.md:1:2:2:28
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **## not docs!
## docs, but with a problem** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**mixed_docs.md:1:2:2:28:**
```roc
### not docs!
## docs, but with a problem
```


# TOKENS
~~~zig
Newline(1:2-1:14),
Newline(2:2-2:28),
Newline(3:2-3:60),
Newline(4:2-4:11),
LowerIdent(5:1-5:2),OpAssign(5:3-5:4),Int(5:5-5:6),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(7:1-7:3),Newline(1:1-1:1),
MalformedUnknownToken(8:1-8:2),MalformedUnknownToken(8:2-8:3),MalformedUnknownToken(8:3-8:4),EndOfFile(8:4-8:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.2-2.28 (reason "expr_unexpected_token"))
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
