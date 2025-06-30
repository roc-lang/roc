# META
~~~ini
description=docs
type=expr
~~~
# SOURCE
~~~roc
#######
### not docs!
##actually docs
##
######
x = 5

42
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **######
### not docs!** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**docs.md:1:2:2:14:**
```roc
#######
### not docs!
```


# TOKENS
~~~zig
Newline(1:2-1:8),
Newline(2:2-2:14),
Newline(3:2-3:16),
Newline(4:2-4:3),
Newline(5:2-5:7),
LowerIdent(6:1-6:2),OpAssign(6:3-6:4),Int(6:5-6:6),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(8:1-8:3),EndOfFile(8:3-8:3),
~~~
# PARSE
~~~clojure
(e-malformed @1.2-2.14 (reason "expr_unexpected_token"))
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
