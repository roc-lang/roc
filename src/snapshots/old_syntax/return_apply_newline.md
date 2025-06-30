# META
~~~ini
description=return_apply_newline
type=expr
~~~
# SOURCE
~~~roc
return
 n
 r#
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**return_apply_newline.md:1:1:1:1:**
```roc
return
```



# TOKENS
~~~zig
KwReturn(1:1-1:7),Newline(1:1-1:1),
LowerIdent(2:2-2:3),Newline(1:1-1:1),
LowerIdent(3:2-3:3),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.1 (reason "expr_unexpected_token"))
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
