# META
~~~ini
description=block_string_ann
type=expr
~~~
# SOURCE
~~~roc
"""${g}""":q
f
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **"""** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**block_string_ann.md:1:1:1:4:**
```roc
"""${g}""":q
```
^^^


# TOKENS
~~~zig
MultilineStringStart(1:1-1:4),StringPart(1:4-1:4),OpenStringInterpolation(1:4-1:6),LowerIdent(1:6-1:7),CloseStringInterpolation(1:7-1:8),StringPart(1:8-1:8),MultilineStringEnd(1:8-1:11),OpColon(1:11-1:12),LowerIdent(1:12-1:13),Newline(1:1-1:1),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
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
