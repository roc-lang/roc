# META
~~~ini
description=dbg_stmt
type=expr
~~~
# SOURCE
~~~roc

dbg (1 == 1)

4
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - dbg_stmt.md:1:1:2:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **
dbg** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**dbg_stmt.md:1:1:2:4:**
```roc

dbg (1 == 1)
```


# TOKENS
~~~zig
Newline(1:1-1:1),
KwDbg(2:1-2:4),OpenRound(2:5-2:6),Int(2:6-2:7),OpEquals(2:8-2:10),Int(2:11-2:12),CloseRound(2:12-2:13),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(4:1-4:2),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-2.4 (reason "expr_unexpected_token"))
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
