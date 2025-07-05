# META
~~~ini
description=dbg_stmt_in_parens
type=expr
~~~
# SOURCE
~~~roc
(dbg D
 q
h)
~~~
~~~
# EXPECTED
PARSE ERROR - dbg_stmt_in_parens.md:3:2:3:2
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**dbg_stmt_in_parens.md:3:2:3:2:**
```roc
h)
```
 


# TOKENS
~~~zig
OpenRound(1:1-1:2),KwDbg(1:2-1:5),UpperIdent(1:6-1:7),Newline(1:1-1:1),
LowerIdent(2:2-2:3),Newline(1:1-1:1),
LowerIdent(3:1-3:2),CloseRound(3:2-3:3),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.1 (reason "expected_expr_close_round_or_comma"))
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
