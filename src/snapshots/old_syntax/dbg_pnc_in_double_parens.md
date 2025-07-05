# META
~~~ini
description=dbg_pnc_in_double_parens
type=expr
~~~
# SOURCE
~~~roc
((dbg(r))
r)
~~~
~~~
# EXPECTED
PARSE ERROR - dbg_pnc_in_double_parens.md:2:2:2:2
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**dbg_pnc_in_double_parens.md:2:2:2:2:**
```roc
r)
```
 


# TOKENS
~~~zig
OpenRound(1:1-1:2),NoSpaceOpenRound(1:2-1:3),KwDbg(1:3-1:6),NoSpaceOpenRound(1:6-1:7),LowerIdent(1:7-1:8),CloseRound(1:8-1:9),CloseRound(1:9-1:10),Newline(1:1-1:1),
LowerIdent(2:1-2:2),CloseRound(2:2-2:3),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
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
