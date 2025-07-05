# META
~~~ini
description=parens_apply_newline
type=expr
~~~
# SOURCE
~~~roc
(f
N)
N#
~~~
~~~
# EXPECTED
PARSE ERROR - parens_apply_newline.md:2:2:2:2
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**parens_apply_newline.md:2:2:2:2:**
```roc
N)
```
 


# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:2-1:3),Newline(1:1-1:1),
UpperIdent(2:1-2:2),CloseRound(2:2-2:3),Newline(1:1-1:1),
UpperIdent(3:1-3:2),Newline(3:3-3:3),
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
