# META
~~~ini
description=when_in_parens
type=expr
~~~
# SOURCE
~~~roc
(when x is
    Ok ->
        3)
~~~
~~~
# EXPECTED
PARSE ERROR - when_in_parens.md:3:10:3:10
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**when_in_parens.md:3:10:3:10:**
```roc
        3)
```
         


# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:2-1:6),LowerIdent(1:7-1:8),LowerIdent(1:9-1:11),Newline(1:1-1:1),
UpperIdent(2:5-2:7),OpArrow(2:8-2:10),Newline(1:1-1:1),
Int(3:9-3:10),CloseRound(3:10-3:11),Newline(1:1-1:1),
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
