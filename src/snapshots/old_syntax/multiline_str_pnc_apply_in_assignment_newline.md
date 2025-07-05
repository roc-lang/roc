# META
~~~ini
description=multiline_str_pnc_apply_in_assignment_newline
type=expr
~~~
# SOURCE
~~~roc
(e=""""""().d
e)m
~~~
~~~
# EXPECTED
PARSE ERROR - multiline_str_pnc_apply_in_assignment_newline.md:1:11:1:14
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**multiline_str_pnc_apply_in_assignment_newline.md:1:11:1:14:**
```roc
(e=""""""().d
```
          ^^^


# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:2-1:3),OpAssign(1:3-1:4),MultilineStringStart(1:4-1:7),StringPart(1:7-1:7),MultilineStringEnd(1:7-1:10),NoSpaceOpenRound(1:10-1:11),CloseRound(1:11-1:12),NoSpaceDotLowerIdent(1:12-1:14),Newline(1:1-1:1),
LowerIdent(2:1-2:2),CloseRound(2:2-2:3),LowerIdent(2:3-2:4),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.11-1.14 (reason "expected_expr_close_round_or_comma"))
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
