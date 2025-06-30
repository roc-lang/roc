# META
~~~ini
description=parenthetical_field_qualified_var
type=expr
~~~
# SOURCE
~~~roc
(One.Two.rec).field
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**parenthetical_field_qualified_var.md:1:13:1:20:**
```roc
(One.Two.rec).field
```
            ^^^^^^^


# TOKENS
~~~zig
OpenRound(1:1-1:2),UpperIdent(1:2-1:5),NoSpaceDotUpperIdent(1:5-1:9),NoSpaceDotLowerIdent(1:9-1:13),CloseRound(1:13-1:14),NoSpaceDotLowerIdent(1:14-1:20),EndOfFile(1:20-1:20),
~~~
# PARSE
~~~clojure
(e-malformed @1.13-1.20 (reason "expected_expr_close_round_or_comma"))
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
