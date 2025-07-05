# META
~~~ini
description=newline_before_operator_with_defs fail
type=expr
~~~
# SOURCE
~~~roc
7
==(Q:c 42)
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**newline_before_operator_with_defs.md:2:10:2:11:**
```roc
==(Q:c 42)
```
         ^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

# TOKENS
~~~zig
Int(1:1-1:2),Newline(1:1-1:1),
OpEquals(2:1-2:3),NoSpaceOpenRound(2:3-2:4),UpperIdent(2:4-2:5),OpColon(2:5-2:6),LowerIdent(2:6-2:7),Int(2:8-2:10),CloseRound(2:10-2:11),EndOfFile(2:11-2:11),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.11 (op "==")
	(e-int @1.1-1.2 (raw "7"))
	(e-malformed @2.10-2.11 (reason "expected_expr_close_round_or_comma")))
~~~
# FORMATTED
~~~roc
7
	== 
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.11 (op "eq")
	(e-int @1.1-1.2 (value "7"))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.11 (type "*"))
~~~
