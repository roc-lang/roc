# META
~~~ini
description=dbg_stmt_two_exprs
type=expr
~~~
# SOURCE
~~~roc
dbg
 (q
    qt)
g
 qt
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**dbg_stmt_two_exprs.md:3:7:3:7:**
```roc
    qt)
```
      


**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize dbg expression
Let us know if you want to help!

# TOKENS
~~~zig
KwDbg(1:1-1:4),Newline(1:1-1:1),
OpenRound(2:2-2:3),LowerIdent(2:3-2:4),Newline(1:1-1:1),
LowerIdent(3:5-3:7),CloseRound(3:7-3:8),Newline(1:1-1:1),
LowerIdent(4:1-4:2),Newline(1:1-1:1),
LowerIdent(5:2-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(e-dbg
	(e-malformed @1.1-1.1 (reason "expected_expr_close_round_or_comma")))
~~~
# FORMATTED
~~~roc
dbg 
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Error"))
~~~
