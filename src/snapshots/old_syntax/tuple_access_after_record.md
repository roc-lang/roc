# META
~~~ini
description=tuple_access_after_record
type=expr
~~~
# SOURCE
~~~roc
{ a: (1, 2) }.a.0
~~~
# EXPECTED
PARSE ERROR - tuple_access_after_record.md:1:16:1:18
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expr_no_space_dot_int`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**tuple_access_after_record.md:1:16:1:18:**
```roc
{ a: (1, 2) }.a.0
```
               ^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:4),OpColon(1:4-1:5),OpenRound(1:6-1:7),Int(1:7-1:8),Comma(1:8-1:9),Int(1:10-1:11),CloseRound(1:11-1:12),CloseCurly(1:13-1:14),NoSpaceDotLowerIdent(1:14-1:16),NoSpaceDotInt(1:16-1:18),EndOfFile(1:18-1:18),
~~~
# PARSE
~~~clojure
(e-malformed @1.16-1.18 (reason "expr_no_space_dot_int"))
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
