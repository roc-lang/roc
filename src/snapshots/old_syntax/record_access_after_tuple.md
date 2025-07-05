# META
~~~ini
description=record_access_after_tuple
type=expr
~~~
# SOURCE
~~~roc
({a: 0}, {b: 1}).0.a
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expr_no_space_dot_int`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**record_access_after_tuple.md:1:17:1:21:**
```roc
({a: 0}, {b: 1}).0.a
```
                ^^^^


# TOKENS
~~~zig
OpenRound(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:3-1:4),OpColon(1:4-1:5),Int(1:6-1:7),CloseCurly(1:7-1:8),Comma(1:8-1:9),OpenCurly(1:10-1:11),LowerIdent(1:11-1:12),OpColon(1:12-1:13),Int(1:14-1:15),CloseCurly(1:15-1:16),CloseRound(1:16-1:17),NoSpaceDotInt(1:17-1:19),NoSpaceDotLowerIdent(1:19-1:21),EndOfFile(1:21-1:21),
~~~
# PARSE
~~~clojure
(e-malformed @1.17-1.21 (reason "expr_no_space_dot_int"))
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
