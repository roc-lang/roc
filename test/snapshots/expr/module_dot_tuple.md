# META
~~~ini
description=Module dot malformed (should error)
type=expr
~~~
# SOURCE
~~~roc
I.5
~~~
# EXPECTED
PARSE ERROR - module_dot_tuple.md:1:2:1:4
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expr_no_space_dot_int`
This is an unexpected parsing error. Please check your syntax.

**module_dot_tuple.md:1:2:1:4:**
```roc
I.5
```
 ^^


# TOKENS
~~~zig
UpperIdent(1:1-1:2),NoSpaceDotInt(1:2-1:4),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-malformed @1.2-1.4 (reason "expr_no_space_dot_int"))
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
