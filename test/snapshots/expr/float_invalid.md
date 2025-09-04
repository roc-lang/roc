# META
~~~ini
description=Invalid float literal too many decimal points
type=expr
~~~
# SOURCE
~~~roc
3.14.15
~~~
# EXPECTED
PARSE ERROR - float_invalid.md:1:5:1:8
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expr_no_space_dot_int`
This is an unexpected parsing error. Please check your syntax.

**float_invalid.md:1:5:1:8:**
```roc
3.14.15
```
    ^^^


# TOKENS
~~~zig
Float(1:1-1:5),NoSpaceDotInt(1:5-1:8),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-malformed @1.5-1.8 (reason "expr_no_space_dot_int"))
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
