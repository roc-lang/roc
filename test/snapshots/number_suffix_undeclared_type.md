# META
~~~ini
description=Number with type suffix that is not in scope
type=expr
~~~
# SOURCE
~~~roc
0.F
~~~
# EXPECTED
UNDECLARED TYPE - number_suffix_undeclared_type.md:1:1:1:4
# PROBLEMS
**UNDECLARED TYPE**
The type _F_ is not declared in this scope.

This type is referenced here:
**number_suffix_undeclared_type.md:1:1:1:4:**
```roc
0.F
```
^^^


# TOKENS
~~~zig
Int,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-typed-int (raw "0") (type ".F"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "undeclared_type"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
