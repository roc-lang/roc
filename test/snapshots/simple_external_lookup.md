# META
~~~ini
description=Simple external declaration lookup
type=expr
~~~
# SOURCE
~~~roc
List.map
~~~
# EXPECTED
DOES NOT EXIST - simple_external_lookup.md:1:1:1:9
# PROBLEMS
**DOES NOT EXIST**
`List.map` does not exist.

**simple_external_lookup.md:1:1:1:9:**
```roc
List.map
```
^^^^^^^^


# TOKENS
~~~zig
UpperIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-ident (raw "List.map"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "qualified_ident_does_not_exist"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
