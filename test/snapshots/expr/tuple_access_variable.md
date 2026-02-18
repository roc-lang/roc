# META
~~~ini
description=Tuple access on a variable
type=expr
~~~
# SOURCE
~~~roc
t.0
~~~
# EXPECTED
UNDEFINED VARIABLE - tuple_access_variable.md:1:1:1:2
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `t` in this scope.
Is there an `import` or `exposing` missing up-top?

**tuple_access_variable.md:1:1:1:2:**
```roc
t.0
```
^


# TOKENS
~~~zig
LowerIdent,NoSpaceDotInt,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple-access
	(e-ident (raw "t"))
	".0")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple-access (index "0")
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
