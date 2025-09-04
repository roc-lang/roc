# META
~~~ini
description=unary_not
type=expr
~~~
# SOURCE
~~~roc
!blah
~~~
# EXPECTED
UNDEFINED VARIABLE - unary_not.md:1:2:1:6
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `blah` in this scope.
Is there an `import` or `exposing` missing up-top?

**unary_not.md:1:2:1:6:**
```roc
!blah
```
 ^^^^


# TOKENS
~~~zig
OpBang(1:1-1:2),LowerIdent(1:2-1:6),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(unary "!"
	(e-ident @1.2-1.6 (raw "blah")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-unary-not @1.1-1.6
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "Error"))
~~~
