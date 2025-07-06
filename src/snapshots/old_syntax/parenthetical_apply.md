# META
~~~ini
description=parenthetical_apply
type=expr
~~~
# SOURCE
~~~roc
(whee) 1
~~~
# EXPECTED
UNDEFINED VARIABLE - parenthetical_apply.md:1:2:1:6
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `whee` in this scope.
Is there an `import` or `exposing` missing up-top?

**parenthetical_apply.md:1:2:1:6:**
```roc
(whee) 1
```
 ^^^^


# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:2-1:6),CloseRound(1:6-1:7),Int(1:8-1:9),EndOfFile(1:9-1:9),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-1.7
	(e-ident @1.2-1.6 (raw "whee")))
~~~
# FORMATTED
~~~roc
(whee)
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.2-1.6 (type "Error"))
~~~
