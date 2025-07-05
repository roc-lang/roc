# META
~~~ini
description=call_bang
type=expr
~~~
# SOURCE
~~~roc
launchTheNukes! 123
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `launchTheNukes!` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:16),Int(1:17-1:20),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.16 (raw "launchTheNukes!"))
~~~
# FORMATTED
~~~roc
launchTheNukes!
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.16 (type "Error"))
~~~
