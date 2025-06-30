# META
~~~ini
description=type_decl_with_underscore
type=expr
~~~
# SOURCE
~~~roc
doStuff : UserId -> Dict Str _
42
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `doStuff` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:8),OpColon(1:9-1:10),UpperIdent(1:11-1:17),OpArrow(1:18-1:20),UpperIdent(1:21-1:25),UpperIdent(1:26-1:29),Underscore(1:30-1:31),Newline(1:1-1:1),
Int(2:1-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.8 (qaul "") (raw "doStuff"))
~~~
# FORMATTED
~~~roc
doStuff
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.8 (type "Error"))
~~~
