# META
~~~ini
description=if_bang_then_else_one_line
type=expr
~~~
# SOURCE
~~~roc
f=if!b!then""else
 e
""
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:2-1:3),LowerIdent(1:3-1:12),StringStart(1:12-1:13),StringPart(1:13-1:13),StringEnd(1:13-1:14),KwElse(1:14-1:18),Newline(1:1-1:1),
LowerIdent(2:2-2:3),Newline(1:1-1:1),
StringStart(3:1-3:2),StringPart(3:2-3:2),StringEnd(3:2-3:3),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "f"))
~~~
# FORMATTED
~~~roc
f
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
