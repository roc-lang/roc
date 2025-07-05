# META
~~~ini
description=multiline_str_apply_in_parens_pat
type=expr
~~~
# SOURCE
~~~roc
u ("""""" (0)):f
s
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `u` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpenRound(1:3-1:4),MultilineStringStart(1:4-1:7),StringPart(1:7-1:7),MultilineStringEnd(1:7-1:10),OpenRound(1:11-1:12),Int(1:12-1:13),CloseRound(1:13-1:14),CloseRound(1:14-1:15),OpColon(1:15-1:16),LowerIdent(1:16-1:17),Newline(1:1-1:1),
LowerIdent(2:1-2:2),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "u"))
~~~
# FORMATTED
~~~roc
u
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
