# META
~~~ini
description=quotes_in_parens_in_pat malformed
type=expr
~~~
# SOURCE
~~~roc
Q (""""""""):a
q
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpenRound(1:3-1:4),MultilineStringStart(1:4-1:7),StringPart(1:7-1:7),MultilineStringEnd(1:7-1:10),StringStart(1:10-1:11),StringPart(1:11-1:11),StringEnd(1:11-1:12),CloseRound(1:12-1:13),OpColon(1:13-1:14),LowerIdent(1:14-1:15),Newline(1:1-1:1),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "Q"))
~~~
# FORMATTED
~~~roc
Q
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (ext-var 73) (name "Q") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[Q]*"))
~~~
