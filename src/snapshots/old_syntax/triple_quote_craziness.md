# META
~~~ini
description=triple_quote_craziness
type=expr
~~~
# SOURCE
~~~roc
H""""""=f""""""
f!
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),MultilineStringStart(1:2-1:5),StringPart(1:5-1:5),MultilineStringEnd(1:5-1:8),OpAssign(1:8-1:9),LowerIdent(1:9-1:10),MultilineStringStart(1:10-1:13),StringPart(1:13-1:13),MultilineStringEnd(1:13-1:16),Newline(1:1-1:1),
LowerIdent(2:1-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "H"))
~~~
# FORMATTED
~~~roc
H
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "H"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[H]*"))
~~~
