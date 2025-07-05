# META
~~~ini
description=multiline_str_and_str_in_alias
type=expr
~~~
# SOURCE
~~~roc
8
("""""""")f:C
U
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),Newline(1:1-1:1),
OpenRound(2:1-2:2),MultilineStringStart(2:2-2:5),StringPart(2:5-2:5),MultilineStringEnd(2:5-2:8),StringStart(2:8-2:9),StringPart(2:9-2:9),StringEnd(2:9-2:10),CloseRound(2:10-2:11),LowerIdent(2:11-2:12),OpColon(2:12-2:13),UpperIdent(2:13-2:14),Newline(1:1-1:1),
UpperIdent(3:1-3:2),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.2 (raw "8"))
~~~
# FORMATTED
~~~roc
8
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.2 (value "8"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Num(*)"))
~~~
