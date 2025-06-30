# META
~~~ini
description=multiline_str_in_pat
type=expr
~~~
# SOURCE
~~~roc
1"""""""^"2:A
""
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),MultilineStringStart(1:2-1:5),StringPart(1:5-1:5),MultilineStringEnd(1:5-1:8),StringStart(1:8-1:9),StringPart(1:9-1:10),StringEnd(1:10-1:11),Int(1:11-1:12),OpColon(1:12-1:13),UpperIdent(1:13-1:14),Newline(1:1-1:1),
StringStart(2:1-2:2),StringPart(2:2-2:2),StringEnd(2:2-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.2 (raw "1"))
~~~
# FORMATTED
~~~roc
1
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.2 (value "1"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Num(*)"))
~~~
