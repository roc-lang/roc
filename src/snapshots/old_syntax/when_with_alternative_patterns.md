# META
~~~ini
description=when_with_alternative_patterns
type=expr
~~~
# SOURCE
~~~roc
when x is
 "blah" | "blop" -> 1
 "foo" |
  "bar"
 |"baz" -> 2
 "stuff" -> 4
~~~
# EXPECTED
UNDEFINED VARIABLE - when_with_alternative_patterns.md:1:1:1:5
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
StringStart(2:2-2:3),StringPart(2:3-2:7),StringEnd(2:7-2:8),OpBar(2:9-2:10),StringStart(2:11-2:12),StringPart(2:12-2:16),StringEnd(2:16-2:17),OpArrow(2:18-2:20),Int(2:21-2:22),Newline(1:1-1:1),
StringStart(3:2-3:3),StringPart(3:3-3:6),StringEnd(3:6-3:7),OpBar(3:8-3:9),Newline(1:1-1:1),
StringStart(4:3-4:4),StringPart(4:4-4:7),StringEnd(4:7-4:8),Newline(1:1-1:1),
OpBar(5:2-5:3),StringStart(5:3-5:4),StringPart(5:4-5:7),StringEnd(5:7-5:8),OpArrow(5:9-5:11),Int(5:12-5:13),Newline(1:1-1:1),
StringStart(6:2-6:3),StringPart(6:3-6:8),StringEnd(6:8-6:9),OpArrow(6:10-6:12),Int(6:13-6:14),EndOfFile(6:14-6:14),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "when"))
~~~
# FORMATTED
~~~roc
when
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Error"))
~~~
