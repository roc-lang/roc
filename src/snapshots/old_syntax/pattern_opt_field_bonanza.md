# META
~~~ini
description=pattern_opt_field_bonanza malformed
type=expr
~~~
# SOURCE
~~~roc
M{s?s{J&
}}{s?s{J&
}}:p
y
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:3-1:4),NoSpaceOpQuestion(1:4-1:5),LowerIdent(1:5-1:6),OpenCurly(1:6-1:7),UpperIdent(1:7-1:8),OpAmpersand(1:8-1:9),Newline(1:1-1:1),
CloseCurly(2:1-2:2),CloseCurly(2:2-2:3),OpenCurly(2:3-2:4),LowerIdent(2:4-2:5),NoSpaceOpQuestion(2:5-2:6),LowerIdent(2:6-2:7),OpenCurly(2:7-2:8),UpperIdent(2:8-2:9),OpAmpersand(2:9-2:10),Newline(1:1-1:1),
CloseCurly(3:1-3:2),CloseCurly(3:2-3:3),OpColon(3:3-3:4),LowerIdent(3:4-3:5),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "M"))
~~~
# FORMATTED
~~~roc
M
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "M") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[M]a"))
~~~
