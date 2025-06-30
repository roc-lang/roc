# META
~~~ini
description=ability_demands_not_indented_with_first fail
type=expr
~~~
# SOURCE
~~~roc
MEq implements
    eq : a, a -> U64 where a implements MEq
        neq : a, a -> U64 where a implements MEq

1
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:4),KwImplements(1:5-1:15),Newline(1:1-1:1),
LowerIdent(2:5-2:7),OpColon(2:8-2:9),LowerIdent(2:10-2:11),Comma(2:11-2:12),LowerIdent(2:13-2:14),OpArrow(2:15-2:17),UpperIdent(2:18-2:21),KwWhere(2:22-2:27),LowerIdent(2:28-2:29),KwImplements(2:30-2:40),UpperIdent(2:41-2:44),Newline(1:1-1:1),
LowerIdent(3:9-3:12),OpColon(3:13-3:14),LowerIdent(3:15-3:16),Comma(3:16-3:17),LowerIdent(3:18-3:19),OpArrow(3:20-3:22),UpperIdent(3:23-3:26),KwWhere(3:27-3:32),LowerIdent(3:33-3:34),KwImplements(3:35-3:45),UpperIdent(3:46-3:49),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.4 (raw "MEq"))
~~~
# FORMATTED
~~~roc
MEq
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.4 (ext-var 73) (name "MEq") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[MEq]*"))
~~~
