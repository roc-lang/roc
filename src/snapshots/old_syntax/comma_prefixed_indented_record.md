# META
~~~ini
description=comma_prefixed_indented_record
type=expr
~~~
# SOURCE
~~~roc
Model position :
    { evaluated : Set position
    , openSet : Set position
    , costs : Dict.Dict position F64
    , cameFrom : Dict.Dict position position
    }

a
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:6),LowerIdent(1:7-1:15),OpColon(1:16-1:17),Newline(1:1-1:1),
OpenCurly(2:5-2:6),LowerIdent(2:7-2:16),OpColon(2:17-2:18),UpperIdent(2:19-2:22),LowerIdent(2:23-2:31),Newline(1:1-1:1),
Comma(3:5-3:6),LowerIdent(3:7-3:14),OpColon(3:15-3:16),UpperIdent(3:17-3:20),LowerIdent(3:21-3:29),Newline(1:1-1:1),
Comma(4:5-4:6),LowerIdent(4:7-4:12),OpColon(4:13-4:14),UpperIdent(4:15-4:19),NoSpaceDotUpperIdent(4:19-4:24),LowerIdent(4:25-4:33),UpperIdent(4:34-4:37),Newline(1:1-1:1),
Comma(5:5-5:6),LowerIdent(5:7-5:15),OpColon(5:16-5:17),UpperIdent(5:18-5:22),NoSpaceDotUpperIdent(5:22-5:27),LowerIdent(5:28-5:36),LowerIdent(5:37-5:45),Newline(1:1-1:1),
CloseCurly(6:5-6:6),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(8:1-8:2),EndOfFile(8:2-8:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.6 (raw "Model"))
~~~
# FORMATTED
~~~roc
Model
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.6 (name "Model"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "[Model]*"))
~~~
