# META
~~~ini
description=underscore_in_assignment_pattern
type=expr
~~~
# SOURCE
~~~roc
Pair x _ = Pair 0 1
Pair _ y = Pair 0 1
Pair _ _ = Pair 0 1
_ = Pair 0 1
Pair (Pair x _) (Pair _ y) = Pair (Pair 0 1) (Pair 2 3)

0
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:5),LowerIdent(1:6-1:7),Underscore(1:8-1:9),OpAssign(1:10-1:11),UpperIdent(1:12-1:16),Int(1:17-1:18),Int(1:19-1:20),Newline(1:1-1:1),
UpperIdent(2:1-2:5),Underscore(2:6-2:7),LowerIdent(2:8-2:9),OpAssign(2:10-2:11),UpperIdent(2:12-2:16),Int(2:17-2:18),Int(2:19-2:20),Newline(1:1-1:1),
UpperIdent(3:1-3:5),Underscore(3:6-3:7),Underscore(3:8-3:9),OpAssign(3:10-3:11),UpperIdent(3:12-3:16),Int(3:17-3:18),Int(3:19-3:20),Newline(1:1-1:1),
Underscore(4:1-4:2),OpAssign(4:3-4:4),UpperIdent(4:5-4:9),Int(4:10-4:11),Int(4:12-4:13),Newline(1:1-1:1),
UpperIdent(5:1-5:5),OpenRound(5:6-5:7),UpperIdent(5:7-5:11),LowerIdent(5:12-5:13),Underscore(5:14-5:15),CloseRound(5:15-5:16),OpenRound(5:17-5:18),UpperIdent(5:18-5:22),Underscore(5:23-5:24),LowerIdent(5:25-5:26),CloseRound(5:26-5:27),OpAssign(5:28-5:29),UpperIdent(5:30-5:34),OpenRound(5:35-5:36),UpperIdent(5:36-5:40),Int(5:41-5:42),Int(5:43-5:44),CloseRound(5:44-5:45),OpenRound(5:46-5:47),UpperIdent(5:47-5:51),Int(5:52-5:53),Int(5:54-5:55),CloseRound(5:55-5:56),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(7:1-7:2),EndOfFile(7:2-7:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.5 (raw "Pair"))
~~~
# FORMATTED
~~~roc
Pair
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.5 (ext-var 73) (name "Pair") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[Pair]*"))
~~~
