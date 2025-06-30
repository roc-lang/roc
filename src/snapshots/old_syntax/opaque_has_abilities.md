# META
~~~ini
description=opaque_has_abilities
type=expr
~~~
# SOURCE
~~~roc
A := U8 implements [Eq, Hash]

A := a where a implements Other implements [Eq, Hash]

A := a where a implements Other
     implements [Eq, Hash]

A := U8 implements [Eq {eq}, Hash {hash}]

A := U8 implements [Eq {eq, eq1}]

A := U8 implements [Eq {eq, eq1}, Hash]

A := U8 implements [Hash, Eq {eq, eq1}]

A := U8 implements []

A := a where a implements Other
     implements [Eq {eq}, Hash {hash}]

A := U8 implements [Eq {}]

0
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColonEqual(1:3-1:5),UpperIdent(1:6-1:8),KwImplements(1:9-1:19),OpenSquare(1:20-1:21),UpperIdent(1:21-1:23),Comma(1:23-1:24),UpperIdent(1:25-1:29),CloseSquare(1:29-1:30),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:2),OpColonEqual(3:3-3:5),LowerIdent(3:6-3:7),KwWhere(3:8-3:13),LowerIdent(3:14-3:15),KwImplements(3:16-3:26),UpperIdent(3:27-3:32),KwImplements(3:33-3:43),OpenSquare(3:44-3:45),UpperIdent(3:45-3:47),Comma(3:47-3:48),UpperIdent(3:49-3:53),CloseSquare(3:53-3:54),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(5:1-5:2),OpColonEqual(5:3-5:5),LowerIdent(5:6-5:7),KwWhere(5:8-5:13),LowerIdent(5:14-5:15),KwImplements(5:16-5:26),UpperIdent(5:27-5:32),Newline(1:1-1:1),
KwImplements(6:6-6:16),OpenSquare(6:17-6:18),UpperIdent(6:18-6:20),Comma(6:20-6:21),UpperIdent(6:22-6:26),CloseSquare(6:26-6:27),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(8:1-8:2),OpColonEqual(8:3-8:5),UpperIdent(8:6-8:8),KwImplements(8:9-8:19),OpenSquare(8:20-8:21),UpperIdent(8:21-8:23),OpenCurly(8:24-8:25),LowerIdent(8:25-8:27),CloseCurly(8:27-8:28),Comma(8:28-8:29),UpperIdent(8:30-8:34),OpenCurly(8:35-8:36),LowerIdent(8:36-8:40),CloseCurly(8:40-8:41),CloseSquare(8:41-8:42),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(10:1-10:2),OpColonEqual(10:3-10:5),UpperIdent(10:6-10:8),KwImplements(10:9-10:19),OpenSquare(10:20-10:21),UpperIdent(10:21-10:23),OpenCurly(10:24-10:25),LowerIdent(10:25-10:27),Comma(10:27-10:28),LowerIdent(10:29-10:32),CloseCurly(10:32-10:33),CloseSquare(10:33-10:34),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(12:1-12:2),OpColonEqual(12:3-12:5),UpperIdent(12:6-12:8),KwImplements(12:9-12:19),OpenSquare(12:20-12:21),UpperIdent(12:21-12:23),OpenCurly(12:24-12:25),LowerIdent(12:25-12:27),Comma(12:27-12:28),LowerIdent(12:29-12:32),CloseCurly(12:32-12:33),Comma(12:33-12:34),UpperIdent(12:35-12:39),CloseSquare(12:39-12:40),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(14:1-14:2),OpColonEqual(14:3-14:5),UpperIdent(14:6-14:8),KwImplements(14:9-14:19),OpenSquare(14:20-14:21),UpperIdent(14:21-14:25),Comma(14:25-14:26),UpperIdent(14:27-14:29),OpenCurly(14:30-14:31),LowerIdent(14:31-14:33),Comma(14:33-14:34),LowerIdent(14:35-14:38),CloseCurly(14:38-14:39),CloseSquare(14:39-14:40),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(16:1-16:2),OpColonEqual(16:3-16:5),UpperIdent(16:6-16:8),KwImplements(16:9-16:19),OpenSquare(16:20-16:21),CloseSquare(16:21-16:22),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(18:1-18:2),OpColonEqual(18:3-18:5),LowerIdent(18:6-18:7),KwWhere(18:8-18:13),LowerIdent(18:14-18:15),KwImplements(18:16-18:26),UpperIdent(18:27-18:32),Newline(1:1-1:1),
KwImplements(19:6-19:16),OpenSquare(19:17-19:18),UpperIdent(19:18-19:20),OpenCurly(19:21-19:22),LowerIdent(19:22-19:24),CloseCurly(19:24-19:25),Comma(19:25-19:26),UpperIdent(19:27-19:31),OpenCurly(19:32-19:33),LowerIdent(19:33-19:37),CloseCurly(19:37-19:38),CloseSquare(19:38-19:39),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(21:1-21:2),OpColonEqual(21:3-21:5),UpperIdent(21:6-21:8),KwImplements(21:9-21:19),OpenSquare(21:20-21:21),UpperIdent(21:21-21:23),OpenCurly(21:24-21:25),CloseCurly(21:25-21:26),CloseSquare(21:26-21:27),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(23:1-23:2),EndOfFile(23:2-23:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "A"))
~~~
# FORMATTED
~~~roc
A
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "A") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[A]*"))
~~~
