# META
~~~ini
description=nested_tuples_annotation_terrible_perf fail
type=expr
~~~
# SOURCE
~~~roc
.:(i,i,(i,(i,ii,(i,(i,(i,i,(i,(i,i,(i,(J(i,(i,(i,(i,(i,i,(i,(i,i,(i,(i,(i,(J[]
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - nested_tuples_annotation_terrible_perf.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **.:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nested_tuples_annotation_terrible_perf.md:1:1:1:3:**
```roc
.:(i,i,(i,(i,ii,(i,(i,(i,i,(i,(i,i,(i,(J(i,(i,(i,(i,(i,i,(i,(i,i,(i,(i,(i,(J[]
```
^^


# TOKENS
~~~zig
Dot(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),LowerIdent(1:4-1:5),Comma(1:5-1:6),LowerIdent(1:6-1:7),Comma(1:7-1:8),NoSpaceOpenRound(1:8-1:9),LowerIdent(1:9-1:10),Comma(1:10-1:11),NoSpaceOpenRound(1:11-1:12),LowerIdent(1:12-1:13),Comma(1:13-1:14),LowerIdent(1:14-1:16),Comma(1:16-1:17),NoSpaceOpenRound(1:17-1:18),LowerIdent(1:18-1:19),Comma(1:19-1:20),NoSpaceOpenRound(1:20-1:21),LowerIdent(1:21-1:22),Comma(1:22-1:23),NoSpaceOpenRound(1:23-1:24),LowerIdent(1:24-1:25),Comma(1:25-1:26),LowerIdent(1:26-1:27),Comma(1:27-1:28),NoSpaceOpenRound(1:28-1:29),LowerIdent(1:29-1:30),Comma(1:30-1:31),NoSpaceOpenRound(1:31-1:32),LowerIdent(1:32-1:33),Comma(1:33-1:34),LowerIdent(1:34-1:35),Comma(1:35-1:36),NoSpaceOpenRound(1:36-1:37),LowerIdent(1:37-1:38),Comma(1:38-1:39),NoSpaceOpenRound(1:39-1:40),UpperIdent(1:40-1:41),NoSpaceOpenRound(1:41-1:42),LowerIdent(1:42-1:43),Comma(1:43-1:44),NoSpaceOpenRound(1:44-1:45),LowerIdent(1:45-1:46),Comma(1:46-1:47),NoSpaceOpenRound(1:47-1:48),LowerIdent(1:48-1:49),Comma(1:49-1:50),NoSpaceOpenRound(1:50-1:51),LowerIdent(1:51-1:52),Comma(1:52-1:53),NoSpaceOpenRound(1:53-1:54),LowerIdent(1:54-1:55),Comma(1:55-1:56),LowerIdent(1:56-1:57),Comma(1:57-1:58),NoSpaceOpenRound(1:58-1:59),LowerIdent(1:59-1:60),Comma(1:60-1:61),NoSpaceOpenRound(1:61-1:62),LowerIdent(1:62-1:63),Comma(1:63-1:64),LowerIdent(1:64-1:65),Comma(1:65-1:66),NoSpaceOpenRound(1:66-1:67),LowerIdent(1:67-1:68),Comma(1:68-1:69),NoSpaceOpenRound(1:69-1:70),LowerIdent(1:70-1:71),Comma(1:71-1:72),NoSpaceOpenRound(1:72-1:73),LowerIdent(1:73-1:74),Comma(1:74-1:75),NoSpaceOpenRound(1:75-1:76),UpperIdent(1:76-1:77),OpenSquare(1:77-1:78),CloseSquare(1:78-1:79),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.3 (reason "expr_unexpected_token"))
~~~
# FORMATTED
~~~roc

~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
