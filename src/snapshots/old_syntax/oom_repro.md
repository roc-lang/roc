# META
~~~ini
description=oom_repro fail
type=expr
~~~
# SOURCE
~~~roc
K:bMM,(
A0,AectAfects,(
A0,AenAA1M,(
A0,Ae,(
A0bMprovi0,A=>A1,MA,M,e,M,A ,A ,s,(
AbMM,(A0
,Ae,(
A0bMprovi0,A=>A1,MA,M,e,M,e8,ActAfects,(
A0,AenAA1M,(
A0,Ae,(
A0bMprovi0,A=>A1,MA,M,e,M,A ,A ,s,(
AbMM,(A0
,Ae,(
A0bMprovi0,A=>A1,MA,M,e,M,e8,A)WM,ulsee,M,e8,A)db1,MA,M,e,M,e8,A)WM,ulsee,M,e8,A)dbgMA,MO,e,M,e4,A)WA)WenA1)WM,ulsee,M,e8,A)db1,MA,M,e,M,e8,A)WM,ulsee,M,e8,A)dbgMA,MO,e,M,e4,A)WA)WenA1,A)W
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),LowerIdent(1:3-1:6),Comma(1:6-1:7),NoSpaceOpenRound(1:7-1:8),Newline(1:1-1:1),
UpperIdent(2:1-2:3),Comma(2:3-2:4),UpperIdent(2:4-2:14),Comma(2:14-2:15),NoSpaceOpenRound(2:15-2:16),Newline(1:1-1:1),
UpperIdent(3:1-3:3),Comma(3:3-3:4),UpperIdent(3:4-3:11),Comma(3:11-3:12),NoSpaceOpenRound(3:12-3:13),Newline(1:1-1:1),
UpperIdent(4:1-4:3),Comma(4:3-4:4),UpperIdent(4:4-4:6),Comma(4:6-4:7),NoSpaceOpenRound(4:7-4:8),Newline(1:1-1:1),
UpperIdent(5:1-5:11),Comma(5:11-5:12),UpperIdent(5:12-5:13),OpFatArrow(5:13-5:15),UpperIdent(5:15-5:17),Comma(5:17-5:18),UpperIdent(5:18-5:20),Comma(5:20-5:21),UpperIdent(5:21-5:22),Comma(5:22-5:23),LowerIdent(5:23-5:24),Comma(5:24-5:25),UpperIdent(5:25-5:26),Comma(5:26-5:27),UpperIdent(5:27-5:28),Comma(5:29-5:30),UpperIdent(5:30-5:31),Comma(5:32-5:33),LowerIdent(5:33-5:34),Comma(5:34-5:35),NoSpaceOpenRound(5:35-5:36),Newline(1:1-1:1),
UpperIdent(6:1-6:5),Comma(6:5-6:6),NoSpaceOpenRound(6:6-6:7),UpperIdent(6:7-6:9),Newline(1:1-1:1),
Comma(7:1-7:2),UpperIdent(7:2-7:4),Comma(7:4-7:5),NoSpaceOpenRound(7:5-7:6),Newline(1:1-1:1),
UpperIdent(8:1-8:11),Comma(8:11-8:12),UpperIdent(8:12-8:13),OpFatArrow(8:13-8:15),UpperIdent(8:15-8:17),Comma(8:17-8:18),UpperIdent(8:18-8:20),Comma(8:20-8:21),UpperIdent(8:21-8:22),Comma(8:22-8:23),LowerIdent(8:23-8:24),Comma(8:24-8:25),UpperIdent(8:25-8:26),Comma(8:26-8:27),LowerIdent(8:27-8:29),Comma(8:29-8:30),UpperIdent(8:30-8:39),Comma(8:39-8:40),NoSpaceOpenRound(8:40-8:41),Newline(1:1-1:1),
UpperIdent(9:1-9:3),Comma(9:3-9:4),UpperIdent(9:4-9:11),Comma(9:11-9:12),NoSpaceOpenRound(9:12-9:13),Newline(1:1-1:1),
UpperIdent(10:1-10:3),Comma(10:3-10:4),UpperIdent(10:4-10:6),Comma(10:6-10:7),NoSpaceOpenRound(10:7-10:8),Newline(1:1-1:1),
UpperIdent(11:1-11:11),Comma(11:11-11:12),UpperIdent(11:12-11:13),OpFatArrow(11:13-11:15),UpperIdent(11:15-11:17),Comma(11:17-11:18),UpperIdent(11:18-11:20),Comma(11:20-11:21),UpperIdent(11:21-11:22),Comma(11:22-11:23),LowerIdent(11:23-11:24),Comma(11:24-11:25),UpperIdent(11:25-11:26),Comma(11:26-11:27),UpperIdent(11:27-11:28),Comma(11:29-11:30),UpperIdent(11:30-11:31),Comma(11:32-11:33),LowerIdent(11:33-11:34),Comma(11:34-11:35),NoSpaceOpenRound(11:35-11:36),Newline(1:1-1:1),
UpperIdent(12:1-12:5),Comma(12:5-12:6),NoSpaceOpenRound(12:6-12:7),UpperIdent(12:7-12:9),Newline(1:1-1:1),
Comma(13:1-13:2),UpperIdent(13:2-13:4),Comma(13:4-13:5),NoSpaceOpenRound(13:5-13:6),Newline(1:1-1:1),
UpperIdent(14:1-14:11),Comma(14:11-14:12),UpperIdent(14:12-14:13),OpFatArrow(14:13-14:15),UpperIdent(14:15-14:17),Comma(14:17-14:18),UpperIdent(14:18-14:20),Comma(14:20-14:21),UpperIdent(14:21-14:22),Comma(14:22-14:23),LowerIdent(14:23-14:24),Comma(14:24-14:25),UpperIdent(14:25-14:26),Comma(14:26-14:27),LowerIdent(14:27-14:29),Comma(14:29-14:30),UpperIdent(14:30-14:31),CloseRound(14:31-14:32),UpperIdent(14:32-14:34),Comma(14:34-14:35),LowerIdent(14:35-14:40),Comma(14:40-14:41),UpperIdent(14:41-14:42),Comma(14:42-14:43),LowerIdent(14:43-14:45),Comma(14:45-14:46),UpperIdent(14:46-14:47),CloseRound(14:47-14:48),LowerIdent(14:48-14:51),Comma(14:51-14:52),UpperIdent(14:52-14:54),Comma(14:54-14:55),UpperIdent(14:55-14:56),Comma(14:56-14:57),LowerIdent(14:57-14:58),Comma(14:58-14:59),UpperIdent(14:59-14:60),Comma(14:60-14:61),LowerIdent(14:61-14:63),Comma(14:63-14:64),UpperIdent(14:64-14:65),CloseRound(14:65-14:66),UpperIdent(14:66-14:68),Comma(14:68-14:69),LowerIdent(14:69-14:74),Comma(14:74-14:75),UpperIdent(14:75-14:76),Comma(14:76-14:77),LowerIdent(14:77-14:79),Comma(14:79-14:80),UpperIdent(14:80-14:81),CloseRound(14:81-14:82),LowerIdent(14:82-14:87),Comma(14:87-14:88),UpperIdent(14:88-14:90),Comma(14:90-14:91),LowerIdent(14:91-14:92),Comma(14:92-14:93),UpperIdent(14:93-14:94),Comma(14:94-14:95),LowerIdent(14:95-14:97),Comma(14:97-14:98),UpperIdent(14:98-14:99),CloseRound(14:99-14:100),UpperIdent(14:100-14:102),CloseRound(14:102-14:103),UpperIdent(14:103-14:108),CloseRound(14:108-14:109),UpperIdent(14:109-14:111),Comma(14:111-14:112),LowerIdent(14:112-14:117),Comma(14:117-14:118),UpperIdent(14:118-14:119),Comma(14:119-14:120),LowerIdent(14:120-14:122),Comma(14:122-14:123),UpperIdent(14:123-14:124),CloseRound(14:124-14:125),LowerIdent(14:125-14:128),Comma(14:128-14:129),UpperIdent(14:129-14:131),Comma(14:131-14:132),UpperIdent(14:132-14:133),Comma(14:133-14:134),LowerIdent(14:134-14:135),Comma(14:135-14:136),UpperIdent(14:136-14:137),Comma(14:137-14:138),LowerIdent(14:138-14:140),Comma(14:140-14:141),UpperIdent(14:141-14:142),CloseRound(14:142-14:143),UpperIdent(14:143-14:145),Comma(14:145-14:146),LowerIdent(14:146-14:151),Comma(14:151-14:152),UpperIdent(14:152-14:153),Comma(14:153-14:154),LowerIdent(14:154-14:156),Comma(14:156-14:157),UpperIdent(14:157-14:158),CloseRound(14:158-14:159),LowerIdent(14:159-14:164),Comma(14:164-14:165),UpperIdent(14:165-14:167),Comma(14:167-14:168),LowerIdent(14:168-14:169),Comma(14:169-14:170),UpperIdent(14:170-14:171),Comma(14:171-14:172),LowerIdent(14:172-14:174),Comma(14:174-14:175),UpperIdent(14:175-14:176),CloseRound(14:176-14:177),UpperIdent(14:177-14:179),CloseRound(14:179-14:180),UpperIdent(14:180-14:185),Comma(14:185-14:186),UpperIdent(14:186-14:187),CloseRound(14:187-14:188),UpperIdent(14:188-14:189),Newline(1:1-1:1),
MalformedUnknownToken(15:1-15:2),MalformedUnknownToken(15:2-15:3),MalformedUnknownToken(15:3-15:4),EndOfFile(15:4-15:4),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "K"))
~~~
# FORMATTED
~~~roc
K
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "K"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[K]*"))
~~~
