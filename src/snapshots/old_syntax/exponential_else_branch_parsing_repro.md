# META
~~~ini
description=exponential_else_branch_parsing_repro fail
type=expr
~~~
# SOURCE
~~~roc
ee&&i!efs@&exp5/osif.!see&e@&ex/if.!see&&ifss@osif.!see&e@&ex/if.!see&&ifss@5/if.!if.!&ex/if.!see&&ifss@5/if.!is5/if.osif.!see&e@&ex/if.!see&&ifss@5/if.!if.!xpAs5/if.!see&e@&e&&ifss@5/if.!is5/if.osif.!see&e@&ex/if.!see&&ifss@5/if.!if.!xpAs5/if.!see&e@&expos5/if.!if.!poxpos5/if.!if.!pos5/if.!xp#'
then#f#,w
{
}&&ifs!s-

t__T+_____^_zese
else
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `ee` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:3),OpAmpersand(1:3-1:4),OpAmpersand(1:4-1:5),LowerIdent(1:5-1:10),MalformedOpaqueNameWithoutName(1:10-1:11),OpAmpersand(1:11-1:12),LowerIdent(1:12-1:16),OpSlash(1:16-1:17),LowerIdent(1:17-1:21),Dot(1:21-1:22),OpBang(1:22-1:23),LowerIdent(1:23-1:26),OpAmpersand(1:26-1:27),LowerIdent(1:27-1:28),MalformedOpaqueNameWithoutName(1:28-1:29),OpAmpersand(1:29-1:30),LowerIdent(1:30-1:32),OpSlash(1:32-1:33),KwIf(1:33-1:35),Dot(1:35-1:36),OpBang(1:36-1:37),LowerIdent(1:37-1:40),OpAmpersand(1:40-1:41),OpAmpersand(1:41-1:42),LowerIdent(1:42-1:46),OpaqueName(1:46-1:51),Dot(1:51-1:52),OpBang(1:52-1:53),LowerIdent(1:53-1:56),OpAmpersand(1:56-1:57),LowerIdent(1:57-1:58),MalformedOpaqueNameWithoutName(1:58-1:59),OpAmpersand(1:59-1:60),LowerIdent(1:60-1:62),OpSlash(1:62-1:63),KwIf(1:63-1:65),Dot(1:65-1:66),OpBang(1:66-1:67),LowerIdent(1:67-1:70),OpAmpersand(1:70-1:71),OpAmpersand(1:71-1:72),LowerIdent(1:72-1:76),OpaqueName(1:76-1:78),OpSlash(1:78-1:79),KwIf(1:79-1:81),Dot(1:81-1:82),OpBang(1:82-1:83),KwIf(1:83-1:85),Dot(1:85-1:86),OpBang(1:86-1:87),OpAmpersand(1:87-1:88),LowerIdent(1:88-1:90),OpSlash(1:90-1:91),KwIf(1:91-1:93),Dot(1:93-1:94),OpBang(1:94-1:95),LowerIdent(1:95-1:98),OpAmpersand(1:98-1:99),OpAmpersand(1:99-1:100),LowerIdent(1:100-1:104),OpaqueName(1:104-1:106),OpSlash(1:106-1:107),KwIf(1:107-1:109),Dot(1:109-1:110),OpBang(1:110-1:111),LowerIdent(1:111-1:114),OpSlash(1:114-1:115),KwIf(1:115-1:117),NoSpaceDotLowerIdent(1:117-1:122),Dot(1:122-1:123),OpBang(1:123-1:124),LowerIdent(1:124-1:127),OpAmpersand(1:127-1:128),LowerIdent(1:128-1:129),MalformedOpaqueNameWithoutName(1:129-1:130),OpAmpersand(1:130-1:131),LowerIdent(1:131-1:133),OpSlash(1:133-1:134),KwIf(1:134-1:136),Dot(1:136-1:137),OpBang(1:137-1:138),LowerIdent(1:138-1:141),OpAmpersand(1:141-1:142),OpAmpersand(1:142-1:143),LowerIdent(1:143-1:147),OpaqueName(1:147-1:149),OpSlash(1:149-1:150),KwIf(1:150-1:152),Dot(1:152-1:153),OpBang(1:153-1:154),KwIf(1:154-1:156),Dot(1:156-1:157),OpBang(1:157-1:158),LowerIdent(1:158-1:163),OpSlash(1:163-1:164),KwIf(1:164-1:166),Dot(1:166-1:167),OpBang(1:167-1:168),LowerIdent(1:168-1:171),OpAmpersand(1:171-1:172),LowerIdent(1:172-1:173),MalformedOpaqueNameWithoutName(1:173-1:174),OpAmpersand(1:174-1:175),LowerIdent(1:175-1:176),OpAmpersand(1:176-1:177),OpAmpersand(1:177-1:178),LowerIdent(1:178-1:182),OpaqueName(1:182-1:184),OpSlash(1:184-1:185),KwIf(1:185-1:187),Dot(1:187-1:188),OpBang(1:188-1:189),LowerIdent(1:189-1:192),OpSlash(1:192-1:193),KwIf(1:193-1:195),NoSpaceDotLowerIdent(1:195-1:200),Dot(1:200-1:201),OpBang(1:201-1:202),LowerIdent(1:202-1:205),OpAmpersand(1:205-1:206),LowerIdent(1:206-1:207),MalformedOpaqueNameWithoutName(1:207-1:208),OpAmpersand(1:208-1:209),LowerIdent(1:209-1:211),OpSlash(1:211-1:212),KwIf(1:212-1:214),Dot(1:214-1:215),OpBang(1:215-1:216),LowerIdent(1:216-1:219),OpAmpersand(1:219-1:220),OpAmpersand(1:220-1:221),LowerIdent(1:221-1:225),OpaqueName(1:225-1:227),OpSlash(1:227-1:228),KwIf(1:228-1:230),Dot(1:230-1:231),OpBang(1:231-1:232),KwIf(1:232-1:234),Dot(1:234-1:235),OpBang(1:235-1:236),LowerIdent(1:236-1:241),OpSlash(1:241-1:242),KwIf(1:242-1:244),Dot(1:244-1:245),OpBang(1:245-1:246),LowerIdent(1:246-1:249),OpAmpersand(1:249-1:250),LowerIdent(1:250-1:251),MalformedOpaqueNameWithoutName(1:251-1:252),OpAmpersand(1:252-1:253),LowerIdent(1:253-1:259),OpSlash(1:259-1:260),KwIf(1:260-1:262),Dot(1:262-1:263),OpBang(1:263-1:264),KwIf(1:264-1:266),Dot(1:266-1:267),OpBang(1:267-1:268),LowerIdent(1:268-1:275),OpSlash(1:275-1:276),KwIf(1:276-1:278),Dot(1:278-1:279),OpBang(1:279-1:280),KwIf(1:280-1:282),Dot(1:282-1:283),OpBang(1:283-1:284),LowerIdent(1:284-1:288),OpSlash(1:288-1:289),KwIf(1:289-1:291),Dot(1:291-1:292),OpBang(1:292-1:293),LowerIdent(1:293-1:295),Newline(1:296-1:297),
LowerIdent(2:1-2:5),Newline(2:6-2:10),
OpenCurly(3:1-3:2),Newline(1:1-1:1),
CloseCurly(4:1-4:2),OpAmpersand(4:2-4:3),OpAmpersand(4:3-4:4),LowerIdent(4:4-4:9),OpBinaryMinus(4:9-4:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:5),OpPlus(6:5-6:6),Underscore(6:6-6:7),Underscore(6:7-6:8),Underscore(6:8-6:9),Underscore(6:9-6:10),Underscore(6:10-6:11),OpCaret(6:11-6:12),NamedUnderscore(6:12-6:17),Newline(1:1-1:1),
KwElse(7:1-7:5),EndOfFile(7:5-7:5),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.3 (raw "ee"))
~~~
# FORMATTED
~~~roc
ee
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.3 (type "Error"))
~~~
