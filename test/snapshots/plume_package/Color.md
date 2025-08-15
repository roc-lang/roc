# META
~~~ini
description=Color module from package
type=package
~~~
# SOURCE
~~~roc
module [
    Color,
    to_str,
    rgb,
    rgba,
    hex,
    named,
]

Color := [
    RGB(U8, U8, U8),
    RGBA(U8, U8, U8, Dec),
    Named(Str),
    Hex(Str),
]

rgb : U8, U8, U8 -> Color
rgb = |r, g, b| Color.RGB(r, g, b)

rgba : U8, U8, U8, U8 -> Color
rgba = |r, g, b, a| {
    rounded = a.to_frac() / 255.0
    Color.RGBA(r, g, b, rounded)
}

hex : Str -> Result(Color, [InvalidHex(Str)])
hex = |str| {

    bytes = str.to_utf8()
    is_char_in_hex_range = |b| (b >= '0' and b <= '9') or (b >= 'a' and b <= 'f') or (b >= 'A' and b <= 'F')

    match bytes {
        ['#', a, b, c, d, e, f] => {
            is_valid =
                a.is_char_in_hex_range()
                and b.is_char_in_hex_range()
                and c.is_char_in_hex_range()
                and d.is_char_in_hex_range()
                and e.is_char_in_hex_range()
                and f.is_char_in_hex_range()

            if is_valid Ok(Color.Hex(str)) else Err(InvalidHex("Expected Hex to be in the range 0-9, a-f, A-F, got ${str}"))
        }
        _ => Err(InvalidHex("Expected Hex must start with # and be 7 characters long, got ${str}"))
    }
}

to_str : Color -> Str
to_str = |color| match color {
    Color.RGB(r, g, b) => "rgb(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)})"
    Color.RGBA(r, g, b, a) => "rgba(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)}, ${Num.to_str(a)})"
    Color.Named(inner) => inner
    Color.Hex(inner) => inner
}

expect rgb(124, 56, 245).to_str() == "rgb(124, 56, 245)"
expect rgba(124, 56, 245, 255).to_str() == "rgba(124, 56, 245, 1.0)"
expect hex("#ff00ff").map_ok(to_str) == Ok("#ff00ff")

named : Str -> Result(Color, [UnknownColor(Str)])
named = |str|
    if str.is_named_color()
        Ok(Color.Named(str))
    else
        Err(UnknownColor("Unknown color ${str}"))

is_named_color = |str|{
    colors = Set.from_list(["AliceBlue", "AntiqueWhite", "Aqua"])

    colors.contains(str)
}
~~~
# EXPECTED
UNUSED VARIABLE - Color.md:30:5:30:25
UNDEFINED VARIABLE - Color.md:68:14:68:27
INVALID NOMINAL TAG - Color.md:23:5:23:33
TYPE MISMATCH - Color.md:27:7:46:2
# PROBLEMS
**UNUSED VARIABLE**
Variable `is_char_in_hex_range` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_is_char_in_hex_range` to suppress this warning.
The unused variable is declared here:
**Color.md:30:5:30:25:**
```roc
    is_char_in_hex_range = |b| (b >= '0' and b <= '9') or (b >= 'a' and b <= 'f') or (b >= 'A' and b <= 'F')
```
    ^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `from_list` in this scope.
Is there an `import` or `exposing` missing up-top?

**Color.md:68:14:68:27:**
```roc
    colors = Set.from_list(["AliceBlue", "AntiqueWhite", "Aqua"])
```
             ^^^^^^^^^^^^^


**INVALID NOMINAL TAG**
I'm having trouble with this nominal tag:
**Color.md:23:5:23:33:**
```roc
    Color.RGBA(r, g, b, rounded)
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The tag is:
    _RGBA(U8, U8, U8, Num(_size))_

But it should be one of:
    _[Hex(Str), Named(Str), RGB(U8, U8, U8), RGBA(U8, U8, U8, Dec)]_

**Hint:** The nominal type has a tag with the same name, but different args:

    _RGBA(U8, U8, U8, Dec)_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**Color.md:27:7:46:2:**
```roc
hex = |str| {

    bytes = str.to_utf8()
    is_char_in_hex_range = |b| (b >= '0' and b <= '9') or (b >= 'a' and b <= 'f') or (b >= 'A' and b <= 'F')

    match bytes {
        ['#', a, b, c, d, e, f] => {
            is_valid =
                a.is_char_in_hex_range()
                and b.is_char_in_hex_range()
                and c.is_char_in_hex_range()
                and d.is_char_in_hex_range()
                and e.is_char_in_hex_range()
                and f.is_char_in_hex_range()

            if is_valid Ok(Color.Hex(str)) else Err(InvalidHex("Expected Hex to be in the range 0-9, a-f, A-F, got ${str}"))
        }
        _ => Err(InvalidHex("Expected Hex must start with # and be 7 characters long, got ${str}"))
    }
}
```

The type annotation says it should have the type:
    _Str -> Result(Error, [InvalidHex(Str)])_

But you are trying to use it as:
    _{ to_utf8: List(Num(_size)) } -> Result(Error, [InvalidHex(Str)]_others)_

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),
UpperIdent(2:5-2:10),Comma(2:10-2:11),
LowerIdent(3:5-3:11),Comma(3:11-3:12),
LowerIdent(4:5-4:8),Comma(4:8-4:9),
LowerIdent(5:5-5:9),Comma(5:9-5:10),
LowerIdent(6:5-6:8),Comma(6:8-6:9),
LowerIdent(7:5-7:10),Comma(7:10-7:11),
CloseSquare(8:1-8:2),
UpperIdent(10:1-10:6),OpColonEqual(10:7-10:9),OpenSquare(10:10-10:11),
UpperIdent(11:5-11:8),NoSpaceOpenRound(11:8-11:9),UpperIdent(11:9-11:11),Comma(11:11-11:12),UpperIdent(11:13-11:15),Comma(11:15-11:16),UpperIdent(11:17-11:19),CloseRound(11:19-11:20),Comma(11:20-11:21),
UpperIdent(12:5-12:9),NoSpaceOpenRound(12:9-12:10),UpperIdent(12:10-12:12),Comma(12:12-12:13),UpperIdent(12:14-12:16),Comma(12:16-12:17),UpperIdent(12:18-12:20),Comma(12:20-12:21),UpperIdent(12:22-12:25),CloseRound(12:25-12:26),Comma(12:26-12:27),
UpperIdent(13:5-13:10),NoSpaceOpenRound(13:10-13:11),UpperIdent(13:11-13:14),CloseRound(13:14-13:15),Comma(13:15-13:16),
UpperIdent(14:5-14:8),NoSpaceOpenRound(14:8-14:9),UpperIdent(14:9-14:12),CloseRound(14:12-14:13),Comma(14:13-14:14),
CloseSquare(15:1-15:2),
LowerIdent(17:1-17:4),OpColon(17:5-17:6),UpperIdent(17:7-17:9),Comma(17:9-17:10),UpperIdent(17:11-17:13),Comma(17:13-17:14),UpperIdent(17:15-17:17),OpArrow(17:18-17:20),UpperIdent(17:21-17:26),
LowerIdent(18:1-18:4),OpAssign(18:5-18:6),OpBar(18:7-18:8),LowerIdent(18:8-18:9),Comma(18:9-18:10),LowerIdent(18:11-18:12),Comma(18:12-18:13),LowerIdent(18:14-18:15),OpBar(18:15-18:16),UpperIdent(18:17-18:22),NoSpaceDotUpperIdent(18:22-18:26),NoSpaceOpenRound(18:26-18:27),LowerIdent(18:27-18:28),Comma(18:28-18:29),LowerIdent(18:30-18:31),Comma(18:31-18:32),LowerIdent(18:33-18:34),CloseRound(18:34-18:35),
LowerIdent(20:1-20:5),OpColon(20:6-20:7),UpperIdent(20:8-20:10),Comma(20:10-20:11),UpperIdent(20:12-20:14),Comma(20:14-20:15),UpperIdent(20:16-20:18),Comma(20:18-20:19),UpperIdent(20:20-20:22),OpArrow(20:23-20:25),UpperIdent(20:26-20:31),
LowerIdent(21:1-21:5),OpAssign(21:6-21:7),OpBar(21:8-21:9),LowerIdent(21:9-21:10),Comma(21:10-21:11),LowerIdent(21:12-21:13),Comma(21:13-21:14),LowerIdent(21:15-21:16),Comma(21:16-21:17),LowerIdent(21:18-21:19),OpBar(21:19-21:20),OpenCurly(21:21-21:22),
LowerIdent(22:5-22:12),OpAssign(22:13-22:14),LowerIdent(22:15-22:16),NoSpaceDotLowerIdent(22:16-22:24),NoSpaceOpenRound(22:24-22:25),CloseRound(22:25-22:26),OpSlash(22:27-22:28),Float(22:29-22:34),
UpperIdent(23:5-23:10),NoSpaceDotUpperIdent(23:10-23:15),NoSpaceOpenRound(23:15-23:16),LowerIdent(23:16-23:17),Comma(23:17-23:18),LowerIdent(23:19-23:20),Comma(23:20-23:21),LowerIdent(23:22-23:23),Comma(23:23-23:24),LowerIdent(23:25-23:32),CloseRound(23:32-23:33),
CloseCurly(24:1-24:2),
LowerIdent(26:1-26:4),OpColon(26:5-26:6),UpperIdent(26:7-26:10),OpArrow(26:11-26:13),UpperIdent(26:14-26:20),NoSpaceOpenRound(26:20-26:21),UpperIdent(26:21-26:26),Comma(26:26-26:27),OpenSquare(26:28-26:29),UpperIdent(26:29-26:39),NoSpaceOpenRound(26:39-26:40),UpperIdent(26:40-26:43),CloseRound(26:43-26:44),CloseSquare(26:44-26:45),CloseRound(26:45-26:46),
LowerIdent(27:1-27:4),OpAssign(27:5-27:6),OpBar(27:7-27:8),LowerIdent(27:8-27:11),OpBar(27:11-27:12),OpenCurly(27:13-27:14),
LowerIdent(29:5-29:10),OpAssign(29:11-29:12),LowerIdent(29:13-29:16),NoSpaceDotLowerIdent(29:16-29:24),NoSpaceOpenRound(29:24-29:25),CloseRound(29:25-29:26),
LowerIdent(30:5-30:25),OpAssign(30:26-30:27),OpBar(30:28-30:29),LowerIdent(30:29-30:30),OpBar(30:30-30:31),OpenRound(30:32-30:33),LowerIdent(30:33-30:34),OpGreaterThanOrEq(30:35-30:37),SingleQuote(30:38-30:41),OpAnd(30:42-30:45),LowerIdent(30:46-30:47),OpLessThanOrEq(30:48-30:50),SingleQuote(30:51-30:54),CloseRound(30:54-30:55),OpOr(30:56-30:58),OpenRound(30:59-30:60),LowerIdent(30:60-30:61),OpGreaterThanOrEq(30:62-30:64),SingleQuote(30:65-30:68),OpAnd(30:69-30:72),LowerIdent(30:73-30:74),OpLessThanOrEq(30:75-30:77),SingleQuote(30:78-30:81),CloseRound(30:81-30:82),OpOr(30:83-30:85),OpenRound(30:86-30:87),LowerIdent(30:87-30:88),OpGreaterThanOrEq(30:89-30:91),SingleQuote(30:92-30:95),OpAnd(30:96-30:99),LowerIdent(30:100-30:101),OpLessThanOrEq(30:102-30:104),SingleQuote(30:105-30:108),CloseRound(30:108-30:109),
KwMatch(32:5-32:10),LowerIdent(32:11-32:16),OpenCurly(32:17-32:18),
OpenSquare(33:9-33:10),SingleQuote(33:10-33:13),Comma(33:13-33:14),LowerIdent(33:15-33:16),Comma(33:16-33:17),LowerIdent(33:18-33:19),Comma(33:19-33:20),LowerIdent(33:21-33:22),Comma(33:22-33:23),LowerIdent(33:24-33:25),Comma(33:25-33:26),LowerIdent(33:27-33:28),Comma(33:28-33:29),LowerIdent(33:30-33:31),CloseSquare(33:31-33:32),OpFatArrow(33:33-33:35),OpenCurly(33:36-33:37),
LowerIdent(34:13-34:21),OpAssign(34:22-34:23),
LowerIdent(35:17-35:18),NoSpaceDotLowerIdent(35:18-35:39),NoSpaceOpenRound(35:39-35:40),CloseRound(35:40-35:41),
OpAnd(36:17-36:20),LowerIdent(36:21-36:22),NoSpaceDotLowerIdent(36:22-36:43),NoSpaceOpenRound(36:43-36:44),CloseRound(36:44-36:45),
OpAnd(37:17-37:20),LowerIdent(37:21-37:22),NoSpaceDotLowerIdent(37:22-37:43),NoSpaceOpenRound(37:43-37:44),CloseRound(37:44-37:45),
OpAnd(38:17-38:20),LowerIdent(38:21-38:22),NoSpaceDotLowerIdent(38:22-38:43),NoSpaceOpenRound(38:43-38:44),CloseRound(38:44-38:45),
OpAnd(39:17-39:20),LowerIdent(39:21-39:22),NoSpaceDotLowerIdent(39:22-39:43),NoSpaceOpenRound(39:43-39:44),CloseRound(39:44-39:45),
OpAnd(40:17-40:20),LowerIdent(40:21-40:22),NoSpaceDotLowerIdent(40:22-40:43),NoSpaceOpenRound(40:43-40:44),CloseRound(40:44-40:45),
KwIf(42:13-42:15),LowerIdent(42:16-42:24),UpperIdent(42:25-42:27),NoSpaceOpenRound(42:27-42:28),UpperIdent(42:28-42:33),NoSpaceDotUpperIdent(42:33-42:37),NoSpaceOpenRound(42:37-42:38),LowerIdent(42:38-42:41),CloseRound(42:41-42:42),CloseRound(42:42-42:43),KwElse(42:44-42:48),UpperIdent(42:49-42:52),NoSpaceOpenRound(42:52-42:53),UpperIdent(42:53-42:63),NoSpaceOpenRound(42:63-42:64),StringStart(42:64-42:65),StringPart(42:65-42:116),OpenStringInterpolation(42:116-42:118),LowerIdent(42:118-42:121),CloseStringInterpolation(42:121-42:122),StringPart(42:122-42:122),StringEnd(42:122-42:123),CloseRound(42:123-42:124),CloseRound(42:124-42:125),
CloseCurly(43:9-43:10),
Underscore(44:9-44:10),OpFatArrow(44:11-44:13),UpperIdent(44:14-44:17),NoSpaceOpenRound(44:17-44:18),UpperIdent(44:18-44:28),NoSpaceOpenRound(44:28-44:29),StringStart(44:29-44:30),StringPart(44:30-44:91),OpenStringInterpolation(44:91-44:93),LowerIdent(44:93-44:96),CloseStringInterpolation(44:96-44:97),StringPart(44:97-44:97),StringEnd(44:97-44:98),CloseRound(44:98-44:99),CloseRound(44:99-44:100),
CloseCurly(45:5-45:6),
CloseCurly(46:1-46:2),
LowerIdent(48:1-48:7),OpColon(48:8-48:9),UpperIdent(48:10-48:15),OpArrow(48:16-48:18),UpperIdent(48:19-48:22),
LowerIdent(49:1-49:7),OpAssign(49:8-49:9),OpBar(49:10-49:11),LowerIdent(49:11-49:16),OpBar(49:16-49:17),KwMatch(49:18-49:23),LowerIdent(49:24-49:29),OpenCurly(49:30-49:31),
UpperIdent(50:5-50:10),NoSpaceDotUpperIdent(50:10-50:14),NoSpaceOpenRound(50:14-50:15),LowerIdent(50:15-50:16),Comma(50:16-50:17),LowerIdent(50:18-50:19),Comma(50:19-50:20),LowerIdent(50:21-50:22),CloseRound(50:22-50:23),OpFatArrow(50:24-50:26),StringStart(50:27-50:28),StringPart(50:28-50:32),OpenStringInterpolation(50:32-50:34),UpperIdent(50:34-50:37),NoSpaceDotLowerIdent(50:37-50:44),NoSpaceOpenRound(50:44-50:45),LowerIdent(50:45-50:46),CloseRound(50:46-50:47),CloseStringInterpolation(50:47-50:48),StringPart(50:48-50:50),OpenStringInterpolation(50:50-50:52),UpperIdent(50:52-50:55),NoSpaceDotLowerIdent(50:55-50:62),NoSpaceOpenRound(50:62-50:63),LowerIdent(50:63-50:64),CloseRound(50:64-50:65),CloseStringInterpolation(50:65-50:66),StringPart(50:66-50:68),OpenStringInterpolation(50:68-50:70),UpperIdent(50:70-50:73),NoSpaceDotLowerIdent(50:73-50:80),NoSpaceOpenRound(50:80-50:81),LowerIdent(50:81-50:82),CloseRound(50:82-50:83),CloseStringInterpolation(50:83-50:84),StringPart(50:84-50:85),StringEnd(50:85-50:86),
UpperIdent(51:5-51:10),NoSpaceDotUpperIdent(51:10-51:15),NoSpaceOpenRound(51:15-51:16),LowerIdent(51:16-51:17),Comma(51:17-51:18),LowerIdent(51:19-51:20),Comma(51:20-51:21),LowerIdent(51:22-51:23),Comma(51:23-51:24),LowerIdent(51:25-51:26),CloseRound(51:26-51:27),OpFatArrow(51:28-51:30),StringStart(51:31-51:32),StringPart(51:32-51:37),OpenStringInterpolation(51:37-51:39),UpperIdent(51:39-51:42),NoSpaceDotLowerIdent(51:42-51:49),NoSpaceOpenRound(51:49-51:50),LowerIdent(51:50-51:51),CloseRound(51:51-51:52),CloseStringInterpolation(51:52-51:53),StringPart(51:53-51:55),OpenStringInterpolation(51:55-51:57),UpperIdent(51:57-51:60),NoSpaceDotLowerIdent(51:60-51:67),NoSpaceOpenRound(51:67-51:68),LowerIdent(51:68-51:69),CloseRound(51:69-51:70),CloseStringInterpolation(51:70-51:71),StringPart(51:71-51:73),OpenStringInterpolation(51:73-51:75),UpperIdent(51:75-51:78),NoSpaceDotLowerIdent(51:78-51:85),NoSpaceOpenRound(51:85-51:86),LowerIdent(51:86-51:87),CloseRound(51:87-51:88),CloseStringInterpolation(51:88-51:89),StringPart(51:89-51:91),OpenStringInterpolation(51:91-51:93),UpperIdent(51:93-51:96),NoSpaceDotLowerIdent(51:96-51:103),NoSpaceOpenRound(51:103-51:104),LowerIdent(51:104-51:105),CloseRound(51:105-51:106),CloseStringInterpolation(51:106-51:107),StringPart(51:107-51:108),StringEnd(51:108-51:109),
UpperIdent(52:5-52:10),NoSpaceDotUpperIdent(52:10-52:16),NoSpaceOpenRound(52:16-52:17),LowerIdent(52:17-52:22),CloseRound(52:22-52:23),OpFatArrow(52:24-52:26),LowerIdent(52:27-52:32),
UpperIdent(53:5-53:10),NoSpaceDotUpperIdent(53:10-53:14),NoSpaceOpenRound(53:14-53:15),LowerIdent(53:15-53:20),CloseRound(53:20-53:21),OpFatArrow(53:22-53:24),LowerIdent(53:25-53:30),
CloseCurly(54:1-54:2),
KwExpect(56:1-56:7),LowerIdent(56:8-56:11),NoSpaceOpenRound(56:11-56:12),Int(56:12-56:15),Comma(56:15-56:16),Int(56:17-56:19),Comma(56:19-56:20),Int(56:21-56:24),CloseRound(56:24-56:25),NoSpaceDotLowerIdent(56:25-56:32),NoSpaceOpenRound(56:32-56:33),CloseRound(56:33-56:34),OpEquals(56:35-56:37),StringStart(56:38-56:39),StringPart(56:39-56:56),StringEnd(56:56-56:57),
KwExpect(57:1-57:7),LowerIdent(57:8-57:12),NoSpaceOpenRound(57:12-57:13),Int(57:13-57:16),Comma(57:16-57:17),Int(57:18-57:20),Comma(57:20-57:21),Int(57:22-57:25),Comma(57:25-57:26),Int(57:27-57:30),CloseRound(57:30-57:31),NoSpaceDotLowerIdent(57:31-57:38),NoSpaceOpenRound(57:38-57:39),CloseRound(57:39-57:40),OpEquals(57:41-57:43),StringStart(57:44-57:45),StringPart(57:45-57:68),StringEnd(57:68-57:69),
KwExpect(58:1-58:7),LowerIdent(58:8-58:11),NoSpaceOpenRound(58:11-58:12),StringStart(58:12-58:13),StringPart(58:13-58:20),StringEnd(58:20-58:21),CloseRound(58:21-58:22),NoSpaceDotLowerIdent(58:22-58:29),NoSpaceOpenRound(58:29-58:30),LowerIdent(58:30-58:36),CloseRound(58:36-58:37),OpEquals(58:38-58:40),UpperIdent(58:41-58:43),NoSpaceOpenRound(58:43-58:44),StringStart(58:44-58:45),StringPart(58:45-58:52),StringEnd(58:52-58:53),CloseRound(58:53-58:54),
LowerIdent(60:1-60:6),OpColon(60:7-60:8),UpperIdent(60:9-60:12),OpArrow(60:13-60:15),UpperIdent(60:16-60:22),NoSpaceOpenRound(60:22-60:23),UpperIdent(60:23-60:28),Comma(60:28-60:29),OpenSquare(60:30-60:31),UpperIdent(60:31-60:43),NoSpaceOpenRound(60:43-60:44),UpperIdent(60:44-60:47),CloseRound(60:47-60:48),CloseSquare(60:48-60:49),CloseRound(60:49-60:50),
LowerIdent(61:1-61:6),OpAssign(61:7-61:8),OpBar(61:9-61:10),LowerIdent(61:10-61:13),OpBar(61:13-61:14),
KwIf(62:5-62:7),LowerIdent(62:8-62:11),NoSpaceDotLowerIdent(62:11-62:26),NoSpaceOpenRound(62:26-62:27),CloseRound(62:27-62:28),
UpperIdent(63:9-63:11),NoSpaceOpenRound(63:11-63:12),UpperIdent(63:12-63:17),NoSpaceDotUpperIdent(63:17-63:23),NoSpaceOpenRound(63:23-63:24),LowerIdent(63:24-63:27),CloseRound(63:27-63:28),CloseRound(63:28-63:29),
KwElse(64:5-64:9),
UpperIdent(65:9-65:12),NoSpaceOpenRound(65:12-65:13),UpperIdent(65:13-65:25),NoSpaceOpenRound(65:25-65:26),StringStart(65:26-65:27),StringPart(65:27-65:41),OpenStringInterpolation(65:41-65:43),LowerIdent(65:43-65:46),CloseStringInterpolation(65:46-65:47),StringPart(65:47-65:47),StringEnd(65:47-65:48),CloseRound(65:48-65:49),CloseRound(65:49-65:50),
LowerIdent(67:1-67:15),OpAssign(67:16-67:17),OpBar(67:18-67:19),LowerIdent(67:19-67:22),OpBar(67:22-67:23),OpenCurly(67:23-67:24),
LowerIdent(68:5-68:11),OpAssign(68:12-68:13),UpperIdent(68:14-68:17),NoSpaceDotLowerIdent(68:17-68:27),NoSpaceOpenRound(68:27-68:28),OpenSquare(68:28-68:29),StringStart(68:29-68:30),StringPart(68:30-68:39),StringEnd(68:39-68:40),Comma(68:40-68:41),StringStart(68:42-68:43),StringPart(68:43-68:55),StringEnd(68:55-68:56),Comma(68:56-68:57),StringStart(68:58-68:59),StringPart(68:59-68:63),StringEnd(68:63-68:64),CloseSquare(68:64-68:65),CloseRound(68:65-68:66),
LowerIdent(70:5-70:11),NoSpaceDotLowerIdent(70:11-70:20),NoSpaceOpenRound(70:20-70:21),LowerIdent(70:21-70:24),CloseRound(70:24-70:25),
CloseCurly(71:1-71:2),
EndOfFile(72:1-72:1),
~~~
# PARSE
~~~clojure
(file @1.1-71.2
	(module @1.1-8.2
		(exposes @1.8-8.2
			(exposed-upper-ident @2.5-2.10 (text "Color"))
			(exposed-lower-ident @3.5-3.11
				(text "to_str"))
			(exposed-lower-ident @4.5-4.8
				(text "rgb"))
			(exposed-lower-ident @5.5-5.9
				(text "rgba"))
			(exposed-lower-ident @6.5-6.8
				(text "hex"))
			(exposed-lower-ident @7.5-7.10
				(text "named"))))
	(statements
		(s-type-decl @10.1-15.2
			(header @10.1-10.6 (name "Color")
				(args))
			(ty-tag-union @10.10-15.2
				(tags
					(ty-apply @11.5-11.20
						(ty @11.5-11.8 (name "RGB"))
						(ty @11.9-11.11 (name "U8"))
						(ty @11.13-11.15 (name "U8"))
						(ty @11.17-11.19 (name "U8")))
					(ty-apply @12.5-12.26
						(ty @12.5-12.9 (name "RGBA"))
						(ty @12.10-12.12 (name "U8"))
						(ty @12.14-12.16 (name "U8"))
						(ty @12.18-12.20 (name "U8"))
						(ty @12.22-12.25 (name "Dec")))
					(ty-apply @13.5-13.15
						(ty @13.5-13.10 (name "Named"))
						(ty @13.11-13.14 (name "Str")))
					(ty-apply @14.5-14.13
						(ty @14.5-14.8 (name "Hex"))
						(ty @14.9-14.12 (name "Str"))))))
		(s-type-anno @17.1-17.26 (name "rgb")
			(ty-fn @17.7-17.26
				(ty @17.7-17.9 (name "U8"))
				(ty @17.11-17.13 (name "U8"))
				(ty @17.15-17.17 (name "U8"))
				(ty @17.21-17.26 (name "Color"))))
		(s-decl @18.1-18.35
			(p-ident @18.1-18.4 (raw "rgb"))
			(e-lambda @18.7-18.35
				(args
					(p-ident @18.8-18.9 (raw "r"))
					(p-ident @18.11-18.12 (raw "g"))
					(p-ident @18.14-18.15 (raw "b")))
				(e-apply @18.17-18.35
					(e-tag @18.17-18.26 (raw "Color.RGB"))
					(e-ident @18.27-18.28 (raw "r"))
					(e-ident @18.30-18.31 (raw "g"))
					(e-ident @18.33-18.34 (raw "b")))))
		(s-type-anno @20.1-20.31 (name "rgba")
			(ty-fn @20.8-20.31
				(ty @20.8-20.10 (name "U8"))
				(ty @20.12-20.14 (name "U8"))
				(ty @20.16-20.18 (name "U8"))
				(ty @20.20-20.22 (name "U8"))
				(ty @20.26-20.31 (name "Color"))))
		(s-decl @21.1-24.2
			(p-ident @21.1-21.5 (raw "rgba"))
			(e-lambda @21.8-24.2
				(args
					(p-ident @21.9-21.10 (raw "r"))
					(p-ident @21.12-21.13 (raw "g"))
					(p-ident @21.15-21.16 (raw "b"))
					(p-ident @21.18-21.19 (raw "a")))
				(e-block @21.21-24.2
					(statements
						(s-decl @22.5-22.34
							(p-ident @22.5-22.12 (raw "rounded"))
							(e-binop @22.15-22.34 (op "/")
								(e-field-access @22.15-22.26
									(e-ident @22.15-22.16 (raw "a"))
									(e-apply @22.16-22.26
										(e-ident @22.16-22.24 (raw "to_frac"))))
								(e-frac @22.29-22.34 (raw "255.0"))))
						(e-apply @23.5-23.33
							(e-tag @23.5-23.15 (raw "Color.RGBA"))
							(e-ident @23.16-23.17 (raw "r"))
							(e-ident @23.19-23.20 (raw "g"))
							(e-ident @23.22-23.23 (raw "b"))
							(e-ident @23.25-23.32 (raw "rounded")))))))
		(s-type-anno @26.1-26.46 (name "hex")
			(ty-fn @26.7-26.46
				(ty @26.7-26.10 (name "Str"))
				(ty-apply @26.14-26.46
					(ty @26.14-26.20 (name "Result"))
					(ty @26.21-26.26 (name "Color"))
					(ty-tag-union @26.28-26.45
						(tags
							(ty-apply @26.29-26.44
								(ty @26.29-26.39 (name "InvalidHex"))
								(ty @26.40-26.43 (name "Str"))))))))
		(s-decl @27.1-46.2
			(p-ident @27.1-27.4 (raw "hex"))
			(e-lambda @27.7-46.2
				(args
					(p-ident @27.8-27.11 (raw "str")))
				(e-block @27.13-46.2
					(statements
						(s-decl @29.5-29.26
							(p-ident @29.5-29.10 (raw "bytes"))
							(e-field-access @29.13-29.26
								(e-ident @29.13-29.16 (raw "str"))
								(e-apply @29.16-29.26
									(e-ident @29.16-29.24 (raw "to_utf8")))))
						(s-decl @30.5-30.109
							(p-ident @30.5-30.25 (raw "is_char_in_hex_range"))
							(e-lambda @30.28-30.109
								(args
									(p-ident @30.29-30.30 (raw "b")))
								(e-binop @30.32-30.109 (op "or")
									(e-tuple @30.32-30.55
										(e-binop @30.33-30.54 (op "and")
											(e-binop @30.33-30.41 (op ">=")
												(e-ident @30.33-30.34 (raw "b"))
												(e-single-quote @30.38-30.41 (raw "'0'")))
											(e-binop @30.46-30.54 (op "<=")
												(e-ident @30.46-30.47 (raw "b"))
												(e-single-quote @30.51-30.54 (raw "'9'")))))
									(e-binop @30.59-30.109 (op "or")
										(e-tuple @30.59-30.82
											(e-binop @30.60-30.81 (op "and")
												(e-binop @30.60-30.68 (op ">=")
													(e-ident @30.60-30.61 (raw "b"))
													(e-single-quote @30.65-30.68 (raw "'a'")))
												(e-binop @30.73-30.81 (op "<=")
													(e-ident @30.73-30.74 (raw "b"))
													(e-single-quote @30.78-30.81 (raw "'f'")))))
										(e-tuple @30.86-30.109
											(e-binop @30.87-30.108 (op "and")
												(e-binop @30.87-30.95 (op ">=")
													(e-ident @30.87-30.88 (raw "b"))
													(e-single-quote @30.92-30.95 (raw "'A'")))
												(e-binop @30.100-30.108 (op "<=")
													(e-ident @30.100-30.101 (raw "b"))
													(e-single-quote @30.105-30.108 (raw "'F'")))))))))
						(e-match
							(e-ident @32.11-32.16 (raw "bytes"))
							(branches
								(branch @33.9-43.10
									(p-list @33.9-33.32
										(p-single-quote @33.10-33.13 (raw "'#'"))
										(p-ident @33.15-33.16 (raw "a"))
										(p-ident @33.18-33.19 (raw "b"))
										(p-ident @33.21-33.22 (raw "c"))
										(p-ident @33.24-33.25 (raw "d"))
										(p-ident @33.27-33.28 (raw "e"))
										(p-ident @33.30-33.31 (raw "f")))
									(e-block @33.36-43.10
										(statements
											(s-decl @34.13-40.45
												(p-ident @34.13-34.21 (raw "is_valid"))
												(e-binop @35.17-40.45 (op "and")
													(e-field-access @35.17-35.41
														(e-ident @35.17-35.18 (raw "a"))
														(e-apply @35.18-35.41
															(e-ident @35.18-35.39 (raw "is_char_in_hex_range"))))
													(e-binop @36.21-40.45 (op "and")
														(e-field-access @36.21-36.45
															(e-ident @36.21-36.22 (raw "b"))
															(e-apply @36.22-36.45
																(e-ident @36.22-36.43 (raw "is_char_in_hex_range"))))
														(e-binop @37.21-40.45 (op "and")
															(e-field-access @37.21-37.45
																(e-ident @37.21-37.22 (raw "c"))
																(e-apply @37.22-37.45
																	(e-ident @37.22-37.43 (raw "is_char_in_hex_range"))))
															(e-binop @38.21-40.45 (op "and")
																(e-field-access @38.21-38.45
																	(e-ident @38.21-38.22 (raw "d"))
																	(e-apply @38.22-38.45
																		(e-ident @38.22-38.43 (raw "is_char_in_hex_range"))))
																(e-binop @39.21-40.45 (op "and")
																	(e-field-access @39.21-39.45
																		(e-ident @39.21-39.22 (raw "e"))
																		(e-apply @39.22-39.45
																			(e-ident @39.22-39.43 (raw "is_char_in_hex_range"))))
																	(e-field-access @40.21-40.45
																		(e-ident @40.21-40.22 (raw "f"))
																		(e-apply @40.22-40.45
																			(e-ident @40.22-40.43 (raw "is_char_in_hex_range"))))))))))
											(e-if-then-else @42.13-42.125
												(e-ident @42.16-42.24 (raw "is_valid"))
												(e-apply @42.25-42.43
													(e-tag @42.25-42.27 (raw "Ok"))
													(e-apply @42.28-42.42
														(e-tag @42.28-42.37 (raw "Color.Hex"))
														(e-ident @42.38-42.41 (raw "str"))))
												(e-apply @42.49-42.125
													(e-tag @42.49-42.52 (raw "Err"))
													(e-apply @42.53-42.124
														(e-tag @42.53-42.63 (raw "InvalidHex"))
														(e-string @42.64-42.123
															(e-string-part @42.65-42.116 (raw "Expected Hex to be in the range 0-9, a-f, A-F, got "))
															(e-ident @42.118-42.121 (raw "str"))
															(e-string-part @42.122-42.122 (raw "")))))))))
								(branch @44.9-44.100
									(p-underscore)
									(e-apply @44.14-44.100
										(e-tag @44.14-44.17 (raw "Err"))
										(e-apply @44.18-44.99
											(e-tag @44.18-44.28 (raw "InvalidHex"))
											(e-string @44.29-44.98
												(e-string-part @44.30-44.91 (raw "Expected Hex must start with # and be 7 characters long, got "))
												(e-ident @44.93-44.96 (raw "str"))
												(e-string-part @44.97-44.97 (raw ""))))))))))))
		(s-type-anno @48.1-48.22 (name "to_str")
			(ty-fn @48.10-48.22
				(ty @48.10-48.15 (name "Color"))
				(ty @48.19-48.22 (name "Str"))))
		(s-decl @49.1-54.2
			(p-ident @49.1-49.7 (raw "to_str"))
			(e-lambda @49.10-54.2
				(args
					(p-ident @49.11-49.16 (raw "color")))
				(e-match
					(e-ident @49.24-49.29 (raw "color"))
					(branches
						(branch @50.5-50.86
							(p-tag @50.5-50.23 (raw ".RGB")
								(p-ident @50.15-50.16 (raw "r"))
								(p-ident @50.18-50.19 (raw "g"))
								(p-ident @50.21-50.22 (raw "b")))
							(e-string @50.27-50.86
								(e-string-part @50.28-50.32 (raw "rgb("))
								(e-apply @50.34-50.47
									(e-ident @50.34-50.44 (raw "Num.to_str"))
									(e-ident @50.45-50.46 (raw "r")))
								(e-string-part @50.48-50.50 (raw ", "))
								(e-apply @50.52-50.65
									(e-ident @50.52-50.62 (raw "Num.to_str"))
									(e-ident @50.63-50.64 (raw "g")))
								(e-string-part @50.66-50.68 (raw ", "))
								(e-apply @50.70-50.83
									(e-ident @50.70-50.80 (raw "Num.to_str"))
									(e-ident @50.81-50.82 (raw "b")))
								(e-string-part @50.84-50.85 (raw ")"))))
						(branch @51.5-51.109
							(p-tag @51.5-51.27 (raw ".RGBA")
								(p-ident @51.16-51.17 (raw "r"))
								(p-ident @51.19-51.20 (raw "g"))
								(p-ident @51.22-51.23 (raw "b"))
								(p-ident @51.25-51.26 (raw "a")))
							(e-string @51.31-51.109
								(e-string-part @51.32-51.37 (raw "rgba("))
								(e-apply @51.39-51.52
									(e-ident @51.39-51.49 (raw "Num.to_str"))
									(e-ident @51.50-51.51 (raw "r")))
								(e-string-part @51.53-51.55 (raw ", "))
								(e-apply @51.57-51.70
									(e-ident @51.57-51.67 (raw "Num.to_str"))
									(e-ident @51.68-51.69 (raw "g")))
								(e-string-part @51.71-51.73 (raw ", "))
								(e-apply @51.75-51.88
									(e-ident @51.75-51.85 (raw "Num.to_str"))
									(e-ident @51.86-51.87 (raw "b")))
								(e-string-part @51.89-51.91 (raw ", "))
								(e-apply @51.93-51.106
									(e-ident @51.93-51.103 (raw "Num.to_str"))
									(e-ident @51.104-51.105 (raw "a")))
								(e-string-part @51.107-51.108 (raw ")"))))
						(branch @52.5-52.32
							(p-tag @52.5-52.23 (raw ".Named")
								(p-ident @52.17-52.22 (raw "inner")))
							(e-ident @52.27-52.32 (raw "inner")))
						(branch @53.5-53.30
							(p-tag @53.5-53.21 (raw ".Hex")
								(p-ident @53.15-53.20 (raw "inner")))
							(e-ident @53.25-53.30 (raw "inner")))))))
		(s-expect @56.1-56.57
			(e-binop @56.8-56.57 (op "==")
				(e-field-access @56.8-56.34
					(e-apply @56.8-56.25
						(e-ident @56.8-56.11 (raw "rgb"))
						(e-int @56.12-56.15 (raw "124"))
						(e-int @56.17-56.19 (raw "56"))
						(e-int @56.21-56.24 (raw "245")))
					(e-apply @56.25-56.34
						(e-ident @56.25-56.32 (raw "to_str"))))
				(e-string @56.38-56.57
					(e-string-part @56.39-56.56 (raw "rgb(124, 56, 245)")))))
		(s-expect @57.1-57.69
			(e-binop @57.8-57.69 (op "==")
				(e-field-access @57.8-57.40
					(e-apply @57.8-57.31
						(e-ident @57.8-57.12 (raw "rgba"))
						(e-int @57.13-57.16 (raw "124"))
						(e-int @57.18-57.20 (raw "56"))
						(e-int @57.22-57.25 (raw "245"))
						(e-int @57.27-57.30 (raw "255")))
					(e-apply @57.31-57.40
						(e-ident @57.31-57.38 (raw "to_str"))))
				(e-string @57.44-57.69
					(e-string-part @57.45-57.68 (raw "rgba(124, 56, 245, 1.0)")))))
		(s-expect @58.1-58.54
			(e-binop @58.8-58.54 (op "==")
				(e-field-access @58.8-58.37
					(e-apply @58.8-58.22
						(e-ident @58.8-58.11 (raw "hex"))
						(e-string @58.12-58.21
							(e-string-part @58.13-58.20 (raw "#ff00ff"))))
					(e-apply @58.22-58.37
						(e-ident @58.22-58.29 (raw "map_ok"))
						(e-ident @58.30-58.36 (raw "to_str"))))
				(e-apply @58.41-58.54
					(e-tag @58.41-58.43 (raw "Ok"))
					(e-string @58.44-58.53
						(e-string-part @58.45-58.52 (raw "#ff00ff"))))))
		(s-type-anno @60.1-60.50 (name "named")
			(ty-fn @60.9-60.50
				(ty @60.9-60.12 (name "Str"))
				(ty-apply @60.16-60.50
					(ty @60.16-60.22 (name "Result"))
					(ty @60.23-60.28 (name "Color"))
					(ty-tag-union @60.30-60.49
						(tags
							(ty-apply @60.31-60.48
								(ty @60.31-60.43 (name "UnknownColor"))
								(ty @60.44-60.47 (name "Str"))))))))
		(s-decl @61.1-65.50
			(p-ident @61.1-61.6 (raw "named"))
			(e-lambda @61.9-65.50
				(args
					(p-ident @61.10-61.13 (raw "str")))
				(e-if-then-else @62.5-65.50
					(e-field-access @62.8-62.28
						(e-ident @62.8-62.11 (raw "str"))
						(e-apply @62.11-62.28
							(e-ident @62.11-62.26 (raw "is_named_color"))))
					(e-apply @63.9-63.29
						(e-tag @63.9-63.11 (raw "Ok"))
						(e-apply @63.12-63.28
							(e-tag @63.12-63.23 (raw "Color.Named"))
							(e-ident @63.24-63.27 (raw "str"))))
					(e-apply @65.9-65.50
						(e-tag @65.9-65.12 (raw "Err"))
						(e-apply @65.13-65.49
							(e-tag @65.13-65.25 (raw "UnknownColor"))
							(e-string @65.26-65.48
								(e-string-part @65.27-65.41 (raw "Unknown color "))
								(e-ident @65.43-65.46 (raw "str"))
								(e-string-part @65.47-65.47 (raw ""))))))))
		(s-decl @67.1-71.2
			(p-ident @67.1-67.15 (raw "is_named_color"))
			(e-lambda @67.18-71.2
				(args
					(p-ident @67.19-67.22 (raw "str")))
				(e-block @67.23-71.2
					(statements
						(s-decl @68.5-68.66
							(p-ident @68.5-68.11 (raw "colors"))
							(e-apply @68.14-68.66
								(e-ident @68.14-68.27 (raw "Set.from_list"))
								(e-list @68.28-68.65
									(e-string @68.29-68.40
										(e-string-part @68.30-68.39 (raw "AliceBlue")))
									(e-string @68.42-68.56
										(e-string-part @68.43-68.55 (raw "AntiqueWhite")))
									(e-string @68.58-68.64
										(e-string-part @68.59-68.63 (raw "Aqua"))))))
						(e-field-access @70.5-70.25
							(e-ident @70.5-70.11 (raw "colors"))
							(e-apply @70.11-70.25
								(e-ident @70.11-70.20 (raw "contains"))
								(e-ident @70.21-70.24 (raw "str"))))))))))
~~~
# FORMATTED
~~~roc
module [
	Color,
	to_str,
	rgb,
	rgba,
	hex,
	named,
]

Color := [
	RGB(U8, U8, U8),
	RGBA(U8, U8, U8, Dec),
	Named(Str),
	Hex(Str),
]

rgb : U8, U8, U8 -> Color
rgb = |r, g, b| Color.RGB(r, g, b)

rgba : U8, U8, U8, U8 -> Color
rgba = |r, g, b, a| {
	rounded = a.to_frac() / 255.0
	Color.RGBA(r, g, b, rounded)
}

hex : Str -> Result(Color, [InvalidHex(Str)])
hex = |str| {

	bytes = str.to_utf8()
	is_char_in_hex_range = |b| (b >= '0' and b <= '9') or (b >= 'a' and b <= 'f') or (b >= 'A' and b <= 'F')

	match bytes {
		['#', a, b, c, d, e, f] => {
			is_valid = 
				a.is_char_in_hex_range()
					and b.is_char_in_hex_range()
						and c.is_char_in_hex_range()
							and d.is_char_in_hex_range()
								and e.is_char_in_hex_range()
									and f.is_char_in_hex_range()

			if is_valid Ok(Color.Hex(str)) else Err(InvalidHex("Expected Hex to be in the range 0-9, a-f, A-F, got ${str}"))
		}
		_ => Err(InvalidHex("Expected Hex must start with # and be 7 characters long, got ${str}"))
	}
}

to_str : Color -> Str
to_str = |color| match color {
	Color.RGB(r, g, b) => "rgb(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)})"
	Color.RGBA(r, g, b, a) => "rgba(${Num.to_str(r)}, ${Num.to_str(g)}, ${Num.to_str(b)}, ${Num.to_str(a)})"
	Color.Named(inner) => inner
	Color.Hex(inner) => inner
}

expect rgb(124, 56, 245).to_str() == "rgb(124, 56, 245)"
expect rgba(124, 56, 245, 255).to_str() == "rgba(124, 56, 245, 1.0)"
expect hex("#ff00ff").map_ok(to_str) == Ok("#ff00ff")

named : Str -> Result(Color, [UnknownColor(Str)])
named = |str|
	if str.is_named_color()
		Ok(Color.Named(str))
			else
				Err(UnknownColor("Unknown color ${str}"))

is_named_color = |str| {
	colors = Set.from_list(["AliceBlue", "AntiqueWhite", "Aqua"])

	colors.contains(str)
}

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @18.1-18.4 (ident "rgb"))
		(e-lambda @18.7-18.35
			(args
				(p-assign @18.8-18.9 (ident "r"))
				(p-assign @18.11-18.12 (ident "g"))
				(p-assign @18.14-18.15 (ident "b")))
			(e-nominal @18.17-18.35 (nominal "Color")
				(e-tag @18.17-18.35 (name "RGB")
					(args
						(e-lookup-local @18.27-18.28
							(p-assign @18.8-18.9 (ident "r")))
						(e-lookup-local @18.30-18.31
							(p-assign @18.11-18.12 (ident "g")))
						(e-lookup-local @18.33-18.34
							(p-assign @18.14-18.15 (ident "b")))))))
		(annotation @18.1-18.4
			(declared-type
				(ty-fn @17.7-17.26 (effectful false)
					(ty @17.7-17.9 (name "U8"))
					(ty @17.11-17.13 (name "U8"))
					(ty @17.15-17.17 (name "U8"))
					(ty @17.21-17.26 (name "Color"))))))
	(d-let
		(p-assign @21.1-21.5 (ident "rgba"))
		(e-lambda @21.8-24.2
			(args
				(p-assign @21.9-21.10 (ident "r"))
				(p-assign @21.12-21.13 (ident "g"))
				(p-assign @21.15-21.16 (ident "b"))
				(p-assign @21.18-21.19 (ident "a")))
			(e-block @21.21-24.2
				(s-let @22.5-22.34
					(p-assign @22.5-22.12 (ident "rounded"))
					(e-binop @22.15-22.34 (op "div")
						(e-dot-access @22.15-22.26 (field "to_frac")
							(receiver
								(e-lookup-local @22.15-22.16
									(p-assign @21.18-21.19 (ident "a"))))
							(args))
						(e-dec-small @22.29-22.34 (numerator "2550") (denominator-power-of-ten "1") (value "255"))))
				(e-nominal @23.5-23.33 (nominal "Color")
					(e-tag @23.5-23.33 (name "RGBA")
						(args
							(e-lookup-local @23.16-23.17
								(p-assign @21.9-21.10 (ident "r")))
							(e-lookup-local @23.19-23.20
								(p-assign @21.12-21.13 (ident "g")))
							(e-lookup-local @23.22-23.23
								(p-assign @21.15-21.16 (ident "b")))
							(e-lookup-local @23.25-23.32
								(p-assign @22.5-22.12 (ident "rounded"))))))))
		(annotation @21.1-21.5
			(declared-type
				(ty-fn @20.8-20.31 (effectful false)
					(ty @20.8-20.10 (name "U8"))
					(ty @20.12-20.14 (name "U8"))
					(ty @20.16-20.18 (name "U8"))
					(ty @20.20-20.22 (name "U8"))
					(ty @20.26-20.31 (name "Color"))))))
	(d-let
		(p-assign @27.1-27.4 (ident "hex"))
		(e-closure @27.7-46.2
			(captures
				(capture @33.21-33.22 (ident "c"))
				(capture @33.27-33.28 (ident "e"))
				(capture @34.13-34.21 (ident "is_valid"))
				(capture @33.18-33.19 (ident "b"))
				(capture @33.15-33.16 (ident "a"))
				(capture @33.24-33.25 (ident "d"))
				(capture @33.30-33.31 (ident "f")))
			(e-lambda @27.7-46.2
				(args
					(p-assign @27.8-27.11 (ident "str")))
				(e-block @27.13-46.2
					(s-let @29.5-29.26
						(p-assign @29.5-29.10 (ident "bytes"))
						(e-dot-access @29.13-29.26 (field "to_utf8")
							(receiver
								(e-lookup-local @29.13-29.16
									(p-assign @27.8-27.11 (ident "str"))))
							(args)))
					(s-let @30.5-30.109
						(p-assign @30.5-30.25 (ident "is_char_in_hex_range"))
						(e-lambda @30.28-30.109
							(args
								(p-assign @30.29-30.30 (ident "b")))
							(e-binop @30.32-30.109 (op "or")
								(e-binop @30.33-30.54 (op "and")
									(e-binop @30.33-30.41 (op "ge")
										(e-lookup-local @30.33-30.34
											(p-assign @30.29-30.30 (ident "b")))
										(e-int @30.38-30.41 (value "48")))
									(e-binop @30.46-30.54 (op "le")
										(e-lookup-local @30.46-30.47
											(p-assign @30.29-30.30 (ident "b")))
										(e-int @30.51-30.54 (value "57"))))
								(e-binop @30.59-30.109 (op "or")
									(e-binop @30.60-30.81 (op "and")
										(e-binop @30.60-30.68 (op "ge")
											(e-lookup-local @30.60-30.61
												(p-assign @30.29-30.30 (ident "b")))
											(e-int @30.65-30.68 (value "97")))
										(e-binop @30.73-30.81 (op "le")
											(e-lookup-local @30.73-30.74
												(p-assign @30.29-30.30 (ident "b")))
											(e-int @30.78-30.81 (value "102"))))
									(e-binop @30.87-30.108 (op "and")
										(e-binop @30.87-30.95 (op "ge")
											(e-lookup-local @30.87-30.88
												(p-assign @30.29-30.30 (ident "b")))
											(e-int @30.92-30.95 (value "65")))
										(e-binop @30.100-30.108 (op "le")
											(e-lookup-local @30.100-30.101
												(p-assign @30.29-30.30 (ident "b")))
											(e-int @30.105-30.108 (value "70"))))))))
					(e-match @32.5-45.6
						(match @32.5-45.6
							(cond
								(e-lookup-local @32.11-32.16
									(p-assign @29.5-29.10 (ident "bytes"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-list @33.9-33.32
												(patterns
													(p-int @33.10-33.13 (value "35"))
													(p-assign @33.15-33.16 (ident "a"))
													(p-assign @33.18-33.19 (ident "b"))
													(p-assign @33.21-33.22 (ident "c"))
													(p-assign @33.24-33.25 (ident "d"))
													(p-assign @33.27-33.28 (ident "e"))
													(p-assign @33.30-33.31 (ident "f"))))))
									(value
										(e-block @33.36-43.10
											(s-let @34.13-40.45
												(p-assign @34.13-34.21 (ident "is_valid"))
												(e-binop @35.17-40.45 (op "and")
													(e-dot-access @35.17-35.41 (field "is_char_in_hex_range")
														(receiver
															(e-lookup-local @35.17-35.18
																(p-assign @33.15-33.16 (ident "a"))))
														(args))
													(e-binop @36.21-40.45 (op "and")
														(e-dot-access @36.21-36.45 (field "is_char_in_hex_range")
															(receiver
																(e-lookup-local @36.21-36.22
																	(p-assign @33.18-33.19 (ident "b"))))
															(args))
														(e-binop @37.21-40.45 (op "and")
															(e-dot-access @37.21-37.45 (field "is_char_in_hex_range")
																(receiver
																	(e-lookup-local @37.21-37.22
																		(p-assign @33.21-33.22 (ident "c"))))
																(args))
															(e-binop @38.21-40.45 (op "and")
																(e-dot-access @38.21-38.45 (field "is_char_in_hex_range")
																	(receiver
																		(e-lookup-local @38.21-38.22
																			(p-assign @33.24-33.25 (ident "d"))))
																	(args))
																(e-binop @39.21-40.45 (op "and")
																	(e-dot-access @39.21-39.45 (field "is_char_in_hex_range")
																		(receiver
																			(e-lookup-local @39.21-39.22
																				(p-assign @33.27-33.28 (ident "e"))))
																		(args))
																	(e-dot-access @40.21-40.45 (field "is_char_in_hex_range")
																		(receiver
																			(e-lookup-local @40.21-40.22
																				(p-assign @33.30-33.31 (ident "f"))))
																		(args))))))))
											(e-if @42.13-42.125
												(if-branches
													(if-branch
														(e-lookup-local @42.16-42.24
															(p-assign @34.13-34.21 (ident "is_valid")))
														(e-nominal @42.25-42.43 (nominal "Result")
															(e-tag @42.25-42.43 (name "Ok")
																(args
																	(e-nominal @42.28-42.42 (nominal "Color")
																		(e-tag @42.28-42.42 (name "Hex")
																			(args
																				(e-lookup-local @42.38-42.41
																					(p-assign @27.8-27.11 (ident "str")))))))))))
												(if-else
													(e-nominal @42.49-42.125 (nominal "Result")
														(e-tag @42.49-42.125 (name "Err")
															(args
																(e-tag @42.53-42.124 (name "InvalidHex")
																	(args
																		(e-string @42.64-42.123
																			(e-literal @42.65-42.116 (string "Expected Hex to be in the range 0-9, a-f, A-F, got "))
																			(e-lookup-local @42.118-42.121
																				(p-assign @27.8-27.11 (ident "str")))
																			(e-literal @42.122-42.122 (string "")))))))))))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-underscore @44.9-44.10)))
									(value
										(e-nominal @44.14-44.100 (nominal "Result")
											(e-tag @44.14-44.100 (name "Err")
												(args
													(e-tag @44.18-44.99 (name "InvalidHex")
														(args
															(e-string @44.29-44.98
																(e-literal @44.30-44.91 (string "Expected Hex must start with # and be 7 characters long, got "))
																(e-lookup-local @44.93-44.96
																	(p-assign @27.8-27.11 (ident "str")))
																(e-literal @44.97-44.97 (string ""))))))))))))))))
		(annotation @27.1-27.4
			(declared-type
				(ty-fn @26.7-26.46 (effectful false)
					(ty @26.7-26.10 (name "Str"))
					(ty-apply @26.14-26.46 (symbol "Result")
						(ty @26.21-26.26 (name "Color"))
						(ty-tag-union @26.28-26.45
							(ty-apply @26.29-26.44 (symbol "InvalidHex")
								(ty @26.40-26.43 (name "Str")))))))))
	(d-let
		(p-assign @49.1-49.7 (ident "to_str"))
		(e-closure @49.10-54.2
			(captures
				(capture @50.18-50.19 (ident "g"))
				(capture @52.17-52.22 (ident "inner"))
				(capture @50.21-50.22 (ident "b"))
				(capture @49.1-49.7 (ident "to_str"))
				(capture @51.19-51.20 (ident "g"))
				(capture @50.15-50.16 (ident "r"))
				(capture @51.16-51.17 (ident "r"))
				(capture @51.22-51.23 (ident "b"))
				(capture @51.25-51.26 (ident "a"))
				(capture @53.15-53.20 (ident "inner")))
			(e-lambda @49.10-54.2
				(args
					(p-assign @49.11-49.16 (ident "color")))
				(e-match @49.18-54.2
					(match @49.18-54.2
						(cond
							(e-lookup-local @49.24-49.29
								(p-assign @49.11-49.16 (ident "color"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal @50.5-50.23
											(p-applied-tag @50.5-50.23))))
								(value
									(e-string @50.27-50.86
										(e-literal @50.28-50.32 (string "rgb("))
										(e-call @50.34-50.47
											(e-lookup-local @50.34-50.44
												(p-assign @49.1-49.7 (ident "to_str")))
											(e-lookup-local @50.45-50.46
												(p-assign @50.15-50.16 (ident "r"))))
										(e-literal @50.48-50.50 (string ", "))
										(e-call @50.52-50.65
											(e-lookup-local @50.52-50.62
												(p-assign @49.1-49.7 (ident "to_str")))
											(e-lookup-local @50.63-50.64
												(p-assign @50.18-50.19 (ident "g"))))
										(e-literal @50.66-50.68 (string ", "))
										(e-call @50.70-50.83
											(e-lookup-local @50.70-50.80
												(p-assign @49.1-49.7 (ident "to_str")))
											(e-lookup-local @50.81-50.82
												(p-assign @50.21-50.22 (ident "b"))))
										(e-literal @50.84-50.85 (string ")")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal @51.5-51.27
											(p-applied-tag @51.5-51.27))))
								(value
									(e-string @51.31-51.109
										(e-literal @51.32-51.37 (string "rgba("))
										(e-call @51.39-51.52
											(e-lookup-local @51.39-51.49
												(p-assign @49.1-49.7 (ident "to_str")))
											(e-lookup-local @51.50-51.51
												(p-assign @51.16-51.17 (ident "r"))))
										(e-literal @51.53-51.55 (string ", "))
										(e-call @51.57-51.70
											(e-lookup-local @51.57-51.67
												(p-assign @49.1-49.7 (ident "to_str")))
											(e-lookup-local @51.68-51.69
												(p-assign @51.19-51.20 (ident "g"))))
										(e-literal @51.71-51.73 (string ", "))
										(e-call @51.75-51.88
											(e-lookup-local @51.75-51.85
												(p-assign @49.1-49.7 (ident "to_str")))
											(e-lookup-local @51.86-51.87
												(p-assign @51.22-51.23 (ident "b"))))
										(e-literal @51.89-51.91 (string ", "))
										(e-call @51.93-51.106
											(e-lookup-local @51.93-51.103
												(p-assign @49.1-49.7 (ident "to_str")))
											(e-lookup-local @51.104-51.105
												(p-assign @51.25-51.26 (ident "a"))))
										(e-literal @51.107-51.108 (string ")")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal @52.5-52.23
											(p-applied-tag @52.5-52.23))))
								(value
									(e-lookup-local @52.27-52.32
										(p-assign @52.17-52.22 (ident "inner")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal @53.5-53.21
											(p-applied-tag @53.5-53.21))))
								(value
									(e-lookup-local @53.25-53.30
										(p-assign @53.15-53.20 (ident "inner"))))))))))
		(annotation @49.1-49.7
			(declared-type
				(ty-fn @48.10-48.22 (effectful false)
					(ty @48.10-48.15 (name "Color"))
					(ty @48.19-48.22 (name "Str"))))))
	(d-let
		(p-assign @61.1-61.6 (ident "named"))
		(e-lambda @61.9-65.50
			(args
				(p-assign @61.10-61.13 (ident "str")))
			(e-if @62.5-65.50
				(if-branches
					(if-branch
						(e-dot-access @62.8-62.28 (field "is_named_color")
							(receiver
								(e-lookup-local @62.8-62.11
									(p-assign @61.10-61.13 (ident "str"))))
							(args))
						(e-nominal @63.9-63.29 (nominal "Result")
							(e-tag @63.9-63.29 (name "Ok")
								(args
									(e-nominal @63.12-63.28 (nominal "Color")
										(e-tag @63.12-63.28 (name "Named")
											(args
												(e-lookup-local @63.24-63.27
													(p-assign @61.10-61.13 (ident "str")))))))))))
				(if-else
					(e-nominal @65.9-65.50 (nominal "Result")
						(e-tag @65.9-65.50 (name "Err")
							(args
								(e-tag @65.13-65.49 (name "UnknownColor")
									(args
										(e-string @65.26-65.48
											(e-literal @65.27-65.41 (string "Unknown color "))
											(e-lookup-local @65.43-65.46
												(p-assign @61.10-61.13 (ident "str")))
											(e-literal @65.47-65.47 (string "")))))))))))
		(annotation @61.1-61.6
			(declared-type
				(ty-fn @60.9-60.50 (effectful false)
					(ty @60.9-60.12 (name "Str"))
					(ty-apply @60.16-60.50 (symbol "Result")
						(ty @60.23-60.28 (name "Color"))
						(ty-tag-union @60.30-60.49
							(ty-apply @60.31-60.48 (symbol "UnknownColor")
								(ty @60.44-60.47 (name "Str")))))))))
	(d-let
		(p-assign @67.1-67.15 (ident "is_named_color"))
		(e-lambda @67.18-71.2
			(args
				(p-assign @67.19-67.22 (ident "str")))
			(e-block @67.23-71.2
				(s-let @68.5-68.66
					(p-assign @68.5-68.11 (ident "colors"))
					(e-call @68.14-68.66
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-list @68.28-68.65
							(elems
								(e-string @68.29-68.40
									(e-literal @68.30-68.39 (string "AliceBlue")))
								(e-string @68.42-68.56
									(e-literal @68.43-68.55 (string "AntiqueWhite")))
								(e-string @68.58-68.64
									(e-literal @68.59-68.63 (string "Aqua")))))))
				(e-dot-access @70.5-70.25 (field "contains")
					(receiver
						(e-lookup-local @70.5-70.11
							(p-assign @68.5-68.11 (ident "colors"))))
					(args
						(e-lookup-local @70.21-70.24
							(p-assign @67.19-67.22 (ident "str"))))))))
	(s-nominal-decl @10.1-15.2
		(ty-header @10.1-10.6 (name "Color"))
		(ty-tag-union @10.10-15.2
			(ty-apply @11.5-11.20 (symbol "RGB")
				(ty @11.9-11.11 (name "U8"))
				(ty @11.13-11.15 (name "U8"))
				(ty @11.17-11.19 (name "U8")))
			(ty-apply @12.5-12.26 (symbol "RGBA")
				(ty @12.10-12.12 (name "U8"))
				(ty @12.14-12.16 (name "U8"))
				(ty @12.18-12.20 (name "U8"))
				(ty @12.22-12.25 (name "Dec")))
			(ty-apply @13.5-13.15 (symbol "Named")
				(ty @13.11-13.14 (name "Str")))
			(ty-apply @14.5-14.13 (symbol "Hex")
				(ty @14.9-14.12 (name "Str")))))
	(s-expect @56.1-56.57
		(e-binop @56.8-56.57 (op "eq")
			(e-dot-access @56.8-56.34 (field "to_str")
				(receiver
					(e-call @56.8-56.25
						(e-lookup-local @56.8-56.11
							(p-assign @18.1-18.4 (ident "rgb")))
						(e-int @56.12-56.15 (value "124"))
						(e-int @56.17-56.19 (value "56"))
						(e-int @56.21-56.24 (value "245"))))
				(args))
			(e-string @56.38-56.57
				(e-literal @56.39-56.56 (string "rgb(124, 56, 245)")))))
	(s-expect @57.1-57.69
		(e-binop @57.8-57.69 (op "eq")
			(e-dot-access @57.8-57.40 (field "to_str")
				(receiver
					(e-call @57.8-57.31
						(e-lookup-local @57.8-57.12
							(p-assign @21.1-21.5 (ident "rgba")))
						(e-int @57.13-57.16 (value "124"))
						(e-int @57.18-57.20 (value "56"))
						(e-int @57.22-57.25 (value "245"))
						(e-int @57.27-57.30 (value "255"))))
				(args))
			(e-string @57.44-57.69
				(e-literal @57.45-57.68 (string "rgba(124, 56, 245, 1.0)")))))
	(s-expect @58.1-58.54
		(e-binop @58.8-58.54 (op "eq")
			(e-dot-access @58.8-58.37 (field "map_ok")
				(receiver
					(e-call @58.8-58.22
						(e-lookup-local @58.8-58.11
							(p-assign @27.1-27.4 (ident "hex")))
						(e-string @58.12-58.21
							(e-literal @58.13-58.20 (string "#ff00ff")))))
				(args
					(e-lookup-local @58.30-58.36
						(p-assign @49.1-49.7 (ident "to_str")))))
			(e-nominal @58.41-58.54 (nominal "Result")
				(e-tag @58.41-58.54 (name "Ok")
					(args
						(e-string @58.44-58.53
							(e-literal @58.45-58.52 (string "#ff00ff")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @18.1-18.4 (type "U8, U8, U8 -> Error"))
		(patt @21.1-21.5 (type "U8, U8, U8, U8 -> Error"))
		(patt @27.1-27.4 (type "Error"))
		(patt @49.1-49.7 (type "Error -> Str"))
		(patt @61.1-61.6 (type "Str -> Result(Error, [UnknownColor(Str)])"))
		(patt @67.1-67.15 (type "_arg -> _ret")))
	(type_decls
		(nominal @10.1-15.2 (type "Error")
			(ty-header @10.1-10.6 (name "Color"))))
	(expressions
		(expr @18.7-18.35 (type "U8, U8, U8 -> Error"))
		(expr @21.8-24.2 (type "U8, U8, U8, U8 -> Error"))
		(expr @27.7-46.2 (type "Error"))
		(expr @49.10-54.2 (type "Error -> Str"))
		(expr @61.9-65.50 (type "Str -> Result(Error, [UnknownColor(Str)])"))
		(expr @67.18-71.2 (type "_arg -> _ret"))))
~~~
