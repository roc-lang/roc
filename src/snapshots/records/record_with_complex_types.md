# META
~~~ini
description=Record construction with complex field types including lists and tag unions
type=expr
~~~
# SOURCE
~~~roc
{
    name: "Alice",
    scores: [95, 87, 92, 78],
    status: Active({ since: "2023-01-15" }),
    preferences: { theme: Dark, notifications: Email("alice@example.com") },
    metadata: Ok {
        tags: ["developer", "senior", "fullstack"],
        permissions: [Read, Write, Admin],
    },
    callback: |x| x + 1,
    nested: {
        items: [Some("first"), None, Some("third")],
        result: Success({ data: [1, 2, 3], timestamp: "2024-01-01" })
    }
}
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**record_with_complex_types.md:6:18:6:18:**
```roc
    metadata: Ok {
```


# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:5-2:9),OpColon(2:9-2:10),StringStart(2:11-2:12),StringPart(2:12-2:17),StringEnd(2:17-2:18),Comma(2:18-2:19),Newline(1:1-1:1),
LowerIdent(3:5-3:11),OpColon(3:11-3:12),OpenSquare(3:13-3:14),Int(3:14-3:16),Comma(3:16-3:17),Int(3:18-3:20),Comma(3:20-3:21),Int(3:22-3:24),Comma(3:24-3:25),Int(3:26-3:28),CloseSquare(3:28-3:29),Comma(3:29-3:30),Newline(1:1-1:1),
LowerIdent(4:5-4:11),OpColon(4:11-4:12),UpperIdent(4:13-4:19),NoSpaceOpenRound(4:19-4:20),OpenCurly(4:20-4:21),LowerIdent(4:22-4:27),OpColon(4:27-4:28),StringStart(4:29-4:30),StringPart(4:30-4:40),StringEnd(4:40-4:41),CloseCurly(4:42-4:43),CloseRound(4:43-4:44),Comma(4:44-4:45),Newline(1:1-1:1),
LowerIdent(5:5-5:16),OpColon(5:16-5:17),OpenCurly(5:18-5:19),LowerIdent(5:20-5:25),OpColon(5:25-5:26),UpperIdent(5:27-5:31),Comma(5:31-5:32),LowerIdent(5:33-5:46),OpColon(5:46-5:47),UpperIdent(5:48-5:53),NoSpaceOpenRound(5:53-5:54),StringStart(5:54-5:55),StringPart(5:55-5:72),StringEnd(5:72-5:73),CloseRound(5:73-5:74),CloseCurly(5:75-5:76),Comma(5:76-5:77),Newline(1:1-1:1),
LowerIdent(6:5-6:13),OpColon(6:13-6:14),UpperIdent(6:15-6:17),OpenCurly(6:18-6:19),Newline(1:1-1:1),
LowerIdent(7:9-7:13),OpColon(7:13-7:14),OpenSquare(7:15-7:16),StringStart(7:16-7:17),StringPart(7:17-7:26),StringEnd(7:26-7:27),Comma(7:27-7:28),StringStart(7:29-7:30),StringPart(7:30-7:36),StringEnd(7:36-7:37),Comma(7:37-7:38),StringStart(7:39-7:40),StringPart(7:40-7:49),StringEnd(7:49-7:50),CloseSquare(7:50-7:51),Comma(7:51-7:52),Newline(1:1-1:1),
LowerIdent(8:9-8:20),OpColon(8:20-8:21),OpenSquare(8:22-8:23),UpperIdent(8:23-8:27),Comma(8:27-8:28),UpperIdent(8:29-8:34),Comma(8:34-8:35),UpperIdent(8:36-8:41),CloseSquare(8:41-8:42),Comma(8:42-8:43),Newline(1:1-1:1),
CloseCurly(9:5-9:6),Comma(9:6-9:7),Newline(1:1-1:1),
LowerIdent(10:5-10:13),OpColon(10:13-10:14),OpBar(10:15-10:16),LowerIdent(10:16-10:17),OpBar(10:17-10:18),LowerIdent(10:19-10:20),OpPlus(10:21-10:22),Int(10:23-10:24),Comma(10:24-10:25),Newline(1:1-1:1),
LowerIdent(11:5-11:11),OpColon(11:11-11:12),OpenCurly(11:13-11:14),Newline(1:1-1:1),
LowerIdent(12:9-12:14),OpColon(12:14-12:15),OpenSquare(12:16-12:17),UpperIdent(12:17-12:21),NoSpaceOpenRound(12:21-12:22),StringStart(12:22-12:23),StringPart(12:23-12:28),StringEnd(12:28-12:29),CloseRound(12:29-12:30),Comma(12:30-12:31),UpperIdent(12:32-12:36),Comma(12:36-12:37),UpperIdent(12:38-12:42),NoSpaceOpenRound(12:42-12:43),StringStart(12:43-12:44),StringPart(12:44-12:49),StringEnd(12:49-12:50),CloseRound(12:50-12:51),CloseSquare(12:51-12:52),Comma(12:52-12:53),Newline(1:1-1:1),
LowerIdent(13:9-13:15),OpColon(13:15-13:16),UpperIdent(13:17-13:24),NoSpaceOpenRound(13:24-13:25),OpenCurly(13:25-13:26),LowerIdent(13:27-13:31),OpColon(13:31-13:32),OpenSquare(13:33-13:34),Int(13:34-13:35),Comma(13:35-13:36),Int(13:37-13:38),Comma(13:38-13:39),Int(13:40-13:41),CloseSquare(13:41-13:42),Comma(13:42-13:43),LowerIdent(13:44-13:53),OpColon(13:53-13:54),StringStart(13:55-13:56),StringPart(13:56-13:66),StringEnd(13:66-13:67),CloseCurly(13:68-13:69),CloseRound(13:69-13:70),Newline(1:1-1:1),
CloseCurly(14:5-14:6),Newline(1:1-1:1),
CloseCurly(15:1-15:2),EndOfFile(15:2-15:2),
~~~
# PARSE
~~~clojure
(e-malformed @1-1-1-1 (reason "expected_expr_close_curly_or_comma"))
~~~
# FORMATTED
~~~roc

~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~