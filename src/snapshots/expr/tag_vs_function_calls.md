# META
~~~ini
description=Tag applications vs function calls
type=expr
~~~
# SOURCE
~~~roc
{
    someTag: Some(42),
    noneTag: None,
    okTag: Ok("hello"),
    errTag: Err("oops"),
    addOne: \x -> x + 1,
    result: addOne(5),
    nested: Some(Ok(Just(42))),
    tagList: [Some(1), Some(2), None, Some(3)],
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - tag_vs_function_calls.md:6:13:6:15
expected_expr_close_curly_or_comma - tag_vs_function_calls.md:6:14:6:18
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\x** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**tag_vs_function_calls.md:6:13:6:15:**
```roc
    addOne: \x -> x + 1,
```
            ^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**tag_vs_function_calls.md:6:14:6:18:**
```roc
    addOne: \x -> x + 1,
```
             ^^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:5-2:12),OpColon(2:12-2:13),UpperIdent(2:14-2:18),NoSpaceOpenRound(2:18-2:19),Int(2:19-2:21),CloseRound(2:21-2:22),Comma(2:22-2:23),Newline(1:1-1:1),
LowerIdent(3:5-3:12),OpColon(3:12-3:13),UpperIdent(3:14-3:18),Comma(3:18-3:19),Newline(1:1-1:1),
LowerIdent(4:5-4:10),OpColon(4:10-4:11),UpperIdent(4:12-4:14),NoSpaceOpenRound(4:14-4:15),StringStart(4:15-4:16),StringPart(4:16-4:21),StringEnd(4:21-4:22),CloseRound(4:22-4:23),Comma(4:23-4:24),Newline(1:1-1:1),
LowerIdent(5:5-5:11),OpColon(5:11-5:12),UpperIdent(5:13-5:16),NoSpaceOpenRound(5:16-5:17),StringStart(5:17-5:18),StringPart(5:18-5:22),StringEnd(5:22-5:23),CloseRound(5:23-5:24),Comma(5:24-5:25),Newline(1:1-1:1),
LowerIdent(6:5-6:11),OpColon(6:11-6:12),OpBackslash(6:13-6:14),LowerIdent(6:14-6:15),OpArrow(6:16-6:18),LowerIdent(6:19-6:20),OpPlus(6:21-6:22),Int(6:23-6:24),Comma(6:24-6:25),Newline(1:1-1:1),
LowerIdent(7:5-7:11),OpColon(7:11-7:12),LowerIdent(7:13-7:19),NoSpaceOpenRound(7:19-7:20),Int(7:20-7:21),CloseRound(7:21-7:22),Comma(7:22-7:23),Newline(1:1-1:1),
LowerIdent(8:5-8:11),OpColon(8:11-8:12),UpperIdent(8:13-8:17),NoSpaceOpenRound(8:17-8:18),UpperIdent(8:18-8:20),NoSpaceOpenRound(8:20-8:21),UpperIdent(8:21-8:25),NoSpaceOpenRound(8:25-8:26),Int(8:26-8:28),CloseRound(8:28-8:29),CloseRound(8:29-8:30),CloseRound(8:30-8:31),Comma(8:31-8:32),Newline(1:1-1:1),
LowerIdent(9:5-9:12),OpColon(9:12-9:13),OpenSquare(9:14-9:15),UpperIdent(9:15-9:19),NoSpaceOpenRound(9:19-9:20),Int(9:20-9:21),CloseRound(9:21-9:22),Comma(9:22-9:23),UpperIdent(9:24-9:28),NoSpaceOpenRound(9:28-9:29),Int(9:29-9:30),CloseRound(9:30-9:31),Comma(9:31-9:32),UpperIdent(9:33-9:37),Comma(9:37-9:38),UpperIdent(9:39-9:43),NoSpaceOpenRound(9:43-9:44),Int(9:44-9:45),CloseRound(9:45-9:46),CloseSquare(9:46-9:47),Comma(9:47-9:48),Newline(1:1-1:1),
CloseCurly(10:1-10:2),EndOfFile(10:2-10:2),
~~~
# PARSE
~~~clojure
(e-malformed @6.14-6.18 (reason "expected_expr_close_curly_or_comma"))
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
