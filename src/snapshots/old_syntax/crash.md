# META
~~~ini
description=crash
type=expr
~~~
# SOURCE
~~~roc
_ = crash ""
_ = crash "" ""
_ = crash 15 123
_ = try foo (\_ -> crash "")
_ =
  _ = crash ""
  crash

{ f: crash "" }
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - crash.md:1:1:1:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **_ =** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**crash.md:1:1:1:4:**
```roc
_ = crash ""
```
^^^


# TOKENS
~~~zig
Underscore(1:1-1:2),OpAssign(1:3-1:4),KwCrash(1:5-1:10),StringStart(1:11-1:12),StringPart(1:12-1:12),StringEnd(1:12-1:13),Newline(1:1-1:1),
Underscore(2:1-2:2),OpAssign(2:3-2:4),KwCrash(2:5-2:10),StringStart(2:11-2:12),StringPart(2:12-2:12),StringEnd(2:12-2:13),StringStart(2:14-2:15),StringPart(2:15-2:15),StringEnd(2:15-2:16),Newline(1:1-1:1),
Underscore(3:1-3:2),OpAssign(3:3-3:4),KwCrash(3:5-3:10),Int(3:11-3:13),Int(3:14-3:17),Newline(1:1-1:1),
Underscore(4:1-4:2),OpAssign(4:3-4:4),LowerIdent(4:5-4:8),LowerIdent(4:9-4:12),OpenRound(4:13-4:14),OpBackslash(4:14-4:15),Underscore(4:15-4:16),OpArrow(4:17-4:19),KwCrash(4:20-4:25),StringStart(4:26-4:27),StringPart(4:27-4:27),StringEnd(4:27-4:28),CloseRound(4:28-4:29),Newline(1:1-1:1),
Underscore(5:1-5:2),OpAssign(5:3-5:4),Newline(1:1-1:1),
Underscore(6:3-6:4),OpAssign(6:5-6:6),KwCrash(6:7-6:12),StringStart(6:13-6:14),StringPart(6:14-6:14),StringEnd(6:14-6:15),Newline(1:1-1:1),
KwCrash(7:3-7:8),Newline(1:1-1:1),
Newline(1:1-1:1),
OpenCurly(9:1-9:2),LowerIdent(9:3-9:4),OpColon(9:4-9:5),KwCrash(9:6-9:11),StringStart(9:12-9:13),StringPart(9:13-9:13),StringEnd(9:13-9:14),CloseCurly(9:15-9:16),Newline(1:1-1:1),
MalformedUnknownToken(10:1-10:2),MalformedUnknownToken(10:2-10:3),MalformedUnknownToken(10:3-10:4),EndOfFile(10:4-10:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.4 (reason "expr_unexpected_token"))
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
