# META
~~~ini
description=arg_pattern_as
type=expr
~~~
# SOURCE
~~~roc
\({ x, y } as point), (@Location inner as outer) ->
    crash ""
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**arg_pattern_as.md:1:1:1:3:**
```roc
\({ x, y } as point), (@Location inner as outer) ->
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),NoSpaceOpenRound(1:2-1:3),OpenCurly(1:3-1:4),LowerIdent(1:5-1:6),Comma(1:6-1:7),LowerIdent(1:8-1:9),CloseCurly(1:10-1:11),KwAs(1:12-1:14),LowerIdent(1:15-1:20),CloseRound(1:20-1:21),Comma(1:21-1:22),OpenRound(1:23-1:24),OpaqueName(1:24-1:33),LowerIdent(1:34-1:39),KwAs(1:40-1:42),LowerIdent(1:43-1:48),CloseRound(1:48-1:49),OpArrow(1:50-1:52),Newline(1:1-1:1),
KwCrash(2:5-2:10),StringStart(2:11-2:12),StringPart(2:12-2:12),StringEnd(2:12-2:13),EndOfFile(2:13-2:13),
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
