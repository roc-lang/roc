# META
~~~ini
description=record_with_if
type=expr
~~~
# SOURCE
~~~roc
{x : if Bool.true then 1 else 2, y: 3 }
~~~
~~~
# EXPECTED
PARSE ERROR - record_with_if.md:1:24:1:30
PARSE ERROR - record_with_if.md:1:26:1:32
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**record_with_if.md:1:24:1:30:**
```roc
{x : if Bool.true then 1 else 2, y: 3 }
```
                       ^^^^^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**record_with_if.md:1:26:1:32:**
```roc
{x : if Bool.true then 1 else 2, y: 3 }
```
                         ^^^^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:2-1:3),OpColon(1:4-1:5),KwIf(1:6-1:8),UpperIdent(1:9-1:13),NoSpaceDotLowerIdent(1:13-1:18),LowerIdent(1:19-1:23),Int(1:24-1:25),KwElse(1:26-1:30),Int(1:31-1:32),Comma(1:32-1:33),LowerIdent(1:34-1:35),OpColon(1:35-1:36),Int(1:37-1:38),CloseCurly(1:39-1:40),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.26-1.32 (reason "expected_expr_close_curly_or_comma"))
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
