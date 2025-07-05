# META
~~~ini
description=Malformed record syntax using equals instead of colon (error case)
type=expr
~~~
# SOURCE
~~~roc
{ age: 42, name = "Alice" }
~~~
# EXPECTED
PARSE ERROR - error_malformed_syntax_2.md:1:17:1:20
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**error_malformed_syntax_2.md:1:17:1:20:**
```roc
{ age: 42, name = "Alice" }
```
                ^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:6),OpColon(1:6-1:7),Int(1:8-1:10),Comma(1:10-1:11),LowerIdent(1:12-1:16),OpAssign(1:17-1:18),StringStart(1:19-1:20),StringPart(1:20-1:25),StringEnd(1:25-1:26),CloseCurly(1:27-1:28),EndOfFile(1:28-1:28),
~~~
# PARSE
~~~clojure
(e-malformed @1.17-1.20 (reason "expected_expr_close_curly_or_comma"))
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
