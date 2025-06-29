# META
~~~ini
description=If expression with conditional
type=expr
~~~
# SOURCE
~~~roc
if x > 5 then "big" else "small"
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**if_expression.md:1:15:1:19:**
```roc
if x > 5 then "big" else "small"
```
              ^^^^


# TOKENS
~~~zig
KwIf(1:1-1:3),LowerIdent(1:4-1:5),OpGreaterThan(1:6-1:7),Int(1:8-1:9),LowerIdent(1:10-1:14),StringStart(1:15-1:16),StringPart(1:16-1:19),StringEnd(1:19-1:20),KwElse(1:21-1:25),StringStart(1:26-1:27),StringPart(1:27-1:32),StringEnd(1:32-1:33),EndOfFile(1:33-1:33),
~~~
# PARSE
~~~clojure
(e-malformed @1.15-1.19 (reason "no_else"))
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
