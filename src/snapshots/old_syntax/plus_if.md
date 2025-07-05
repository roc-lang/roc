# META
~~~ini
description=plus_if
type=expr
~~~
# SOURCE
~~~roc
1 * if Bool.true then 1 else 1
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**plus_if.md:1:23:1:29:**
```roc
1 * if Bool.true then 1 else 1
```
                      ^^^^^^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

# TOKENS
~~~zig
Int(1:1-1:2),OpStar(1:3-1:4),KwIf(1:5-1:7),UpperIdent(1:8-1:12),NoSpaceDotLowerIdent(1:12-1:17),LowerIdent(1:18-1:22),Int(1:23-1:24),KwElse(1:25-1:29),Int(1:30-1:31),EndOfFile(1:31-1:31),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.29 (op "*")
	(e-int @1.1-1.2 (raw "1"))
	(e-malformed @1.23-1.29 (reason "no_else")))
~~~
# FORMATTED
~~~roc
1 * 
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.29 (op "mul")
	(e-int @1.1-1.2 (value "1"))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.29 (type "*"))
~~~
