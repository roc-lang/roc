# META
~~~ini
description=platform_header_empty (5)
type=file
~~~
# SOURCE
~~~roc
platform "foo"
	requires {} {}
	exposes []
	packages {}
	provides []
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - platform_header_empty_5.md:6:1:6:3
UNEXPECTED TOKEN IN EXPRESSION - platform_header_empty_5.md:6:2:6:4
UNEXPECTED TOKEN IN EXPRESSION - platform_header_empty_5.md:6:3:6:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**platform_header_empty_5.md:6:1:6:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**platform_header_empty_5.md:6:2:6:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**platform_header_empty_5.md:6:3:6:4:**
```roc
~~~
```
  ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwPlatform(1:1-1:9),StringStart(1:10-1:11),StringPart(1:11-1:14),StringEnd(1:14-1:15),Newline(1:1-1:1),
KwRequires(2:2-2:10),OpenCurly(2:11-2:12),CloseCurly(2:12-2:13),OpenCurly(2:14-2:15),CloseCurly(2:15-2:16),Newline(1:1-1:1),
KwExposes(3:2-3:9),OpenSquare(3:10-3:11),CloseSquare(3:11-3:12),Newline(1:1-1:1),
KwPackages(4:2-4:10),OpenCurly(4:11-4:12),CloseCurly(4:12-4:13),Newline(1:1-1:1),
KwProvides(5:2-5:10),OpenSquare(5:11-5:12),CloseSquare(5:12-5:13),Newline(1:1-1:1),
MalformedUnknownToken(6:1-6:2),MalformedUnknownToken(6:2-6:3),MalformedUnknownToken(6:3-6:4),EndOfFile(6:4-6:4),
~~~
# PARSE
~~~clojure
(file @1.1-6.4
	(platform @1.1-5.13 (name "foo")
		(rigids @2.11-2.13)
		(ty-record @2.14-2.16)
		(exposes @3.10-3.12)
		(packages @4.11-4.13)
		(provides @5.11-5.13))
	(statements
		(e-malformed @6.1-6.3 (reason "expr_unexpected_token"))
		(e-malformed @6.2-6.4 (reason "expr_unexpected_token"))
		(e-malformed @6.3-6.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
platform "foo"
	requires {} {}
	exposes []
	packages {}
	provides []

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
