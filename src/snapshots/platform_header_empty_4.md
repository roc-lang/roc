# META
~~~ini
description=platform_header_empty (4)
type=file
~~~
# SOURCE
~~~roc
platform "foo" requires {} {} exposes [] packages {} provides []
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - platform_header_empty_4.md:2:1:2:3
UNEXPECTED TOKEN IN EXPRESSION - platform_header_empty_4.md:2:2:2:4
UNEXPECTED TOKEN IN EXPRESSION - platform_header_empty_4.md:2:3:2:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**platform_header_empty_4.md:2:1:2:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**platform_header_empty_4.md:2:2:2:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**platform_header_empty_4.md:2:3:2:4:**
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
KwPlatform(1:1-1:9),StringStart(1:10-1:11),StringPart(1:11-1:14),StringEnd(1:14-1:15),KwRequires(1:16-1:24),OpenCurly(1:25-1:26),CloseCurly(1:26-1:27),OpenCurly(1:28-1:29),CloseCurly(1:29-1:30),KwExposes(1:31-1:38),OpenSquare(1:39-1:40),CloseSquare(1:40-1:41),KwPackages(1:42-1:50),OpenCurly(1:51-1:52),CloseCurly(1:52-1:53),KwProvides(1:54-1:62),OpenSquare(1:63-1:64),CloseSquare(1:64-1:65),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(file @1.1-2.4
	(platform @1.1-1.65 (name "foo")
		(rigids @1.25-1.27)
		(ty-record @1.28-1.30)
		(exposes @1.39-1.41)
		(packages @1.51-1.53)
		(provides @1.63-1.65))
	(statements
		(e-malformed @2.1-2.3 (reason "expr_unexpected_token"))
		(e-malformed @2.2-2.4 (reason "expr_unexpected_token"))
		(e-malformed @2.3-2.4 (reason "expr_unexpected_token"))))
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
