# META
~~~ini
description=two strings
type=file
~~~
# SOURCE
~~~roc
module []

"one"

"two"
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - string.md:6:1:6:3
UNEXPECTED TOKEN IN EXPRESSION - string.md:6:2:6:4
UNEXPECTED TOKEN IN EXPRESSION - string.md:6:3:6:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**string.md:6:1:6:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**string.md:6:2:6:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**string.md:6:3:6:4:**
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

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
StringStart(3:1-3:2),StringPart(3:2-3:5),StringEnd(3:5-3:6),Newline(1:1-1:1),
Newline(1:1-1:1),
StringStart(5:1-5:2),StringPart(5:2-5:5),StringEnd(5:5-5:6),Newline(1:1-1:1),
MalformedUnknownToken(6:1-6:2),MalformedUnknownToken(6:2-6:3),MalformedUnknownToken(6:3-6:4),EndOfFile(6:4-6:4),
~~~
# PARSE
~~~clojure
(file @1.1-6.4
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(e-string @3.1-3.6
			(e-string-part @3.2-3.5 (raw "one")))
		(e-string @5.1-5.6
			(e-string-part @5.2-5.5 (raw "two")))
		(e-malformed @6.1-6.3 (reason "expr_unexpected_token"))
		(e-malformed @6.2-6.4 (reason "expr_unexpected_token"))
		(e-malformed @6.3-6.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module []

"one"

"two"

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
