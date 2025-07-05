# META
~~~ini
description=An empty module with singleline exposes
type=file
~~~
# SOURCE
~~~roc
module [something, SomeType]
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - module_nonempty_single.md:2:1:2:3
UNEXPECTED TOKEN IN EXPRESSION - module_nonempty_single.md:2:2:2:4
UNEXPECTED TOKEN IN EXPRESSION - module_nonempty_single.md:2:3:2:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**module_nonempty_single.md:2:1:2:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**module_nonempty_single.md:2:2:2:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**module_nonempty_single.md:2:3:2:4:**
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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:18),Comma(1:18-1:19),UpperIdent(1:20-1:28),CloseSquare(1:28-1:29),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(file @1.1-2.4
	(module @1.1-1.29
		(exposes @1.8-1.29
			(exposed-lower-ident (text "something"))
			(exposed-upper-ident (text "SomeType"))))
	(statements
		(e-malformed @2.1-2.3 (reason "expr_unexpected_token"))
		(e-malformed @2.2-2.4 (reason "expr_unexpected_token"))
		(e-malformed @2.3-2.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [something, SomeType]

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
