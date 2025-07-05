# META
~~~ini
description=package_header_nonempty_multiline (4)
type=file
~~~
# SOURCE
~~~roc
package
	[
		something,
		SomeType,
	]
	{
		somePkg: "../main.roc",
	}
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - package_header_nonempty_multiline_4.md:9:2:9:4
UNEXPECTED TOKEN IN EXPRESSION - package_header_nonempty_multiline_4.md:9:3:9:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**package_header_nonempty_multiline_4.md:9:2:9:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**package_header_nonempty_multiline_4.md:9:3:9:4:**
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

# TOKENS
~~~zig
KwPackage(1:1-1:8),Newline(1:1-1:1),
OpenSquare(2:2-2:3),Newline(1:1-1:1),
LowerIdent(3:3-3:12),Comma(3:12-3:13),Newline(1:1-1:1),
UpperIdent(4:3-4:11),Comma(4:11-4:12),Newline(1:1-1:1),
CloseSquare(5:2-5:3),Newline(1:1-1:1),
OpenCurly(6:2-6:3),Newline(1:1-1:1),
LowerIdent(7:3-7:10),OpColon(7:10-7:11),StringStart(7:12-7:13),StringPart(7:13-7:24),StringEnd(7:24-7:25),Comma(7:25-7:26),Newline(1:1-1:1),
CloseCurly(8:2-8:3),Newline(1:1-1:1),
MalformedUnknownToken(9:1-9:2),MalformedUnknownToken(9:2-9:3),MalformedUnknownToken(9:3-9:4),EndOfFile(9:4-9:4),
~~~
# PARSE
~~~clojure
(file @1.1-9.4
	(package @1.1-8.3
		(exposes @2.2-5.3
			(exposed-lower-ident (text "something"))
			(exposed-upper-ident (text "SomeType")))
		(packages @6.2-8.3
			(record-field @7.3-7.26 (name "somePkg")
				(e-string @7.12-7.25
					(e-string-part @7.13-7.24 (raw "../main.roc"))))))
	(statements
		(e-malformed @9.2-9.4 (reason "expr_unexpected_token"))
		(e-malformed @9.3-9.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
package
	[
		something,
		SomeType,
	]
	{
		somePkg: "../main.roc",
	}
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
