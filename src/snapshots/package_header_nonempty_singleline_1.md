# META
~~~ini
description=package_header_nonempty_singleline (1)
type=file
~~~
# SOURCE
~~~roc
package [something, SomeType] { somePkg: "../main.roc", other: "../../other/main.roc" }
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - package_header_nonempty_singleline_1.md:2:2:2:4
UNEXPECTED TOKEN IN EXPRESSION - package_header_nonempty_singleline_1.md:2:3:2:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**package_header_nonempty_singleline_1.md:2:2:2:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**package_header_nonempty_singleline_1.md:2:3:2:4:**
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
KwPackage(1:1-1:8),OpenSquare(1:9-1:10),LowerIdent(1:10-1:19),Comma(1:19-1:20),UpperIdent(1:21-1:29),CloseSquare(1:29-1:30),OpenCurly(1:31-1:32),LowerIdent(1:33-1:40),OpColon(1:40-1:41),StringStart(1:42-1:43),StringPart(1:43-1:54),StringEnd(1:54-1:55),Comma(1:55-1:56),LowerIdent(1:57-1:62),OpColon(1:62-1:63),StringStart(1:64-1:65),StringPart(1:65-1:85),StringEnd(1:85-1:86),CloseCurly(1:87-1:88),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(file @1.1-2.4
	(package @1.1-1.88
		(exposes @1.9-1.30
			(exposed-lower-ident (text "something"))
			(exposed-upper-ident (text "SomeType")))
		(packages @1.31-1.88
			(record-field @1.33-1.56 (name "somePkg")
				(e-string @1.42-1.55
					(e-string-part @1.43-1.54 (raw "../main.roc"))))
			(record-field @1.57-1.88 (name "other")
				(e-string @1.64-1.86
					(e-string-part @1.65-1.85 (raw "../../other/main.roc"))))))
	(statements
		(e-malformed @2.2-2.4 (reason "expr_unexpected_token"))
		(e-malformed @2.3-2.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
package [something, SomeType] { somePkg: "../main.roc", other: "../../other/main.roc" }
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
